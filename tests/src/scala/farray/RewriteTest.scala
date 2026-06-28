package farray

import org.junit.Test
import org.junit.Assert.*

/** Algebraic stage rewrites: adjacent `take(n).take(m)` → `take(min(n,m))` and `drop(n).drop(m)` → `drop(n+m)`,
 *  which collapse redundant per-element counter machinery. Asserts both correctness (vs List) and that the
 *  collapse actually happened (one limit/counter in the generated code, not two). */
class RewriteTest:
  private val xs = FArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  private val ls = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  // ── correctness vs List (the reference) across orderings and chains ───────────────────────────────────
  @Test def takeTake_value(): Unit =
    assertEquals(ls.take(3).take(5), xs.fuse.take(3).take(5).run.toList)
    assertEquals(ls.take(5).take(3), xs.fuse.take(5).take(3).run.toList)
    assertEquals(ls.take(7).take(4).take(2), xs.fuse.take(7).take(4).take(2).run.toList) // 3-chain
    assertEquals(ls.take(0).take(3), xs.fuse.take(0).take(3).run.toList)                  // zero
    assertEquals(ls.take(20).take(4), xs.fuse.take(20).take(4).run.toList)                // over length

  @Test def dropDrop_value(): Unit =
    assertEquals(ls.drop(2).drop(3), xs.fuse.drop(2).drop(3).run.toList)
    assertEquals(ls.drop(3).drop(2).drop(1), xs.fuse.drop(3).drop(2).drop(1).run.toList)  // 3-chain
    assertEquals(ls.drop(0).drop(4), xs.fuse.drop(0).drop(4).run.toList)
    assertEquals(ls.drop(20).drop(2), xs.fuse.drop(20).drop(2).run.toList)                // over length → empty

  /** a stage BETWEEN the takes must prevent the collapse (positions differ): take.filter.take. */
  @Test def takeFilterTake_notCollapsed_butCorrect(): Unit =
    assertEquals(ls.take(8).filter(_ > 3).take(2), xs.fuse.take(8).filter(_ > 3).take(2).run.toList)

  // ── take commutes LEFT past map (length+position preserving) → enables collapse across a map ──────────
  @Test def takeMapTake_value(): Unit =
    assertEquals(ls.take(7).map(_ * 2).take(3), xs.fuse.take(7).map(_ * 2).take(3).run.toList)
    assertEquals(ls.take(2).map(_ * 2).take(5), xs.fuse.take(2).map(_ * 2).take(5).run.toList) // tighter take is first
    assertEquals(ls.map(_ + 1).take(4), xs.fuse.map(_ + 1).take(4).run.toList)
    assertEquals(ls.take(6).map(_ * 3).map(_ - 1).take(2),
      xs.fuse.take(6).map(_ * 3).map(_ - 1).take(2).run.toList) // through two maps

  /** drop must NOT slide past map (would skip f's evaluation on the dropped prefix). Correctness still holds. */
  @Test def dropMapDrop_value(): Unit =
    assertEquals(ls.drop(2).map(_ * 2).drop(3), xs.fuse.drop(2).map(_ * 2).drop(3).run.toList)

  @Test def runtimeArgs(): Unit =
    val n = 4; val m = 2 // non-literal args still collapse via Math.min / +
    assertEquals(ls.take(n).take(m), xs.fuse.take(n).take(m).run.toList)
    assertEquals(ls.drop(n).drop(m), xs.fuse.drop(n).drop(m).run.toList)

  // ── the collapse actually happened (structural) ──────────────────────────────────────────────────────
  /** `take(3).take(5)` must emit ONE clamped limit, not two. (A `take` emits `val lim: Int = {…clamp…}`.) */
  @Test def takeTake_collapses_to_one_limit(): Unit =
    val gen = FuseDebug.show(xs.fuse.take(3).take(5).count)
    val lims = "val lim".r.findAllIn(gen).size
    assertEquals("take(n).take(m) should emit one clamped limit, not two", 1, lims)

  /** `drop(2).drop(3)` must emit ONE drop counter, not two. (drop emits a `var c` skip counter with no limit.) */
  @Test def dropDrop_collapses_to_one_counter(): Unit =
    val gen = FuseDebug.show(xs.fuse.drop(2).drop(3).foreach(_ => ()))
    // a drop's skip counter is `var c: Int = 0`; the foreach terminal adds none. Two drops un-collapsed → two.
    val counters = gen.linesIterator.count(l => l.trim.matches("""var `?c[₀-₉]*`?: Int = 0"""))
    assertEquals("drop(n).drop(m) should emit one skip counter, not two", 1, counters)

  /** `take(7).map(f).take(3)`: the inner take slides left past the map and fuses with the outer → ONE limit. */
  @Test def takeMapTake_collapses_across_map(): Unit =
    val gen = FuseDebug.show(xs.fuse.take(7).map(_ * 2).take(3).count)
    val lims = "val lim".r.findAllIn(gen).size
    assertEquals("take(7).map(f).take(3) should collapse to one limit across the map", 1, lims)

  /** the commute means `map(f).take(n)` evaluates `f` at most n times (it already did via the done-flag, but the
   *  rewrite must not regress that). A call-count proves f runs exactly n times over a long input. */
  @Test def mapTake_evaluatesMapOnlyNtimes(): Unit =
    var calls = 0
    val out = FArray.tabulate(100000)(i => i).fuse.map(x => { calls += 1; x * 2 }).take(3).run.toList
    assertEquals(List(0, 2, 4), out)
    assertEquals("map(f).take(3) must call f exactly 3 times", 3, calls)

end RewriteTest
