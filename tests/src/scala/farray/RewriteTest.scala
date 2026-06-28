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

end RewriteTest
