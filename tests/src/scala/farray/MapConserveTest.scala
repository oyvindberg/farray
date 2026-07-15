package farray

import org.junit.Test
import org.junit.Assert.*

/** Round-4 Bug 1 acceptance suite: `mapConserve` applies `f` EXACTLY ONCE per element (the dotty compiler
  * passes side-effecting `f`s — symbol creation, position mutation — so a double invocation corrupts state),
  * AND returns the receiver IDENTICALLY (no allocation) when nothing changed (per-element reference `ne`).
  *
  * List.mapConserve semantics: unchanged elements keep their ORIGINALS; the result equals mapping f over all
  * (since f(x) eq x on the unchanged prefix, the original IS f's result). */
class MapConserveTest:

  // ---------- reference elements: identity conserve + change positions ----------

  @Test def ref_no_change_is_identity_and_once: Unit =
    val xs = FArray("a", "b", "c", "d")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; s } // returns the SAME reference every time
    assertEquals(4, calls) // exactly n
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase]) // identity return, no allocation
    assertEquals(List("a", "b", "c", "d"), r.toList)

  @Test def ref_first_change_once: Unit =
    val xs = FArray("a", "b", "c", "d")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; if s == "a" then "A" else s }
    assertEquals(4, calls) // f must NOT run twice on the first changed element
    assertEquals(List("A", "b", "c", "d"), r.toList)
    assertNotSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  @Test def ref_middle_change_once: Unit =
    val xs = FArray("a", "b", "c", "d", "e")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; if s == "c" then "C" else s }
    assertEquals(5, calls)
    assertEquals(List("a", "b", "C", "d", "e"), r.toList) // prefix a,b are the ORIGINALS
    // the unchanged prefix elements are the exact original references
    assertSame(xs(0), r(0))
    assertSame(xs(1), r(1))
    assertSame(xs(4), r(4))

  @Test def ref_last_change_once: Unit =
    val xs = FArray("a", "b", "c", "d")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; if s == "d" then "D" else s }
    assertEquals(4, calls)
    assertEquals(List("a", "b", "c", "D"), r.toList)

  @Test def ref_all_change_once: Unit =
    val xs = FArray("a", "b", "c")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; s.toUpperCase }
    assertEquals(3, calls)
    assertEquals(List("A", "B", "C"), r.toList)

  // ---------- primitive elements: exactly-once (prims can't be conserved, but f still runs n times) ----------

  @Test def int_identity_value_once_content_preserved: Unit =
    val xs = FArray(1, 2, 3, 4, 5)
    var calls = 0
    val r = xs.mapConserve { x => calls += 1; x } // numerically identical
    assertEquals(5, calls) // exactly once per element
    assertEquals(List(1, 2, 3, 4, 5), r.toList)

  @Test def int_change_once: Unit =
    val xs = FArray(1, 2, 3, 4)
    var calls = 0
    val r = xs.mapConserve { x => calls += 1; if x == 2 then 20 else x }
    assertEquals(4, calls)
    assertEquals(List(1, 20, 3, 4), r.toList)

  @Test def long_change_once: Unit =
    val xs = FArray(10L, 20L, 30L)
    var calls = 0
    val r = xs.mapConserve { x => calls += 1; x + 1L }
    assertEquals(3, calls)
    assertEquals(List(11L, 21L, 31L), r.toList)

  // ---------- edge cases: empty / single ----------

  @Test def empty_zero_calls_identity: Unit =
    val xs = FArray.empty[String]
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; s + "!" }
    assertEquals(0, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  @Test def single_ref_unchanged_identity_once: Unit =
    val xs = FArray("only")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; s }
    assertEquals(1, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  @Test def single_ref_changed_once: Unit =
    val xs = FArray("only")
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; "X" }
    assertEquals(1, calls)
    assertEquals(List("X"), r.toList)

  @Test def single_int_once: Unit =
    val xs = FArray(7)
    var calls = 0
    val r = xs.mapConserve { x => calls += 1; x * 2 }
    assertEquals(1, calls)
    assertEquals(List(14), r.toList)

  // ---------- structural node receiver (Concat/Prepend) — exactly-once, prefix originals ----------

  @Test def node_receiver_middle_change_once: Unit =
    val xs = (FArray("a", "b") ++ FArray("c", "d", "e")) // Concat node
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; if s == "d" then "D" else s }
    assertEquals(5, calls)
    assertEquals(List("a", "b", "c", "D", "e"), r.toList)

  @Test def node_receiver_no_change_identity: Unit =
    val xs = (0 +: FArray("x", "y", "z").asInstanceOf[FArray[Any]]) // Prepend node
    var calls = 0
    val r = xs.mapConserve { s => calls += 1; s }
    assertEquals(4, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  // ---------- parity with List.mapConserve semantics (== map content-wise) on identity + change ----------

  @Test def parity_with_list_mapConserve: Unit =
    val base = List("a", "b", "c", "d", "e", "f")
    val fa = base.toFArray
    val f: String => String = s => if s == "c" || s == "e" then s.toUpperCase else s
    // mapConserve's observable content equals map's; identity elements keep originals
    assertEquals(base.map(f), fa.mapConserve(f).toList)
    val id: String => String = s => s
    assertEquals(base, fa.mapConserve(id).toList)
    assertSame(fa.asInstanceOf[FBase], fa.mapConserve(id).asInstanceOf[FBase]) // no-change identity
