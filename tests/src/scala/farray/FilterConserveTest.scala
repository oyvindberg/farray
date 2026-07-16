package farray

import org.junit.Test
import org.junit.Assert.*

/** Round-8 item 2 acceptance suite: `filterConserve` applies `p` EXACTLY ONCE per element in one pass, and returns the receiver IDENTICALLY (no allocation)
  * when NO element is dropped. Mirrors `List.filterConserve` semantics: kept elements keep their ORIGINAL references; result content equals `filter(p)`.
  */
class FilterConserveTest:

  // ---------- reference elements: identity conserve + drops ----------

  @Test def ref_no_drop_is_identity_and_once: Unit =
    val xs = FArray("a", "b", "c", "d")
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; true }
    assertEquals(4, calls) // exactly n
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase]) // identity return, no allocation
    assertEquals(List("a", "b", "c", "d"), r.toList)

  @Test def ref_drop_first_once: Unit =
    val xs = FArray("a", "b", "c", "d")
    var calls = 0
    val r = xs.filterConserve { s => calls += 1; s != "a" }
    assertEquals(4, calls)
    assertEquals(List("b", "c", "d"), r.toList)
    assertNotSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  @Test def ref_drop_middle_once_prefix_originals: Unit =
    val xs = FArray("a", "b", "c", "d", "e")
    var calls = 0
    val r = xs.filterConserve { s => calls += 1; s != "c" }
    assertEquals(5, calls)
    assertEquals(List("a", "b", "d", "e"), r.toList)
    assertSame(xs(0), r(0)) // kept prefix elements are the exact original references
    assertSame(xs(1), r(1))

  @Test def ref_drop_last_once: Unit =
    val xs = FArray("a", "b", "c", "d")
    var calls = 0
    val r = xs.filterConserve { s => calls += 1; s != "d" }
    assertEquals(4, calls)
    assertEquals(List("a", "b", "c"), r.toList)

  @Test def ref_drop_all_once_empty: Unit =
    val xs = FArray("a", "b", "c")
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; false }
    assertEquals(3, calls)
    assertEquals(Nil, r.toList)
    assertSame(Empty.INSTANCE, r.asInstanceOf[FBase])

  // ---------- primitive elements: exactly-once, conserve on no-drop ----------

  @Test def int_no_drop_identity_once: Unit =
    val xs = FArray(1, 2, 3, 4, 5)
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; true }
    assertEquals(5, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])
    assertEquals(List(1, 2, 3, 4, 5), r.toList)

  @Test def int_drop_once: Unit =
    val xs = FArray(1, 2, 3, 4)
    var calls = 0
    val r = xs.filterConserve { x => calls += 1; x % 2 == 1 }
    assertEquals(4, calls)
    assertEquals(List(1, 3), r.toList)

  @Test def long_drop_once: Unit =
    val xs = FArray(10L, 20L, 30L)
    var calls = 0
    val r = xs.filterConserve { x => calls += 1; x != 20L }
    assertEquals(3, calls)
    assertEquals(List(10L, 30L), r.toList)

  // ---------- edge cases: empty / single ----------

  @Test def empty_zero_calls_identity: Unit =
    val xs = FArray.empty[String]
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; false }
    assertEquals(0, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  @Test def single_kept_identity_once: Unit =
    val xs = FArray("only")
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; true }
    assertEquals(1, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  @Test def single_dropped_once_empty: Unit =
    val xs = FArray("only")
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; false }
    assertEquals(1, calls)
    assertEquals(Nil, r.toList)

  @Test def single_int_kept_once: Unit =
    val xs = FArray(7)
    var calls = 0
    val r = xs.filterConserve { x => calls += 1; x > 0 }
    assertEquals(1, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  // ---------- structural node receiver (Concat/Prepend) — exactly-once, kept prefix originals ----------

  @Test def node_receiver_drop_middle_once: Unit =
    val xs = (FArray("a", "b") ++ FArray("c", "d", "e")) // Concat node
    var calls = 0
    val r = xs.filterConserve { s => calls += 1; s != "d" }
    assertEquals(5, calls)
    assertEquals(List("a", "b", "c", "e"), r.toList)

  @Test def node_receiver_no_drop_identity: Unit =
    val xs = (0 +: FArray("x", "y", "z").asInstanceOf[FArray[Any]]) // Prepend node
    var calls = 0
    val r = xs.filterConserve { _ => calls += 1; true }
    assertEquals(4, calls)
    assertSame(xs.asInstanceOf[FBase], r.asInstanceOf[FBase])

  // ---------- parity with List.filterConserve semantics ----------

  @Test def parity_with_list_filter: Unit =
    val base = List("a", "b", "c", "d", "e", "f")
    val fa = base.toFArray
    val p: String => Boolean = s => s != "c" && s != "e"
    // filterConserve's observable content equals filter's
    assertEquals(base.filter(p), fa.filterConserve(p).toList)
    val keepAll: String => Boolean = _ => true
    assertEquals(base, fa.filterConserve(keepAll).toList)
    assertSame(fa.asInstanceOf[FBase], fa.filterConserve(keepAll).asInstanceOf[FBase]) // no-drop identity
