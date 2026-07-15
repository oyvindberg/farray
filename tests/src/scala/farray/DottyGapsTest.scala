package farray

import org.junit.Test
import org.junit.Assert.*

/** Parity + pattern-match tests for the dotty-migration gap-fill surface (Phase 0):
  * `unapplySeq`, the `+:` / `:+` name-based extractors, `newBuilder`, `toFArray`, `lazyZip`,
  * `withFilter`, and the small ops. */
class DottyGapsTest:
  @Test def arity0_int: Unit =
    assertTrue(FArray.empty[Int] match { case FArray() => true; case _ => false })
    assertFalse(FArray(1) match { case FArray() => true; case _ => false })

  @Test def arity2_int_unboxed: Unit =
    val r = FArray(1, 2) match
      case FArray(a, b) => a + b // a, b must be Int (unboxed) for + to work numerically
      case _            => -1
    assertEquals(3, r)

  @Test def arity1_int: Unit =
    val r = FArray(42) match
      case FArray(a) => a
      case _         => -1
    assertEquals(42, r)

  @Test def vararg_suffix_int: Unit =
    val r: Seq[Int] = FArray(1, 2, 3, 4) match
      case FArray(h, rest*) => rest
      case _                => Seq.empty
    assertEquals(Seq(2, 3, 4), r)
    // first-element unboxed
    val h = FArray(9, 8, 7) match { case FArray(x, _*) => x; case _ => -1 }
    assertEquals(9, h)

  @Test def arity2_ref: Unit =
    val r = FArray("a", "b") match
      case FArray(a, b) => a + b
      case _            => "?"
    assertEquals("ab", r)

  @Test def vararg_suffix_ref: Unit =
    val r = FArray("x", "y", "z") match
      case FArray(h, rest*) => (h, rest.toList)
      case _                => ("?", Nil)
    assertEquals(("x", List("y", "z")), r)

  @Test def no_match_wrong_arity: Unit =
    assertFalse(FArray(1, 2, 3) match { case FArray(_, _) => true; case _ => false })

  // ---- Task 1: +: / :+ extractors ----
  @Test def prepend_head_tail_int: Unit =
    import farray.`+:`
    val r = FArray(1, 2, 3) match
      case h +: t => (h, t.toList)
      case _      => (-1, Nil)
    assertEquals((1, List(2, 3)), r)

  @Test def prepend_empty_no_match: Unit =
    import farray.`+:`
    assertFalse(FArray.empty[Int] match { case _ +: _ => true; case _ => false })

  @Test def prepend_deep_int: Unit =
    import farray.`+:`
    val r = FArray(1, 2, 3, 4) match
      case a +: b +: rest => (a, b, rest.toList)
      case _              => (-1, -1, Nil)
    assertEquals((1, 2, List(3, 4)), r)

  @Test def prepend_on_prepend_node: Unit =
    import farray.`+:`
    val fa = 0 +: FArray(1, 2, 3) // Prepend node fast path
    val r = fa match { case h +: t => (h, t.toList); case _ => (-1, Nil) }
    assertEquals((0, List(1, 2, 3)), r)

  @Test def prepend_ref: Unit =
    import farray.`+:`
    val r = FArray("a", "b", "c") match
      case h +: t => (h, t.toList) // inline toList on the ref tail — must not crash LambdaLift
      case _      => ("?", Nil)
    assertEquals(("a", List("b", "c")), r)

  @Test def snoc_init_last_int: Unit =
    import farray.`:+`
    val r = FArray(1, 2, 3) match
      case init :+ last => (init.toList, last)
      case _            => (Nil, -1)
    assertEquals((List(1, 2), 3), r)

  @Test def snoc_empty_no_match: Unit =
    import farray.`:+`
    assertFalse(FArray.empty[Int] match { case _ :+ _ => true; case _ => false })

  @Test def snoc_on_append_node: Unit =
    import farray.`:+`
    val fa = FArray(1, 2, 3) :+ 4 // Append node fast path
    val r = fa match { case init :+ last => (init.toList, last); case _ => (Nil, -1) }
    assertEquals((List(1, 2, 3), 4), r)

  @Test def snoc_deep_int: Unit =
    import farray.`:+`
    val r = FArray(1, 2, 3, 4) match
      case init :+ y :+ z => (init.toList, y, z)
      case _              => (Nil, -1, -1)
    assertEquals((List(1, 2), 3, 4), r)

  @Test def snoc_ref: Unit =
    import farray.`:+`
    val r = FArray("a", "b", "c") match
      case init :+ last => (init.toList, last) // inline toList on the ref init — must not crash
      case _            => (Nil, "?")
    assertEquals((List("a", "b"), "c"), r)

  @Test def prepend_recursive_sum: Unit =
    import farray.`+:`
    def sum(xs: FArray[Int]): Int = xs match
      case h +: t => h + sum(t)
      case _      => 0
    assertEquals(15, sum(FArray(1, 2, 3, 4, 5)))
    assertEquals(15, sum(FArray(1, 2, 3, 4, 5).map(_ + 0))) // leaf, not Prepend
