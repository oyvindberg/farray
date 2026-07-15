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
