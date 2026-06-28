package farray

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue}
import scala.compiletime.testing.typeChecks

/** Research probe: does FArray's COVARIANCE actually work? Every claim below is backed by code that
  * compiles and runs (or a `typeChecks` witness that it does NOT compile).
  *
  * Headline: covariance at the TYPE level is real (`a ++ b : FArray[Int | String]` infers and the
  * value is structurally correct), but the SPECIALIZED, `inline` element API (`apply`, `head`, `map`,
  * `foldLeft`, `foreach`, `iterator`, `toList`, …) does NOT compile for a non-`AnyRef` union element
  * type, because each routes through `summonFrom { case IntRepr[A] … case RefRepr[A] … }` and there is
  * no `Repr[Int | String]` given (`Int|String` is neither `Int` nor `<: AnyRef`). The boxed escape
  * hatch is `xs.toSeq` / `xs.toIndexedSeq` (an `FArraySeq` that reads via `applyBoxed`).
  */
class CovarianceTest:

  // ===========================================================================
  // Q1: inferred element type of `a ++ b`
  // ===========================================================================
  @Test def q1_inferred_type_of_concat: Unit =
    val a: FArray[Int] = FArray(1, 2, 3)
    val b: FArray[String] = FArray("x", "y")
    val c = a ++ b
    // `++[B >: A](that: FArray[B]): FArray[B]` ⇒ B is the LUB of Int and String, i.e. `Int | String`.
    summon[c.type <:< FArray[Int | String]] // upper-bound witness: c IS (at least) FArray[Int|String]
    // and it is precisely assignable to FArray[Int | String] (not widened to Any/Matchable, not Int):
    assertTrue(typeChecks("""
      val a: FArray[Int] = FArray(1, 2, 3); val b: FArray[String] = FArray("x", "y")
      val c: FArray[Int | String] = a ++ b
    """))

  // ===========================================================================
  // Q2a: which operations on a statically-typed FArray[Int | String] COMPILE
  // ===========================================================================
  @Test def q2_structural_ops_work: Unit =
    val a: FArray[Int] = FArray(1, 2, 3)
    val b: FArray[String] = FArray("x", "y")
    val c: FArray[Int | String] = a ++ b // assignment compiles ⇒ covariant value is usable structurally

    // Plain (non-inline) FBase-level structural ops work for ANY A, incl. the union:
    assertEquals(5, c.length)
    assertEquals(5, c.size)
    assertTrue(c.nonEmpty)
    // take/drop/reverse/slice/init/tail span the int/string boundary correctly:
    assertEquals(List[Int | String](1, 2, 3, "x"), c.take(4).toSeq.toList)
    assertEquals(List[Int | String](3, "x", "y"), c.drop(2).toSeq.toList)
    assertEquals(List[Int | String]("y", "x", 3, 2, 1), c.reverse.toSeq.toList)
    assertEquals(List[Int | String](2, 3, "x"), c.slice(1, 4).toSeq.toList)
    // headOption/lastOption are non-inline (boxed) ⇒ work:
    assertEquals(Some(1: Int | String), c.headOption)
    assertEquals(Some("y": Int | String), c.lastOption)
    // ++ chains further:
    assertEquals(7, (c ++ FArray("p", "q")).length)

  // ===========================================================================
  // Q2b: the boxed escape hatch — toSeq / toIndexedSeq read the union safely
  // ===========================================================================
  @Test def q2_boxed_view_reads_union: Unit =
    val a: FArray[Int] = FArray(1, 2, 3)
    val b: FArray[String] = FArray("x", "y")
    val c: FArray[Int | String] = a ++ b
    val seq: IndexedSeq[Int | String] = c.toIndexedSeq // FArraySeq, reads via applyBoxed

    // apply across the boundary:
    assertEquals(1, seq(0))
    assertEquals("x", seq(3))
    // pattern-match elements:
    def describe(x: Int | String): String = x match
      case i: Int    => s"int:$i"
      case s: String => s"str:$s"
    assertEquals("int:1", describe(seq(0)))
    assertEquals("str:y", describe(seq(4)))
    // iterate / map / foldLeft via the Seq (boxed, but correct):
    assertEquals(List("int:1", "int:2", "int:3", "str:x", "str:y"), seq.iterator.map(describe).toList)
    assertEquals("int:1;int:2;int:3;str:x;str:y;", seq.foldLeft("")((acc, x) => acc + describe(x) + ";"))

  // ===========================================================================
  // Q2c: SHARP EDGE — the specialized inline element API does NOT compile for the union
  //      (specialize-or-fail: no Repr[Int | String] given; summonFrom cannot reduce)
  // ===========================================================================
  @Test def q2_specialized_api_rejected_for_union: Unit =
    // The cast to FArray[Int|String] itself compiles, but each specialized (inline, summonFrom-Repr)
    // op below does NOT. `typeChecks` needs a STATICALLY-known String, so each probe is a full literal.
    inline val P = """val c: FArray[Int | String] = FArray(1,2,3) ++ FArray("x","y"); """
    assertTrue("apply must not compile",    !typeChecks(P + "val _ = c(0)"))
    assertTrue("head must not compile",     !typeChecks(P + "val _ = c.head"))
    assertTrue("last must not compile",     !typeChecks(P + "val _ = c.last"))
    assertTrue("map must not compile",      !typeChecks(P + "val _ = c.map(_.toString)"))
    assertTrue("foldLeft must not compile", !typeChecks(P + "val _ = c.foldLeft(0)((n, _) => n)"))
    assertTrue("foreach must not compile",  !typeChecks(P + "c.foreach(_ => ())"))
    assertTrue("iterator must not compile", !typeChecks(P + "val _ = c.iterator"))
    assertTrue("toList must not compile",   !typeChecks(P + "val _ = c.toList"))
    assertTrue("filter must not compile",   !typeChecks(P + "val _ = c.filter(_ => true)"))
    // sanity: the same ops DO compile on a homogeneous primitive FArray and on a Ref FArray:
    assertTrue(typeChecks("""val c = FArray(1,2,3); val _ = c(0); val _ = c.map(_+1); val _ = c.toList"""))
    assertTrue(typeChecks("""val c = FArray("a","b"); val _ = c(0); val _ = c.map(_.length); val _ = c.toList"""))
    // and a union of two REFERENCE types IS specializable (it is <: AnyRef ⇒ RefRepr applies):
    assertTrue(typeChecks("""
      val c: FArray[String | java.lang.Integer] = FArray("a") ++ FArray(java.lang.Integer.valueOf(1))
      val _ = c(0); val _ = c.map(x => x); val _ = c.toList
    """))

  // ===========================================================================
  // Q2d: the rejection produces a FRIENDLY, actionable compile error (not the raw "cannot reduce
  //      summonFrom"). Every specialized op routes its no-Repr case to `compiletime.error(...)`.
  // ===========================================================================
  @Test def q2_rejection_has_friendly_message: Unit =
    import scala.compiletime.testing.typeCheckErrors
    inline val P = """val c: FArray[Int | String] = FArray(1,2,3) ++ FArray("x","y"); """
    def assertFriendly(label: String, errs: List[scala.compiletime.testing.Error]): Unit =
      assertTrue(s"$label should not compile", errs.nonEmpty)
      val msg = errs.map(_.message).mkString(" | ")
      assertTrue(s"$label: friendly message, got: $msg", msg.contains("FArray: no element-kind specialization"))
      assertTrue(s"$label: not the raw summonFrom error, got: $msg", !msg.contains("cannot reduce summonFrom"))
    assertFriendly("apply",    typeCheckErrors(P + "val _ = c(0)"))
    assertFriendly("map",      typeCheckErrors(P + "val _ = c.map(_.toString)"))
    assertFriendly("foldLeft", typeCheckErrors(P + "val _ = c.foldLeft(0)((n, _) => n)"))
    assertFriendly("iterator", typeCheckErrors(P + "val _ = c.iterator"))
    assertFriendly("construct FArray.empty[union]", typeCheckErrors("""val _ = FArray.empty[Int | String]"""))

  // ===========================================================================
  // Q3: runtime safety — heterogeneous reads on the WORKING paths never throw
  // ===========================================================================
  @Test def q3_heterogeneous_read_no_exception: Unit =
    val a: FArray[Int] = FArray(1, 2, 3)
    val b: FArray[String] = FArray("x", "y")
    val c: FArray[Int | String] = a ++ b // Concat(IntArr int[], RefArr Object[])
    val seq = c.toIndexedSeq

    // read every element (boxed applyBoxed: int[] ints box to Integer, Object[] Strings read directly):
    var i = 0
    while i < seq.length do { val _ : Int | String = seq(i); i += 1 }
    // map + foldLeft over the heterogeneous structure:
    assertEquals(List("1", "2", "3", "x", "y"), seq.map(_.toString).toList)
    val totalIntsPlusStrLens = seq.foldLeft(0)((n, x) => n + (x match { case i: Int => i; case s: String => s.length }))
    assertEquals(6 + 2, totalIntsPlusStrLens) // (1+2+3) + len("x")+len("y")

  // ===========================================================================
  // Q3b: the UNSAFE direction — covariant upcast is safe; a force-cast downcast is the USER's fault
  // ===========================================================================
  @Test def q3_unsafe_downcast_is_user_cast: Unit =
    val ints: FArray[Int] = FArray(1, 2, 3)

    // SAFE: covariant upcast to a supertype is always legal, no exception, structure intact:
    val any: FArray[Any] = ints
    assertEquals(3, any.length)
    assertEquals(List[Any](1, 2, 3), any.toSeq.toList) // boxed read of the upcast: fine

    // UNSAFE: an unchecked user cast FArray[Int] -> FArray[String]. The opaque type erases A at
    // runtime, so neither Scala nor FArray can stop this; it is exactly like Array covariance abuse.
    val asStr: FArray[String] = ints.asInstanceOf[FArray[String]]
    var threwOnRead: String = null
    try
      val s: String = asStr.toIndexedSeq.apply(0) // boxed read, then .asInstanceOf[String] inside FArraySeq
      assertTrue(s == null || s.length >= 0)
    catch case e: Throwable => threwOnRead = e.getClass.getName
    // The CCE (if any) surfaces at the user's cast boundary, on read — not from FArray internals.
    println(s"[covariance-probe] (FArray[Int] as FArray[String]).toIndexedSeq(0) read threw: $threwOnRead")
    assertEquals("java.lang.ClassCastException", threwOnRead)

  // ===========================================================================
  // Q4: RECOMMENDED merge pattern — box the int side once (`intArray.map(box) ++ stringArray`) so
  //     BOTH halves are <: AnyRef. The result is a Ref-union FArray[Integer | String] that the FULL
  //     SPECIALIZED inline API accepts directly (RefRepr resolves) — no per-read boxing, no Seq view.
  // ===========================================================================
  @Test def q4_boxed_union_is_fully_usable: Unit =
    val ints: FArray[Int] = FArray(1, 2, 3)
    val strs: FArray[String] = FArray("x", "y")

    val boxed: FArray[java.lang.Integer] = ints.map(i => java.lang.Integer.valueOf(i)) // RefArr
    val c: FArray[java.lang.Integer | String] = boxed ++ strs // Concat(RefArr, RefArr), still <: AnyRef

    // the specialized inline API now COMPILES and runs (vs. Q2c, where the primitive union rejected it):
    assertEquals(java.lang.Integer.valueOf(1), c(0)) // apply
    assertEquals("x", c(3))
    val described = c.map { // map
      case i: java.lang.Integer => "i" + i
      case s: String            => "s" + s
    }
    assertEquals(List("i1", "i2", "i3", "sx", "sy"), described.toList) // toList
    val total = c.foldLeft(0) { // foldLeft
      case (n, i: java.lang.Integer) => n + i.intValue
      case (n, s: String)            => n + s.length
    }
    assertEquals(1 + 2 + 3 + 1 + 1, total)
    assertEquals(List[java.lang.Integer | String](1, 2, 3, "x", "y"), c.iterator.toList) // iterator
    assertEquals(2, c.filter(_.isInstanceOf[String]).length) // filter

    // compile-time witness that the WHOLE specialized API is open for the Ref-union:
    assertTrue(typeChecks("""
      val boxed = FArray(1,2,3).map(i => java.lang.Integer.valueOf(i))
      val c = boxed ++ FArray("x","y")
      val _ = c(0); val _ = c.map(x => x); val _ = c.foldLeft(0)((n,_) => n); val _ = c.toList; val _ = c.iterator
    """))
