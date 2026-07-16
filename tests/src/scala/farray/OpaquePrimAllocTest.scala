package farray

import org.junit.Test
import org.junit.Assert.*

/** Round-5 Item 1: the ClassTag-allocation CCE in `tabulate`'s Ref/boxed-fallback arm.
  *
  * `opaque type Flag = Long` with NO given Repr forwarder. Called from OUTSIDE the defining scope with an explicit `[Flag]`, kind dispatch cannot see through
  * the opaque (no `LongRepr[Flag]` given) -> the Ref/boxed arm via `anyRepr`. But a synthesized `ClassTag[Flag]` DOES see through erasure -> `ClassTag.Long`,
  * whose `newArray` allocates a `long[]`; the old unconditional `.asInstanceOf[Array[Object]]` then threw
  * `ClassCastException: [J cannot be cast to [Ljava.lang.Object;` (this took down the scala3 bootstrap).
  *
  * The fix: in the Ref arm, if the summoned ClassTag's runtimeClass is primitive, allocate the boxed `Object[]` this arm already implies instead of the
  * primitive array; a genuine reference tag still gets the typed array (no regression for `A <: AnyRef`).
  */
object FlagModule:
  opaque type Flag = Long
  object Flag:
    def apply(l: Long): Flag = l
  extension (f: Flag) def toLong: Long = f

class OpaquePrimAllocTest:
  import FlagModule.*

  // white-box: the RefArr backing-array component class distinguishes boxed Object[] from a typed String[].
  private def backingComponent[A](xs: FArray[A]): Class[?] =
    xs.asInstanceOf[RefArr].data.getClass.getComponentType

  // ---- the CCE repros: build OUTSIDE the scope with an explicit [Flag] (abstract -> Ref arm; ClassTag -> Long) ----

  @Test def fill_opaque_primitive_no_cce: Unit =
    val xs: FArray[Flag] = FArray.fill[Flag](3)(Flag(7L))
    assertEquals(3, xs.length)
    assertEquals(List(7L, 7L, 7L), xs.iterator.map(_.toLong).toList)
    assertEquals(classOf[Object], backingComponent(xs)) // boxed Object[], NOT long[]

  @Test def tabulate_opaque_primitive_no_cce: Unit =
    val xs: FArray[Flag] = FArray.tabulate[Flag](3)(i => Flag(i.toLong * 10L))
    assertEquals(List(0L, 10L, 20L), xs.iterator.map(_.toLong).toList)
    assertEquals(classOf[Object], backingComponent(xs))

  @Test def apply_spread_opaque_primitive_no_cce: Unit =
    // a non-literal spread bypasses the apply MACRO -> runtime applyImpl -> Ref arm; still must read back
    val src: Seq[Flag] = Seq(Flag(1L), Flag(2L), Flag(3L))
    val xs: FArray[Flag] = FArray[Flag](src*)
    assertEquals(List(1L, 2L, 3L), xs.iterator.map(_.toLong).toList)

  @Test def fill_single_and_empty_opaque: Unit =
    assertTrue(FArray.fill[Flag](0)(Flag(1L)).isEmpty)
    val one: FArray[Flag] = FArray.fill[Flag](1)(Flag(9L))
    assertEquals(List(9L), one.iterator.map(_.toLong).toList)

  // ---- regression: a NON-primitive ClassTag still allocates the TYPED backing array (String[], not Object[]) ----

  @Test def tabulate_ref_uses_typed_array: Unit =
    val xs: FArray[String] = FArray.tabulate[String](3)(i => s"x$i")
    assertEquals(List("x0", "x1", "x2"), xs.toList)
    assertEquals(classOf[String], backingComponent(xs)) // typed String[], preserved
