package farray

import org.junit.Test
import org.junit.Assert.*

/** Round-4 Bug 2 acceptance suite: an opaque type over a PRIMITIVE (`opaque type V = Long`) — the exact dotty shape (`opaque type FlagSet = Long; type Variance =
  * FlagSet`, cf. `HKTypeLambda.variances`).
  *
  * INSIDE the defining scope `V =:= Long`, so `FArray.fill/tabulate/apply/newBuilder/map` dispatch on the dealiased `Long` and build genuine `LongArr` (long[])
  * storage. OUTSIDE the scope `V` is abstract, so every kind-dispatched op resolves through the boxed `anyRepr` fallback (the Ref arms). The invariant under
  * test: a genuine long[]-backed leaf must be readable through EVERY Ref-arm read path (apply/head/last/iterator/
  * fold/map/equality/mkString/toArray/sort/zip/distinct/pattern-match) — no `[J cannot be cast to [Ljava.lang.Object;` CCE — AND, symmetrically, a boxed-built
  * `FArray[V]` must be readable by the Long-specialized consumers inside the scope.
  */
object VarianceModule:
  opaque type V = Long
  object V:
    def apply(l: Long): V = l
    val Covariant: V = 1L
    val Contravariant: V = 2L
    val Bivariant: V = 3L
  extension (v: V) def toLong: Long = v

  // ---- BUILD INSIDE THE DEFINING SCOPE (V =:= Long -> LongArr storage) ----
  def fillInside(n: Int, v: V): FArray[V] = FArray.fill(n)(v)
  def tabulateInside(n: Int): FArray[V] = FArray.tabulate(n)(i => V(i.toLong))
  def applyInside4: FArray[V] = FArray(V.Covariant, V.Contravariant, V.Bivariant, V.Covariant)
  def applyInside(vs: Long*): FArray[V] = FArray.tabulate(vs.length)(i => V(vs(i)))
  def mapInside(xs: FArray[V]): FArray[V] = xs.map(v => V(v.toLong + 10L))
  def builderInside(vs: Seq[Long]): FArray[V] =
    val b = FArray.newBuilder[V]; vs.foreach(l => b += V(l)); b.result()

  // ---- READ INSIDE THE SCOPE (Long-specialized consumers) — for the reverse direction ----
  def sumInside(xs: FArray[V]): Long = xs.foldLeft(0L)((acc, v) => acc + v.toLong)
  def indexInside(xs: FArray[V], i: Int): Long = xs(i).toLong
  def mapInsideToLong(xs: FArray[V]): FArray[Long] = xs.map(v => v.toLong)
  def toArrayInside(xs: FArray[V]): Array[Long] = xs.toArray[Long] // materialize (Long-specialized, B=Long >: V)
  def distinctInside(xs: FArray[V]): FArray[V] = xs.distinct
  def containsInside(xs: FArray[V], v: V): Boolean = xs.contains(v)
  def indexOfInside(xs: FArray[V], v: V): Int = xs.indexOf(v)
  def takeDropInside(xs: FArray[V]): (List[Long], List[Long]) =
    (xs.take(2).iterator.map(_.toLong).toList, xs.drop(2).iterator.map(_.toLong).toList)
  def foreachSumInside(xs: FArray[V]): Long = { var s = 0L; xs.foreach(v => s += v.toLong); s }
  def minMaxInside(xs: FArray[V]): (Long, Long) =
    given Ordering[V] = Ordering.by(_.toLong); (xs.min.toLong, xs.max.toLong)
  def copyToArrayInside(xs: FArray[V]): List[Long] =
    val d = new Array[Long](xs.length); xs.copyToArray(d); d.toList
  // Long-specialized extractor reads over possibly-boxed storage: head/tail (+:) and init/last (:+)
  def prependReadInside(xs: FArray[V]): (Long, List[Long]) =
    import farray.`+:`
    xs match { case h +: t => (h.toLong, t.iterator.map(_.toLong).toList); case _ => (-1L, Nil) }
  def snocReadInside(xs: FArray[V]): (List[Long], Long) =
    import farray.`:+`
    xs match { case init :+ last => (init.iterator.map(_.toLong).toList, last.toLong); case _ => (Nil, -1L) }
  def existsFindInside(xs: FArray[V]): (Boolean, Option[Long]) =
    (xs.exists(v => v.toLong == 30L), xs.find(v => v.toLong > 20L).map(_.toLong))
  def buildAppendPrependInside(base: FArray[V]): FArray[V] = (V(-1L) +: base) :+ V(99L)

/** The exact dotty shape: a class holding an FArray[V] populated transparently (inside the scope), later read from GENERIC code that only knows the abstract V
  * (another compilation unit).
  */
final class TypeLambdaLike(val variances: FArray[VarianceModule.V]):
  def variance(i: Int): VarianceModule.V = variances(i) // read from the holder (still abstract V here)

class OpaquePrimTest:
  import VarianceModule.*
  type V = VarianceModule.V

  // helper: unwrap a whole FArray[V] to a List[Long] using the extension (works in/out of scope)
  private def asLongs(xs: FArray[V]): List[Long] = xs.iterator.map(_.toLong).toList

  // build OUTSIDE the opaque scope: here V is abstract, so newBuilder dispatches through the boxed
  // anyRepr fallback -> a genuine RefArr of boxed java.lang.Long (the "foreign leaf" for Long consumers).
  private def buildBoxed(vs: List[Long]): FArray[V] =
    val b = FArray.newBuilder[V]; vs.foreach(l => b += V(l)); b.result()

  // GENERIC readers (abstract T) — force the boxed dispatch on a genuinely long[]-backed leaf
  private def gApply[T](xs: FArray[T], i: Int): T = xs(i)
  private def gHead[T](xs: FArray[T]): T = xs.head
  private def gLast[T](xs: FArray[T]): T = xs.last
  private def gIter[T](xs: FArray[T]): List[T] = xs.iterator.toList
  private def gFold[T](xs: FArray[T])(z: Long)(op: (Long, T) => Long): Long = xs.foldLeft(z)(op)
  private def gMkString[T](xs: FArray[T]): String = xs.mkString("[", ",", "]")
  private def gToArray[T <: AnyRef: scala.reflect.ClassTag](xs: FArray[T]): Array[T] = xs.toArray
  private def gDistinct[T](xs: FArray[T]): FArray[T] = xs.distinct
  private def gReverse[T](xs: FArray[T]): List[T] = xs.reverse.iterator.toList
  private def gConcat[T](a: FArray[T], b: FArray[T]): FArray[T] = a ++ b

  // ===== forward direction: build INSIDE (long[]), read OUTSIDE through FArray[V] (boxed Ref arms) =====

  @Test def fill_inside_read_outside_apply_head_last: Unit =
    val xs = fillInside(5, V.Contravariant) // genuine LongArr
    assertEquals(5, xs.length)
    assertEquals(V.Contravariant.toLong, xs(0).toLong)
    assertEquals(V.Contravariant.toLong, xs.head.toLong)
    assertEquals(V.Contravariant.toLong, xs.last.toLong)
    // generic (abstract T) reads over the long[] leaf
    assertEquals(V.Contravariant.toLong, gApply[V](xs, 3).toLong)
    assertEquals(V.Contravariant.toLong, gHead[V](xs).toLong)
    assertEquals(V.Contravariant.toLong, gLast[V](xs).toLong)

  @Test def tabulate_inside_read_outside_iterator_fold: Unit =
    val xs = tabulateInside(6) // long[] 0..5
    assertEquals(List(0L, 1L, 2L, 3L, 4L, 5L), asLongs(xs))
    assertEquals(List(0L, 1L, 2L, 3L, 4L, 5L), gIter[V](xs).map(_.toLong))
    assertEquals(15L, gFold[V](xs)(0L)((acc, v) => acc + v.toLong))

  @Test def apply_inside_read_outside_map: Unit =
    val xs = applyInside4
    // map OUTSIDE the scope (abstract V -> boxed) over the long[] leaf
    val mapped: FArray[Long] = xs.map(v => v.toLong * 100L)
    assertEquals(List(100L, 200L, 300L, 100L), mapped.toList)
    // map V -> V outside
    val bumped: FArray[V] = xs.map(v => V(v.toLong + 1L))
    assertEquals(List(2L, 3L, 4L, 2L), asLongs(bumped))

  @Test def mkString_outside_over_long_leaf: Unit =
    val xs = tabulateInside(4)
    assertEquals("[0,1,2,3]", gMkString[V](xs))
    assertEquals("0,1,2,3", xs.mkString(","))

  @Test def equality_boxed_vs_specialized: Unit =
    val spec: FArray[V] = tabulateInside(5) // LongArr
    // build the "same" content through the boxed builder (RefArr of boxed Longs)
    val boxed: FArray[V] = buildBoxed(List(0L, 1L, 2L, 3L, 4L))
    assertNotSame(spec.asInstanceOf[AnyRef].getClass, boxed.asInstanceOf[AnyRef].getClass)
    assertEquals(spec, boxed)
    assertEquals(boxed, spec)
    assertEquals(spec.hashCode, boxed.hashCode)

  @Test def toArray_style_materialization: Unit =
    // long[] leaf materialized through the Long-specialized toArray
    assertEquals(List(0L, 1L, 2L, 3L), toArrayInside(tabulateInside(4)).toList)
    // boxed RefArr leaf materialized through the SAME Long-specialized toArray (foreign leaf -> must unbox)
    assertEquals(List(5L, 6L, 7L), toArrayInside(buildBoxed(List(5L, 6L, 7L))).toList)

  @Test def pattern_match_outside_over_long_leaf: Unit =
    val xs = applyInside4
    // case FArray(a, b, c, d)
    val r = xs match
      case FArray(a, b, c, d) => a.toLong + b.toLong + c.toLong + d.toLong
      case _                  => -1L
    assertEquals(1L + 2L + 3L + 1L, r)
    // case h +: t
    val ht = { import farray.`+:`; xs match { case h +: t => (h.toLong, asLongs(t)); case _ => (-1L, Nil) } }
    assertEquals((1L, List(2L, 3L, 1L)), ht)
    // vararg
    val v = xs match { case FArray(h, rest*) => (h.toLong, rest.toList.map(_.toLong)); case _ => (-1L, Nil) }
    assertEquals((1L, List(2L, 3L, 1L)), v)

  @Test def sort_outside_over_long_leaf: Unit =
    val xs = applyInside(3L, 1L, 2L, 5L, 4L) // LongArr
    given Ordering[V] = Ordering.by(_.toLong)
    assertEquals(List(1L, 2L, 3L, 4L, 5L), asLongs(xs.sorted))
    assertEquals(List(1L, 2L, 3L, 4L, 5L), asLongs(xs.sortBy(_.toLong)))
    assertEquals(List(1L, 2L, 3L, 4L, 5L), asLongs(xs.sortWith((a, b) => a.toLong < b.toLong)))

  @Test def zip_outside_over_long_leaf: Unit =
    val xs = tabulateInside(4)
    val ys = tabulateInside(4)
    val z = xs.zip(ys)
    assertEquals(List((0L, 0L), (1L, 1L), (2L, 2L), (3L, 3L)), z.iterator.map((a, b) => (a.toLong, b.toLong)).toList)
    // zip long-leaf against a Ref FArray
    val ss = FArray("a", "b", "c")
    assertEquals(List((0L, "a"), (1L, "b"), (2L, "c")), xs.zip(ss).iterator.map((a, b) => (a.toLong, b)).toList)

  @Test def distinct_outside_over_long_leaf: Unit =
    val xs = applyInside(1L, 2L, 2L, 3L, 1L, 3L)
    assertEquals(List(1L, 2L, 3L), asLongs(gDistinct[V](xs)))

  @Test def reverse_and_concat_outside: Unit =
    val xs = tabulateInside(4)
    assertEquals(List(3L, 2L, 1L, 0L), gReverse[V](xs).map(_.toLong))
    val ys = applyInside(10L, 11L)
    assertEquals(List(0L, 1L, 2L, 3L, 10L, 11L), asLongs(gConcat[V](xs, ys)))

  @Test def mapConserve_outside_over_long_leaf: Unit =
    val xs = applyInside(1L, 2L, 3L)
    // identity conserve (returns same content); f is V => V
    val same = xs.mapConserve(v => v)
    assertEquals(List(1L, 2L, 3L), asLongs(same))
    val bumped = xs.mapConserve(v => V(v.toLong + 100L))
    assertEquals(List(101L, 102L, 103L), asLongs(bumped))

  // ===== reverse direction: build OUTSIDE (boxed RefArr), read INSIDE (Long-specialized consumers) =====

  @Test def build_boxed_outside_read_inside_sum_index: Unit =
    val boxed: FArray[V] = buildBoxed(List(10L, 20L, 30L, 40L)) // RefArr of boxed Longs
    // Long-specialized consumers inside the scope read the RefArr leaf and must unbox
    assertEquals(100L, sumInside(boxed))
    assertEquals(30L, indexInside(boxed, 2))
    assertEquals(List(10L, 20L, 30L, 40L), mapInsideToLong(boxed).toList)

  // ===== mixed concat: LongArr ++ RefArr(boxed longs), read both ways =====

  @Test def mixed_concat_read_both_ways: Unit =
    val long: FArray[V] = tabulateInside(3) // LongArr 0,1,2
    val boxed: FArray[V] = buildBoxed(List(3L, 4L)) // RefArr 3,4
    val cat: FArray[V] = long ++ boxed
    // read OUTSIDE (boxed Ref arms)
    assertEquals(List(0L, 1L, 2L, 3L, 4L), asLongs(cat))
    assertEquals(3L, cat(3).toLong)
    // read INSIDE (Long-specialized)
    assertEquals(0L + 1L + 2L + 3L + 4L, sumInside(cat))
    // reverse concat: RefArr ++ LongArr
    val cat2: FArray[V] = boxed ++ long
    assertEquals(List(3L, 4L, 0L, 1L, 2L), asLongs(cat2))
    assertEquals(10L, sumInside(cat2))

  // ===== the exact dotty shape: holder populated transparently, read from generic code =====

  @Test def typelambda_like_holder: Unit =
    val tl = new TypeLambdaLike(tabulateInside(4)) // long[]-backed, held as FArray[V]
    assertEquals(2L, tl.variance(2).toLong)
    // generic consumer in "another unit" reads the holder's variances abstractly
    def describe[T](xs: FArray[T]): Int = xs.length
    assertEquals(4, describe(tl.variances))
    assertEquals(List(0L, 1L, 2L, 3L), tl.variances.iterator.map(_.toLong).toList)
    assertEquals(6L, sumInside(tl.variances))

  @Test def empty_and_single_opaque: Unit =
    val e: FArray[V] = FArray.empty[V]
    assertTrue(e.isEmpty)
    assertEquals(Nil, asLongs(e))
    val one: FArray[V] = fillInside(1, V.Bivariant)
    assertEquals(List(3L), asLongs(one))
    assertEquals(3L, one.head.toLong)
    assertEquals(3L, sumInside(one))

  // ===== extra audit: remaining candidate read paths, both storage directions =====

  // copyToArray over a long[] leaf (outside, into an Array[Any]) and Long-specialized (inside, boxed leaf)
  @Test def copyToArray_both_directions: Unit =
    val long = tabulateInside(4)
    val dst = new Array[Any](4); long.copyToArray(dst) // Ref-arm copyToArray over long[]
    assertEquals(List(0L, 1L, 2L, 3L), dst.toList.map(_.asInstanceOf[Long]))
    // Long-specialized copyToArray over a boxed RefArr leaf (foreign leaf -> must unbox)
    assertEquals(List(10L, 20L, 30L), copyToArrayInside(buildBoxed(List(10L, 20L, 30L))))

  @Test def lazyZip_both_directions: Unit =
    val long = tabulateInside(3) // LongArr
    val boxed = buildBoxed(List(10L, 20L, 30L)) // RefArr
    // lazyZip reads both operands element-wise; long-leaf paired with boxed-leaf
    assertEquals(List(10L, 21L, 32L), long.lazyZip(boxed).map((a, b) => a.toLong + b.toLong).toList)
    // outside generic map over the pair
    assertEquals(List(0L, 20L, 60L), long.lazyZip(boxed).map((a, b) => a.toLong * b.toLong).toList)

  @Test def indexOf_contains_both_directions: Unit =
    val long = applyInside(5L, 6L, 7L, 8L)
    assertTrue(containsInside(long, V(7L)))
    assertEquals(2, indexOfInside(long, V(7L)))
    // over a boxed RefArr leaf (Long-specialized scan must unbox)
    val boxed = buildBoxed(List(5L, 6L, 7L, 8L))
    assertTrue(containsInside(boxed, V(7L)))
    assertEquals(2, indexOfInside(boxed, V(7L)))

  @Test def take_drop_foreach_min_max: Unit =
    val long = applyInside(3L, 1L, 4L, 1L, 5L)
    assertEquals((List(3L, 1L), List(4L, 1L, 5L)), takeDropInside(long))
    assertEquals(14L, foreachSumInside(long))
    assertEquals((1L, 5L), minMaxInside(long))
    // same over a boxed RefArr leaf
    val boxed = buildBoxed(List(3L, 1L, 4L, 1L, 5L))
    assertEquals((List(3L, 1L), List(4L, 1L, 5L)), takeDropInside(boxed))
    assertEquals(14L, foreachSumInside(boxed))
    assertEquals((1L, 5L), minMaxInside(boxed))

  @Test def distinct_indexScan_inside_over_boxed: Unit =
    val boxed = buildBoxed(List(1L, 2L, 2L, 3L, 1L))
    assertEquals(List(1L, 2L, 3L), distinctInside(boxed).iterator.map(_.toLong).toList)

  // extractor Prepend/Append fast paths: a Long-specialized head/init view over a boxed RefPrepend/RefAppend
  @Test def extractor_fast_paths_over_boxed_nodes: Unit =
    val boxed = buildBoxed(List(1L, 2L, 3L)) // RefArr
    val prep: FArray[V] = V(0L) +: boxed // node prepending onto a boxed base
    assertEquals((0L, List(1L, 2L, 3L)), prependReadInside(prep))
    val app: FArray[V] = boxed :+ V(4L) // node appending onto a boxed base
    assertEquals((List(1L, 2L, 3L), 4L), snocReadInside(app))
    // and the specialized-storage counterpart (LongArr base)
    val long = applyInside(1L, 2L, 3L)
    assertEquals((0L, List(1L, 2L, 3L)), prependReadInside(V(0L) +: long))
    assertEquals((List(1L, 2L, 3L), 4L), snocReadInside(long :+ V(4L)))

  @Test def build_append_prepend_and_exists_find: Unit =
    val boxed = buildBoxed(List(10L, 20L, 30L))
    // build a Prepend/Append node over a boxed leaf, then read it back Long-specialized
    val wrapped = buildAppendPrependInside(boxed)
    assertEquals(List(-1L, 10L, 20L, 30L, 99L), wrapped.iterator.map(_.toLong).toList)
    assertEquals((true, Some(30L)), existsFindInside(boxed))
    // builder ++= a foreign-leaf FArray (boxed) inside the Long scope
    val b = FArray.newBuilder[V]
    b += V(1L); b ++= boxed; b += V(2L)
    assertEquals(List(1L, 10L, 20L, 30L, 2L), b.result().iterator.map(_.toLong).toList)
