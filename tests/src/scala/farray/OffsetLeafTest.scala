package farray

import org.junit.Test
import org.junit.Assert.*

/** Phase-2 (D2b) offset-leaf parity. `tail`/`drop`/`take`/`slice`/`init` of a flat leaf now return a NEW flat leaf carrying an `offset` (logical i at
  * `data(offset + i)`) instead of a `SliceNode` wrapper. These tests (1) white-box-assert the offset leaf is actually produced (not a SliceNode, not a
  * materialized copy), and (2) prove List-parity for every engine family reading through a nonzero offset — the blast radius of the representation change.
  * Covers ref + prim kinds, nested tails (tower flattening), extractor destructuring, and fuse pipelines (the FuseMacro leaf arms feed fused loops from the raw
  * backing array).
  */
class OffsetLeafTest:

  // ---- white-box: a leaf slice is a same-class leaf with a nonzero offset, NOT a SliceNode ----
  @Test def dropOfLeaf_isOffsetLeaf_notSlice: Unit =
    val xs = FArray(0, 1, 2, 3, 4, 5, 6, 7)
    val d = xs.drop(3)
    assertTrue(s"expected IntArr, got ${d.getClass.getSimpleName}", d.isInstanceOf[IntArr])
    assertEquals(3, d.asInstanceOf[IntArr].offset)
    assertEquals(5, d.asInstanceOf[IntArr].length)
    // take keeps offset; slice/init compose; tail = drop(1)
    assertEquals(0, xs.take(4).asInstanceOf[IntArr].offset)
    assertEquals(2, xs.slice(2, 6).asInstanceOf[IntArr].offset)
    assertEquals(1, xs.tail.asInstanceOf[IntArr].offset)
    // tail-of-tail-of-tail collapses to ONE leaf with offset 3 — no SliceNode tower
    val t3 = xs.tail.tail.tail
    assertTrue(t3.isInstanceOf[IntArr])
    assertEquals(3, t3.asInstanceOf[IntArr].offset)
    // ref kind too
    val rs = FArray("a", "b", "c", "d", "e").drop(2)
    assertTrue(rs.isInstanceOf[RefArr])
    assertEquals(2, rs.asInstanceOf[RefArr].offset)
    // the backing array is SHARED (documented memory-retention tradeoff, same as SliceNode)
    assertSame(xs.asInstanceOf[IntArr].data, d.asInstanceOf[IntArr].data)

  // ---- a battery of engine ops run through a nonzero offset must match List ----
  private def checkInt(fa: FArray[Int], la: List[Int], tag: String): Unit =
    def eq[A](a: A, b: A, op: String): Unit = assertEquals(s"$tag.$op", b, a)
    eq(fa.toList, la, "toList")
    eq(fa.map(_ + 1).toList, la.map(_ + 1), "map")
    eq(fa.foldLeft(0)(_ + _), la.foldLeft(0)(_ + _), "foldLeft")
    eq(fa.foldRight(0)(_ - _), la.foldRight(0)(_ - _), "foldRight")
    eq(fa.filter(_ % 2 == 0).toList, la.filter(_ % 2 == 0), "filter")
    eq(fa.filterNot(_ % 2 == 0).toList, la.filterNot(_ % 2 == 0), "filterNot")
    eq(fa.collect { case x if x % 2 == 0 => x * 10 }.toList, la.collect { case x if x % 2 == 0 => x * 10 }, "collect")
    eq(fa.scanLeft(0)(_ + _).toList, la.scanLeft(0)(_ + _), "scanLeft")
    eq(fa.scanRight(0)(_ + _).toList, la.scanRight(0)(_ + _), "scanRight")
    eq(fa.exists(_ > 5), la.exists(_ > 5), "exists")
    eq(fa.forall(_ >= 0), la.forall(_ >= 0), "forall")
    eq(fa.find(_ > 4), la.find(_ > 4), "find")
    eq(fa.indexWhere(_ > 4), la.indexWhere(_ > 4), "indexWhere")
    eq(fa.lastIndexWhere(_ < 5), la.lastIndexWhere(_ < 5), "lastIndexWhere")
    eq(fa.count(_ % 2 == 1), la.count(_ % 2 == 1), "count")
    eq(fa.reverse.toList, la.reverse, "reverse")
    eq(fa.iterator.toList, la, "iterator")
    eq(fa.reverseIterator.toList, la.reverse, "reverseIterator")
    eq(fa.sum, la.sum, "sum")
    eq(fa.mkString("[", ",", "]"), la.mkString("[", ",", "]"), "mkString")
    eq(fa.distinct.toList, la.distinct, "distinct")
    eq(fa.sortWith(_ > _).toList, la.sortWith(_ > _), "sortWith")
    eq(fa.groupBy(_ % 3).view.mapValues(_.toList).toMap, la.groupBy(_ % 3), "groupBy")
    eq(fa.zipWithIndex.toList, la.zipWithIndex, "zipWithIndex")
    eq(fa.flatMap(x => FArray(x, x + 100)).toList, la.flatMap(x => List(x, x + 100)), "flatMap")
    eq(fa.##, FArray.fromIterable(la).##, "hashCode")
    assertTrue(s"$tag.equals", fa == FArray.fromIterable(la))
    if la.nonEmpty then
      eq(fa.head, la.head, "head")
      eq(fa.last, la.last, "last")
      eq(fa(la.size - 1), la(la.size - 1), "apply")
    // toArray / copyToArray through offset
    eq(fa.toArray.toList, la, "toArray")
    val dst = new Array[Int](la.size + 2)
    fa.copyToArray(dst, 1, la.size)
    eq(dst.slice(1, 1 + la.size).toList, la, "copyToArray")

  @Test def sliceThenOp_everyEngine_int: Unit =
    val base = FArray((0 until 12)*)
    val lbase = (0 until 12).toList
    // exercise many (from, until) windows so offset != 0 in each engine
    for from <- 0 to 4; until <- 8 to 12 do checkInt(base.slice(from, until), lbase.slice(from, until), s"slice($from,$until)")
    checkInt(base.drop(3), lbase.drop(3), "drop3")
    checkInt(base.take(9).drop(2), lbase.take(9).drop(2), "take9.drop2")
    checkInt(base.tail.tail.tail, lbase.tail.tail.tail, "tail^3")
    checkInt(base.init.drop(2), lbase.init.drop(2), "init.drop2")

  // ---- extractor destructuring over offset leaves (nested `case h +: t`) ----
  @Test def extractorOverOffsetLeaf_int: Unit =
    def drain(fa: FArray[Int]): List[Int] = fa match
      case h +: t => h :: drain(t)
      case _      => Nil
    val xs = FArray(10, 20, 30, 40, 50).drop(1) // offset leaf (starts at 20)
    assertEquals(List(20, 30, 40, 50), drain(xs))
    // 3-deep unpack in one pattern
    val r = xs match
      case a +: b +: c +: rest => (a, b, c, rest.toList)
      case _                   => (0, 0, 0, Nil)
    assertEquals((20, 30, 40, List(50)), r)

  @Test def extractorOverOffsetLeaf_ref: Unit =
    def drain(fa: FArray[String]): List[String] = fa match
      case h +: t => h :: drain(t)
      case _      => Nil
    val xs = FArray("a", "b", "c", "d").slice(1, 4) // offset leaf
    assertEquals(List("b", "c", "d"), drain(xs))

  // ---- fuse pipelines over offset-leaf sources (FuseMacro leaf arms) ----
  @Test def fuseOverOffsetLeaf_int: Unit =
    val xs = FArray((0 until 20)*).drop(5).take(10) // offset leaf, logical 5..14
    val ls = (0 until 20).toList.drop(5).take(10)
    assertEquals(ls.map(_ + 1).filter(_ % 2 == 0).map(_ * 3), xs.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 3).run.toList)
    assertEquals(ls.filter(_ > 8).sum, xs.fuse.filter(_ > 8).map(identity).run.toList.sum)
    assertEquals(ls.count(_ % 3 == 0), xs.fuse.filter(_ % 3 == 0).count)

  @Test def fuseOverOffsetLeaf_ref: Unit =
    val xs = FArray("aa", "bb", "cc", "dd", "ee", "ff").drop(2) // offset leaf
    val ls = List("aa", "bb", "cc", "dd", "ee", "ff").drop(2)
    assertEquals(ls.map(_.toUpperCase).filter(_.startsWith("C")), xs.fuse.map(_.toUpperCase).filter(_.startsWith("C")).run.toList)

  // ---- prim kinds other than Int read correctly through an offset ----
  @Test def sliceThenOp_longDouble: Unit =
    val lg = FArray(1L, 2L, 3L, 4L, 5L, 6L).drop(2)
    assertEquals(List(3L, 4L, 5L, 6L), lg.toList)
    assertEquals(18L, lg.foldLeft(0L)(_ + _))
    assertEquals(List(6L, 8L, 10L, 12L), lg.map(_ * 2).toList)
    val db = FArray(1.5, 2.5, 3.5, 4.5).slice(1, 3)
    assertEquals(List(2.5, 3.5), db.toList)
    assertEquals(6.0, db.sum, 0.0)
    assertEquals(FArray.fromIterable(List(2.5, 3.5)).##, db.##)

  // ---- slack + offset compose: a builder-produced leaf carries capacity>length AND can be sliced ----
  @Test def offsetOverSlackBackedLeaf: Unit =
    val built = FArray.fromIterable(0 until 10).filter(_ => true) // may carry slack
    val sliced = built.drop(3).take(4)
    assertEquals((0 until 10).toList.drop(3).take(4), sliced.toList)
    assertEquals((0 until 10).toList.drop(3).take(4).map(_ + 1), sliced.map(_ + 1).toList)
    // OOB still throws against LOGICAL length (Phase-1 invariant holds under offset)
    val d = FArray(0, 1, 2, 3, 4).drop(2) // logical length 3
    assertThrows(classOf[IndexOutOfBoundsException], () => d(3))
    assertEquals(4, d(2))
