package farray

import org.junit.Test

import scala.collection.BuildFrom

// products used by the fused case-class decomposition tests
case class P2(a: Int, b: Int)
case class Inner(x: Int, y: Int)
case class Outer(inner: Inner, z: Int)

class FListTest:
  def foo(str: String): Either[String, String] = if (str.length > 2) Left(str) else Right(str)

  @Test def test_++ : Unit = test2(_ ++ _)(_ ++ _)
  @Test def test_+: : Unit = test1("x" +: _)("x" +: _)
  @Test def test_:+ : Unit = test1(_ :+ "x")(_ :+ "x")
  @Test def test_:: : Unit = test1("x" :: _)("x" :: _)
  @Test def test_::: : Unit = test2(_ ::: _)(_ ::: _)
  @Test def test_listSyntax: Unit = {
    import farray.ListSyntax.* // method-local: shadows scala.:: only here
    val xs = 1 :: 2 :: 3 :: Nil
    assert(xs.toList == List(1, 2, 3))
    xs match
      case h :: t => assert(h == 1 && t.toList == List(2, 3))
      case _      => assert(false, "should be non-empty")
    val empty: FArray[Int] = Nil
    assert(empty match { case _ :: _ => false; case _ => true })
    def sum(ys: FArray[Int]): Int = ys match { case h :: t => h + sum(t); case _ => 0 }
    assert(sum(xs) == 6)
    assert(sum(xs.map(_ + 1)) == 9) // pick apart after a map (leaf, not Prepend)
    val ss = "a" :: "bb" :: Nil // reference elements too
    assert((ss match { case h :: _ => h; case _ => "" }) == "a")
  }
  @Test def test_iterator_trees: Unit = {
    def check(fa: FArray[Int], expected: List[Int]): Unit =
      assert(fa.iterator.toList == expected, s"iter ${fa.iterator.toList} != $expected")
      assert(fa.reverseIterator.toList == expected.reverse, s"reviter ${fa.reverseIterator.toList} != ${expected.reverse}")
    val a = FArray(1, 2, 3); val b = FArray(4, 5, 6)
    check(a, List(1, 2, 3))
    check(a ++ b, List(1, 2, 3, 4, 5, 6)) // Concat
    check(a :+ 7, List(1, 2, 3, 7)) // Append
    check(0 +: a, List(0, 1, 2, 3)) // Prepend
    check(a.reverse, List(3, 2, 1)) // Reverse
    check(a.drop(1), List(2, 3)) // Slice
    check((a ++ b).drop(1).take(3), List(2, 3, 4)) // Slice over Concat
    check(0 +: (a ++ b) :+ 9, List(0, 1, 2, 3, 4, 5, 6, 9)) // Prepend + Concat + Append
    check(a.padTo(5, 0), List(1, 2, 3, 0, 0)) // Pad
    check(a.updated(1, 9), List(1, 9, 3)) // Updated
    check(((a ++ b) ++ (a ++ b)).reverse, List(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6).reverse) // Reverse of nested Concat
    check(FArray.empty[Int], Nil)
    val s = FArray("a", "b"); assert(((s :+ "c") ++ FArray("d")).iterator.toList == List("a", "b", "c", "d")) // Ref cursor
  }
  @Test def test_iterator_fuzz: Unit = {
    val rng = new java.util.Random(0xf00dL)
    def clamp(k: Int, n: Int): Int = if k < 0 then 0 else if k > n then n else k
    // build a random FArray and the equivalent List in lockstep, via deeply-nested complex ops
    def gen(depth: Int): (FArray[Int], List[Int]) =
      if depth <= 0 then
        val n = rng.nextInt(12)
        val xs = List.fill(n)(rng.nextInt(1000))
        (FArray(xs*), xs)
      else
        val (fa, la) = gen(depth - 1)
        rng.nextInt(9) match
          case 0 => val (fb, lb) = gen(depth - 1); (fa ++ fb, la ++ lb) // Concat
          case 1 => val e = rng.nextInt(1000); (fa :+ e, la :+ e) // Append
          case 2 => val e = rng.nextInt(1000); (e +: fa, e :: la) // Prepend
          case 3 => (fa.reverse, la.reverse) // Reverse
          case 4 => val k = clamp(rng.nextInt(la.length + 2), la.length); (fa.drop(k), la.drop(k))
          case 5 => val k = clamp(rng.nextInt(la.length + 2), la.length); (fa.take(k), la.take(k))
          case 6 => {
            val lo = clamp(rng.nextInt(la.length + 2), la.length); val hi = clamp(lo + rng.nextInt(la.length + 2), la.length);
            (fa.slice(lo, hi), la.slice(lo, hi))
          } // Slice
          case 7 => val len = la.length + rng.nextInt(4) - 1; val e = rng.nextInt(1000); (fa.padTo(len, e), la.padTo(len, e))
          case _ => if la.isEmpty then (fa, la) else { val i = rng.nextInt(la.length); val e = rng.nextInt(1000); (fa.updated(i, e), la.updated(i, e)) }
    var t = 0
    while t < 5000 do
      val (fa, la) = gen(rng.nextInt(7))
      assert(fa.length == la.length, s"len ${fa.length} != ${la.length}")
      assert(fa.iterator.toList == la, s"iter ${fa.iterator.toList} != $la")
      assert(fa.reverseIterator.toList == la.reverse, s"reviter mismatch on $la")
      var i = 0; while i < la.length do { assert(fa(i) == la(i), s"apply($i) ${fa(i)} != ${la(i)} on $la"); i += 1 }
      t += 1
    // a few genuinely long ones: 20k-element leaf put through many slice/concat/reverse layers
    var bigF = FArray.tabulate(20000)(i => i); var bigL = (0 until 20000).toList
    var r = 0
    while r < 40 do
      rng.nextInt(4) match
        case 0 => bigF = bigF ++ bigF.take(100); bigL = bigL ++ bigL.take(100)
        case 1 => bigF = bigF.reverse; bigL = bigL.reverse
        case 2 => val k = rng.nextInt(50); bigF = bigF.drop(k); bigL = bigL.drop(k)
        case _ => val e = rng.nextInt(9); bigF = e +: bigF; bigL = e :: bigL
      r += 1
    assert(bigF.iterator.toList == bigL, "big iter mismatch")
    assert(bigF.reverseIterator.toList == bigL.reverse, "big reviter mismatch")
  }
  @Test def test_foreachWhile: Unit = {
    val fa = FArray(1, 2, 3, 4, 5)
    val seen = scala.collection.mutable.ListBuffer[Int]()
    fa.foreachWhile(x => { seen += x; x < 3 }) // processes 1,2,3 (3 returns false), stops before 4
    assert(seen.toList == List(1, 2, 3), s"foreachWhile stop: ${seen.toList}")
    var sum = 0; fa.foreachWhile(x => { sum += x; true }) // never breaks: full pass
    assert(sum == 15)
    fa.foreachWhile(_ => false) // breaks immediately
    FArray.empty[Int].foreachWhile(_ => { assert(false, "empty should not call f"); true })
    // over a tree
    val t = (FArray(1, 2) ++ FArray(3, 4)) :+ 5
    val s2 = scala.collection.mutable.ListBuffer[Int](); t.foreachWhile(x => { s2 += x; x != 3 })
    assert(s2.toList == List(1, 2, 3))
  }
  @Test def test_match2_trees: Unit = {
    val a = FArray(1, 2, 3, 4); val tree = FArray(1, 2) ++ FArray(3, 4) // Concat (non-leaf)
    assert(a.startsWith(tree) && tree.startsWith(a)) // tree as `that` / as `xs`
    assert(tree.startsWith(FArray(1, 2)) && !tree.startsWith(FArray(1, 9)))
    assert(a.endsWith(FArray(3, 4)) && tree.endsWith(FArray(3, 4)) && !a.endsWith(FArray(9, 4)))
    assert(a.corresponds(tree)(_ == _) && tree.corresponds(a)(_ == _))
    assert(!a.corresponds(FArray(1, 2, 3))(_ == _)) // length mismatch
    val s = FArray("a", "b", "c"); val st = FArray("a") ++ FArray("b", "c") // Ref tree
    assert(s.startsWith(st) && st.startsWith(s) && s.corresponds(st)(_ == _) && st.endsWith(FArray("c")))
  }
  @Test def test_factories: Unit = {
    assert(FArray.fill(3)(7).toList == List(7, 7, 7) && FArray.fill(0)(7).toList == Nil)
    var c = 0; assert(FArray.fill(3)({ c += 1; c }).toList == List(1, 2, 3)) // elem re-evaluated per element
    assert(FArray.iterate(1, 4)(_ * 2).toList == List(1, 2, 4, 8) && FArray.iterate(1, 0)(_ * 2).toList == Nil)
    assert(FArray.from(List(1, 2, 3)).toList == List(1, 2, 3))
    assert(FArray.from(Iterator(4, 5, 6)).toList == List(4, 5, 6)) // IterableOnce: Iterator
    assert(FArray.concat(FArray(1, 2), FArray(3), FArray(4, 5)).toList == List(1, 2, 3, 4, 5))
    assert(FArray.concat[Int]().toList == Nil)
    assert(FArray.unfold(1)(s => if s > 16 then None else Some((s, s * 2))).toList == List(1, 2, 4, 8, 16))
    assert(FArray.fill(2)("x").toList == List("x", "x")) // reference element
  }
  @Test def test_apply: Unit = test1NonEmpty(_(0))(_(0))
  @Test def test_collect: Unit = test1(_.collect { case str if str.nonEmpty => str })(_.collect { case str if str.nonEmpty => str })
  @Test def test_collectFirst: Unit = test1(_.collectFirst { case str if str.nonEmpty => str })(_.collectFirst { case str if str.nonEmpty => str })
  @Test def test_contains: Unit = test1(_.contains("a"))(_.contains("a"))
  @Test def test_corresponds: Unit = test1(xs => xs.corresponds(xs.take(2))(_ == _))(xs => xs.corresponds(xs.take(2))(_ == _))
  @Test def test_count: Unit = test1(_.count(_.contains('a')))(_.count(_.contains('a')))
  @Test def test_diff: Unit = test1(xs => xs.diff(xs.take(2)))(xs => xs.diff(xs.take(2)))
  @Test def test_distinct: Unit = test1(_.distinct)(_.distinct)
  @Test def test_distinct_by: Unit = test1(_.distinctBy(_.length))(_.distinctBy(_.length))
  @Test def test_drop: Unit = test1(_.drop(1))(_.drop(1))
  @Test def test_dropRight: Unit = test1(_.dropRight(1))(_.dropRight(1))
  @Test def test_dropWhile: Unit = test1(_.dropWhile(!_.headOption.contains('b')))(_.dropWhile(!_.headOption.contains('b')))
  @Test def test_endsWith: Unit = test1(xs => xs.endsWith(xs.take(2)))(xs => xs.endsWith(xs.take(2)))
  @Test def test_equals: Unit = test1(xs => xs.equals(xs.take(2)))(xs => xs.equals(xs.take(2)))
  @Test def test_exists: Unit = test1(_.exists(_ == "a"))(_.exists(_ == "a"))
  @Test def test_filter: Unit = test1(xs => xs.filter(xs.headOption.contains))(xs => xs.filter(xs.headOption.contains))
  @Test def test_filterNot: Unit = test1(xs => xs.filterNot(xs.headOption.contains))(xs => xs.filterNot(xs.headOption.contains))
  @Test def test_find: Unit = test1(_.find(_.headOption.contains('a')))(_.find(_.headOption.contains('a')))
  @Test def test_flatMap: Unit = test1(_.flatMap(str => List(str, str)))(_.flatMap(str => FArray(str, str)))
  @Test def test_flatten: Unit = test1(_.map(str => List(str, str)).flatten)(_.map(str => FArray(str, str)).flatten)
  @Test def test_fold: Unit = test1(_.fold("")(_ + _))(_.fold("")(_ + _))
  @Test def test_foldLeft: Unit = test1(_.foldLeft(0)(_ + _.length))(_.foldLeft(0)(_ + _.length))
  @Test def test_foldRight: Unit = test1(_.foldRight("")(_.headOption.getOrElse('x').toString + _))(_.foldRight("")(_.headOption.getOrElse('x').toString + _))
  @Test def test_forall: Unit = test1(_.forall(_.isEmpty))(_.forall(_.isEmpty))
  @Test def test_foreach: Unit = test1({ xs =>
    var ret = ""; xs.foreach(ret += _); ret
  })({ xs =>
    var ret = ""; xs.foreach(ret += _); ret
  })
  @Test def test_groupBy: Unit = test1(_.groupBy(x => x.length))(_.groupBy(x => x.length))
  @Test def test_groupMap: Unit = test1(_.groupMap(x => x.length)(_ * 2))(_.groupMap(x => x.length)(_ * 2))
  @Test def test_head: Unit = test1NonEmpty(_.head)(_.head)
  @Test def test_headOption: Unit = test1(_.headOption)(_.headOption)
  @Test def test_indexOf: Unit = test1(_.indexOf("a"))(_.indexOf("a"))
  @Test def test_indexWhere: Unit = test1(_.indexWhere(_ == "a"))(_.indexWhere(_ == "a"))
  @Test def test_indices: Unit = test1(_.indices)(_.indices)
  @Test def test_init: Unit = test1NonEmpty(_.init)(_.init)
  @Test def test_intersect: Unit = test2(_ intersect _)(_ intersect _)
  @Test def test_isDefinedAt: Unit = test1(_.isDefinedAt(1))(_.isDefinedAt(1))
  @Test def test_isEmpty: Unit = test1(_.isEmpty)(_.isEmpty)
  @Test def test_iterator: Unit = test1(_.iterator)(_.iterator)
  @Test def test_last: Unit = test1NonEmpty(_.last)(_.last)
  @Test def test_lastOption: Unit = test1(_.lastOption)(_.lastOption)
  @Test def test_lazyZip: Unit = test3(_.lazyZip(_).lazyZip(_).toList)(_.lazyZip(_, _))
  @Test def test_lengthCompare: Unit = test1(_.lengthCompare(1))(_.lengthCompare(1))
  @Test def `test_lengthIs >`: Unit = test1(_.lengthIs > 2)(_.lengthIs > 2)
  @Test def `test_lengthIs <=`: Unit = test1(_.lengthIs <= 2)(_.lengthIs <= 2)
  @Test def test_map: Unit = test1(_.map(_.toUpperCase))(_.map(_.toUpperCase))
  @Test def test_mapConserve: Unit = test1(xs => xs.mapConserve(x => x) eq xs)(xs => xs.mapConserve(x => x) eq xs)
  @Test def test_max: Unit = test1NonEmpty(_.max)(_.max)
  @Test def test_maxBy: Unit = test1NonEmpty(_.maxBy(_.length))(_.maxBy(_.length))
  @Test def test_min: Unit = test1NonEmpty(_.min)(_.min)
  @Test def test_minBy: Unit = test1NonEmpty(_.minBy(_.length))(_.minBy(_.length))
  @Test def test_mkString: Unit = test1(_.mkString("_", ";", "-"))(_.mkString("_", ";", "-"))
  @Test def test_nonEmpty: Unit = test1(_.nonEmpty)(_.nonEmpty)
  @Test def test_padTo: Unit = test1(_.padTo(10, ""))(_.padTo(10, ""))
  @Test def test_partition: Unit = test1(_.partition(_.length > 2))(_.partition(_.length > 2))
  @Test def test_partitionMap: Unit = test1(_.partitionMap(foo))(_.partitionMap(foo))
  @Test def test_reduce: Unit = test1NonEmpty(_.reduce((acc, str) => acc + str))(_.reduce((acc, str) => acc + str))
  @Test def test_reduceLeft: Unit = test1NonEmpty(_.reduceLeft((acc, str) => acc + str))(_.reduceLeft((acc, str) => acc + str))
  @Test def test_reduceRight: Unit = test1NonEmpty(_.reduceRight((str, acc) => acc + str))(_.reduceRight((str, acc) => acc + str))
  @Test def test_reduceOption: Unit = test1(_.reduceOption((acc, str) => acc + str))(_.reduceOption((acc, str) => acc + str))
  @Test def test_reverse: Unit = test1(_.reverse)(_.reverse)
  @Test def `test_reverse_:::`: Unit = test2(_.reverse_:::(_))(_.reverse_:::(_))
  @Test def test_reverseIterator: Unit = test1(_.reverseIterator.toList)(_.reverseIterator.toList)
  @Test def test_size: Unit = test1(_.size)(_.size)
  @Test def `test_sizeIs >`: Unit = test1(_.sizeIs > 2)(_.sizeIs > 2)
  @Test def `test_sizeIs <=`: Unit = test1(_.sizeIs <= 2)(_.sizeIs <= 2)
  @Test def test_sortBy: Unit = test1(_.sortBy(x => x))(_.sortBy(x => x))
  @Test def test_sorted: Unit = test1(_.sorted)(_.sorted)
  @Test def test_sortWith: Unit = test1(_.sortWith((x, y) => x < y))(_.sortWith((x, y) => x < y))
  @Test def test_span: Unit = test1(_.span(_.length > 2))(_.span(_.length > 2))
  @Test def test_splitAt: Unit = test1(_.splitAt(2))(_.splitAt(2))
  @Test def test_startsWith: Unit = test1(xs => xs.startsWith(xs.takeRight(2)))(xs => xs.startsWith(xs.takeRight(2)))
  @Test def test_tail: Unit = test1NonEmpty(_.tail)(_.tail)
  @Test def test_take: Unit = test1(_.take(2))(_.take(2))
  @Test def test_takeRight: Unit = test1(_.takeRight(2))(_.takeRight(2))
  @Test def test_takeWhile: Unit = test1(_.takeWhile(_.length <= 2))(_.takeWhile(_.length <= 2))
  @Test def test_toArray: Unit = test1(_.toArray)(_.toArray)
  @Test def test_toList: Unit = test1(_.toList)(_.toList)
  @Test def test_toMap: Unit = test1(_.map(x => x -> x).toMap)(_.map(x => x -> x).toMap)
  @Test def test_toSet: Unit = test1(_.toSet)(_.toSet)
  @Test def test_toVector: Unit = test1(_.toVector)(_.toVector)
  @Test def test_transpose: Unit = test1(_.map(_.toList).transpose)(_.map(str => FArray.fromIterable(str.map(c => c: Character))).transpose)
  @Test def test_unzip: Unit = test1(_.map(x => x -> x).unzip)(_.map(x => x -> x).unzip)
  @Test def test_unzip3: Unit = test1(_.map(x => (x, x, x)).unzip3)(_.map(x => (x, x, x)).unzip3)
  @Test def test_updated: Unit = test1NonEmpty(_.updated(0, "yy"))(_.updated(0, "yy"))
  @Test def test_zip: Unit = test2(_ zip _)(_ zip _)
  @Test def test_zipWithIndex: Unit = test1(_.zipWithIndex)(_.zipWithIndex)

  // ---- newly-added IndexedSeq ops, each checked against List on the same inputs ----
  @Test def test_sum: Unit = test1(_.map(_.length).sum)(_.map(_.length).sum)
  @Test def test_product: Unit = test1(_.map(_.length).product)(_.map(_.length).product)
  @Test def test_scanLeft: Unit = test1(_.scanLeft("z")(_ + _))(_.scanLeft("z")(_ + _))
  @Test def test_scan: Unit = test1(_.scan("z")(_ + _))(_.scan("z")(_ + _))
  @Test def test_scanRight: Unit = test1(_.scanRight("z")(_ + _))(_.scanRight("z")(_ + _))
  @Test def test_reduceLeftOption: Unit = test1(_.reduceLeftOption(_ + _))(_.reduceLeftOption(_ + _))
  @Test def test_reduceRightOption: Unit = test1(_.reduceRightOption((s, acc) => s + acc))(_.reduceRightOption((s, acc) => s + acc))
  @Test def test_segmentLength: Unit = test1(_.segmentLength(_.nonEmpty, 1))(_.segmentLength(_.nonEmpty, 1))
  @Test def test_lastIndexWhere: Unit = test1(_.lastIndexWhere(_ == "a"))(_.lastIndexWhere(_ == "a"))
  @Test def test_lastIndexOf: Unit = test1(_.lastIndexOf("a"))(_.lastIndexOf("a"))
  @Test def test_sameElements: Unit = test1(xs => xs.sameElements(xs.take(2)))(xs => xs.sameElements(xs.take(2)))
  @Test def test_indexOfSlice: Unit = test1(xs => xs.indexOfSlice(xs.slice(2, 4)))(xs => xs.indexOfSlice(xs.slice(2, 4)))
  @Test def test_lastIndexOfSlice: Unit = test1(xs => xs.lastIndexOfSlice(xs.slice(1, 3)))(xs => xs.lastIndexOfSlice(xs.slice(1, 3)))
  @Test def test_containsSlice: Unit = test1(xs => xs.containsSlice(xs.slice(1, 3)))(xs => xs.containsSlice(xs.slice(1, 3)))
  @Test def test_grouped: Unit = test1(_.grouped(2))(_.grouped(2))
  @Test def test_sliding: Unit = test1(_.sliding(2, 1))(_.sliding(2, 1))
  @Test def test_sliding_step: Unit = test1(_.sliding(3, 2))(_.sliding(3, 2))
  @Test def test_inits: Unit = test1(_.inits)(_.inits)
  @Test def test_tails: Unit = test1(_.tails)(_.tails)
  @Test def test_patch: Unit = test1(xs => xs.patch(1, xs.take(2), 2))(xs => xs.patch(1, xs.take(2), 2))
  @Test def test_toIndexedSeq: Unit = test1(_.toIndexedSeq)(_.toIndexedSeq)
  @Test def test_combinations: Unit = test1(_.combinations(2))(_.combinations(2))
  @Test def test_permutations: Unit = test1(_.permutations)(_.permutations)
  @Test def test_copyToArray: Unit =
    lists.foreach { list =>
      val fa = FArray.fromIterable(list)
      val da = new Array[String](list.length + 2); val db = new Array[String](list.length + 2)
      val na = list.copyToArray(da, 1, list.length); val nb = fa.copyToArray(db, 1, list.length)
      assertEquals(list.toString, na.toLong, nb.toLong)
      assertEquals(list.toString, da.toList, db.toList)
    }

  // toArray / mkString / copyToArray across element kinds + tree shapes (specialized FArrayOps impls).
  @Test def test_toArray_kinds(): Unit =
    assert(FArray(1, 2, 3).toArray.sameElements(Array(1, 2, 3)))
    assert((FArray(1, 2) ++ FArray(3, 4)).toArray.sameElements(Array(1, 2, 3, 4))) // Concat
    assert((FArray(1, 2, 3) :+ 4).toArray.sameElements(Array(1, 2, 3, 4))) // Append
    assert((0 +: FArray(1, 2, 3)).toArray.sameElements(Array(0, 1, 2, 3))) // Prepend
    assert(FArray(1, 2, 3, 4).reverse.toArray.sameElements(Array(4, 3, 2, 1))) // Reverse (backward run)
    assert(FArray.range(0, 5).toArray.sameElements(Array(0, 1, 2, 3, 4))) // RangeNode
    assert(FArray(1L, 2L, 3L).toArray.sameElements(Array(1L, 2L, 3L)))
    assert(FArray(1.5, 2.5).toArray.sameElements(Array(1.5, 2.5)))
    assert(FArray("a", "b").toArray.sameElements(Array("a", "b")))
    assert(FArray.empty[Int].toArray.sameElements(Array.empty[Int]))
    val anyArr: Array[Any] = FArray(1, 2, 3).toArray[Any] // wide ClassTag -> typed-loop path
    assert(anyArr.toList == List(1, 2, 3))

  @Test def test_mkString_kinds(): Unit =
    assert(FArray(1, 2, 3).mkString("[", ",", "]") == "[1,2,3]")
    assert((FArray(1, 2) ++ FArray(3, 4)).mkString(",") == List(1, 2, 3, 4).mkString(","))
    assert(FArray(1, 2, 3, 4).reverse.mkString(",") == "4,3,2,1")
    assert(FArray(1L, 2L, 3L).mkString("-") == "1-2-3")
    assert(FArray(1.5, 2.5).mkString(",") == "1.5,2.5")
    assert(FArray("a", "b", "c").mkString == "abc")
    assert(FArray.empty[Int].mkString("(", ",", ")") == "()")
    assert(FArray.range(0, 4).mkString(",") == "0,1,2,3")

  @Test def test_copyToArray_kinds(): Unit =
    // primitive kind, partial len, non-trivial start, over a tree
    val src = (FArray(1, 2) ++ FArray(3, 4)) :+ 5
    val dst = Array.fill(8)(-1)
    val n = src.copyToArray(dst, 2, 3) // copy first 3 into [2..4]
    assert(n == 3 && dst.toList == List(-1, -1, 1, 2, 3, -1, -1, -1), dst.toList.toString)
    val full = new Array[Int](5); assert(src.copyToArray(full, 0, 99) == 5 && full.toList == List(1, 2, 3, 4, 5))
    val strDst = new Array[String](3); FArray("x", "y").copyToArray(strDst, 0, 2)
    assert(strDst.toList == List("x", "y", null))
    // n == 0 and n == 1 fast paths (no dfs): cover empty source, single-element leaf and One nodes,
    // partial copies that clamp to 1, and the no-room (len 0 / start past end) cases that return 0.
    locally {
      val d0 = Array.fill(3)(-1)
      assert(FArray.empty[Int].copyToArray(d0, 0, 3) == 0 && d0.toList == List(-1, -1, -1)) // empty src
      assert(FArray(7).copyToArray(d0, 0, 0) == 0 && d0.toList == List(-1, -1, -1)) // len 0
      assert(FArray(7).copyToArray(d0, 3, 5) == 0 && d0.toList == List(-1, -1, -1)) // no room
      val d1 = Array.fill(3)(-1)
      assert(FArray(9).copyToArray(d1, 1, 1) == 1 && d1.toList == List(-1, 9, -1)) // leaf size 1
      val d2 = Array.fill(3)(-1)
      assert((0 +: FArray.empty[Int]).copyToArray(d2, 0, 1) == 1 && d2.toList == List(0, -1, -1)) // One node
      val d3 = Array.fill(3)(-1)
      assert(FArray(1, 2, 3).copyToArray(d3, 0, 1) == 1 && d3.toList == List(1, -1, -1)) // clamp len to 1
      val d4 = Array.fill(2)(-1)
      assert(FArray(1, 2, 3).copyToArray(d4, 1, 9) == 1 && d4.toList == List(-1, 1)) // clamp avail to 1
      // matches List semantics for n == 0 / 1
      assert(FArray(5).copyToArray(Array.fill(3)(0), 0, 1) == List(5).copyToArray(Array.fill(3)(0), 0, 1))
    }

  @Test def test_smallArity_apply(): Unit =
    assert(FArray(1, 2, 3, 4).toList == List(1, 2, 3, 4))
    assert(FArray(1, 2, 3, 4, 5).toList == List(1, 2, 3, 4, 5))
    assert(FArray(1, 2, 3, 4, 5, 6).toList == List(1, 2, 3, 4, 5, 6))
    assert(FArray(1, 2, 3, 4, 5, 6, 7).toList == List(1, 2, 3, 4, 5, 6, 7))
    assert(FArray(1, 2, 3, 4, 5, 6, 7, 8).toList == List(1, 2, 3, 4, 5, 6, 7, 8))
    assert(FArray("a", "b", "c", "d").toList == List("a", "b", "c", "d")) // Ref kind
    assert(FArray(1L, 2L, 3L, 4L, 5L).toList == List(1L, 2L, 3L, 4L, 5L)) // Long kind
    // exercise the 16- and 32-arg overloads (boundary cases)
    val f16 = FArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    assert(f16.toList == (1 to 16).toList)
    val f32 = FArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)
    assert(f32.toList == (1 to 32).toList && f32.length == 32)
    val d16 = FArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
    assert(d16.toList == (1 to 16).map(_.toDouble).toList) // Double kind, 16-arg

  // dfsCB break + global-index + foldRight must be correct on EVERY node shape, not just a flat leaf.
  @Test def test_shortCircuit_trees(): Unit =
    val flat = (0 until 12).toList
    val shapes: List[(String, FArray[Int], List[Int])] = List(
      ("concat", FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5), flat),
      ("appendChain", { var a = FArray(0); for i <- 1 until 12 do a = a :+ i; a }, flat),
      ("prependChain", { var a = FArray(11); for i <- (0 until 11).reverse do a = i +: a; a }, flat),
      ("reverse", FArray.tabulate(12)(i => 11 - i).reverse, flat),
      ("slice", FArray.tabulate(16)(identity).slice(2, 14), (2 until 14).toList),
      // forward (slice) and backward (reverse) array-backed runs over a leaf base — exercises both onRun paths
      ("sliceLeaf", FArray.tabulate(20)(identity).slice(3, 15), (3 until 15).toList),
      ("reverseLeaf", FArray.tabulate(20)(identity).slice(0, 12).reverse, (0 until 12).reverse.toList),
      ("nested", ((FArray(0, 1) ++ FArray(2)) :+ 3) ++ (4 +: FArray(5, 6)), (0 to 6).toList),
      // reverse-of-TREE: a backward walk flips to FORWARD over the concat (exercises mutual Reverse recursion)
      ("reverseConcat", (FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5)).reverse, flat.reverse),
      ("reverseAppend", ({ var a = FArray(0); for i <- 1 until 12 do a = a :+ i; a }).reverse, flat.reverse),
      // nested reverse (reverse-of-reverse-of-tree); reverse-of-pad; reverse-of-updated
      ("revRevConcat", (FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5)).reverse.reverse, flat),
      ("reversePad", FArray.tabulate(8)(identity).padTo(12, 99).reverse, ((0 until 8).toList ::: List.fill(4)(99)).reverse),
      ("reverseUpdated", FArray.tabulate(12)(identity).updated(6, 6).reverse, flat.reverse)
    )
    for (name, fa, la) <- shapes do
      assertEquals(s"$name exists-hit", la.exists(_ == 7), fa.exists(_ == 7))
      assertEquals(s"$name exists-miss", la.exists(_ == 99), fa.exists(_ == 99))
      assertEquals(s"$name forall-t", la.forall(_ < 100), fa.forall(_ < 100))
      assertEquals(s"$name forall-f", la.forall(_ < 5), fa.forall(_ < 5))
      assertEquals(s"$name find", la.find(_ > 6), fa.find(_ > 6))
      assertEquals(s"$name indexWhere", la.indexWhere(_ == 6).toLong, fa.indexWhere(_ == 6).toLong)
      assertEquals(s"$name indexOf", la.indexOf(8).toLong, fa.indexOf(8).toLong)
      assertEquals(s"$name segmentLength", la.segmentLength(_ < 5, 0).toLong, fa.segmentLength(_ < 5).toLong)
      assertEquals(s"$name count", la.count(_ % 2 == 0).toLong, fa.count(_ % 2 == 0).toLong)
      assertEquals(s"$name contains", la.contains(9), fa.contains(9))
      assertEquals(s"$name sum", la.sum.toLong, fa.sum.toLong)
      assertEquals(s"$name foldLeft", la.foldLeft("")(_ + _.toString), fa.foldLeft("")(_ + _.toString))
      assertEquals(s"$name foldRight", la.foldRight("")(_.toString + _), fa.foldRight("")(_.toString + _))
      // unboxed self-kind accumulator (Int Z over Int input) across EVERY node shape — the new reduce traversers
      assertEquals(s"$name foldLeftInt", la.foldLeft(100)(_ - _).toLong, fa.foldLeft(100)(_ - _).toLong)
      assertEquals(s"$name foldRightInt", la.foldRight(100)(_ - _).toLong, fa.foldRight(100)(_ - _).toLong)
      // the rest of the Reduce family wired onto the same engine, across EVERY node shape
      assertEquals(s"$name product", la.map(_ % 3 + 1).product.toLong, fa.map(_ % 3 + 1).product.toLong)
      assertEquals(s"$name mkString", la.mkString("[", ",", "]"), fa.mkString("[", ",", "]"))
      if la.nonEmpty then assertEquals(s"$name min", la.min.toLong, fa.min.toLong)
      if la.nonEmpty then assertEquals(s"$name max", la.max.toLong, fa.max.toLong)
      if la.nonEmpty then assertEquals(s"$name reduceLeft", la.reduceLeft(_ - _).toLong, fa.reduceLeft(_ - _).toLong)
      assertEquals(s"$name map", la.map(_.toString), fa.map(_.toString).toList)
      assertEquals(s"$name lastIndexWhere", la.lastIndexWhere(_ == 6).toLong, fa.lastIndexWhere(_ == 6).toLong)
      assertEquals(s"$name lastIndexWhere-miss", la.lastIndexWhere(_ == 99).toLong, fa.lastIndexWhere(_ == 99).toLong)
      assertEquals(s"$name lastIndexOf", la.lastIndexOf(6).toLong, fa.lastIndexOf(6).toLong)
      assertEquals(s"$name lastIndexOf-miss", la.lastIndexOf(99).toLong, fa.lastIndexOf(99).toLong)
      if la.nonEmpty then assertEquals(s"$name reduceRight", la.reduceRight(_ - _).toLong, fa.reduceRight(_ - _).toLong)
      assertEquals(s"$name zip", la.zip(la), fa.zip(fa).toList)
      assertEquals(s"$name zipWithIndex", la.zipWithIndex, fa.zipWithIndex.toList)
      assertEquals(s"$name corresponds", la.corresponds(la)(_ == _), fa.corresponds(fa)(_ == _))
      assertEquals(s"$name startsWith", la.startsWith(la.take(3)), fa.startsWith(fa.take(3)))
      assertEquals(s"$name unzip", la.map(x => (x, -x)).unzip._2, fa.map(x => (x, -x)).unzip._2.toList)

  // TRAPS for the ShortCircuit shape: empty input, hit at boundary 0 / length-1 / no-hit, from/end past both ends,
  // forall-all-true, and index arithmetic across a ReverseNode + a Concat boundary. Every answer is checked vs List.
  @Test def test_shortCircuit_traps(): Unit =
    // empty input
    val e: FArray[Int] = FArray.empty[Int]
    val el: List[Int] = Nil
    assertEquals("empty exists", el.exists(_ == 0), e.exists(_ == 0))
    assertEquals("empty forall", el.forall(_ == 0), e.forall(_ == 0))
    assertEquals("empty find", el.find(_ == 0), e.find(_ == 0))
    assertEquals("empty indexWhere", el.indexWhere(_ == 0).toLong, e.indexWhere(_ == 0).toLong)
    assertEquals("empty indexOf", el.indexOf(0).toLong, e.indexOf(0).toLong)
    assertEquals("empty contains", el.contains(0), e.contains(0))
    assertEquals("empty lastIndexWhere", el.lastIndexWhere(_ == 0).toLong, e.lastIndexWhere(_ == 0).toLong)
    assertEquals("empty lastIndexOf", el.lastIndexOf(0).toLong, e.lastIndexOf(0).toLong)
    assertEquals("empty segmentLength", el.segmentLength(_ == 0).toLong, e.segmentLength(_ == 0).toLong)
    assertEquals("empty prefixLength(takeWhile)", el.takeWhile(_ == 0), e.takeWhile(_ == 0).toList)
    assertEquals("empty collectFirst", el.collectFirst { case x if x > 0 => x }, e.collectFirst { case x if x > 0 => x })

    // a flat leaf and a ReverseNode-of-Concat (forces index arithmetic across BOTH a reverse and a concat seam).
    val flatF = FArray.tabulate(12)(identity) // 0..11
    val flatL = (0 until 12).toList
    val revConcatF = (FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5)).reverse // 11,10,...,0
    val revConcatL = flatL.reverse
    val cases: List[(String, FArray[Int], List[Int])] =
      List(("flat", flatF, flatL), ("revConcat", revConcatF, revConcatL))
    for (n, fa, la) <- cases do
      // hit at index 0
      assertEquals(s"$n iw@0", la.indexWhere(_ == la.head).toLong, fa.indexWhere(_ == la.head).toLong)
      assertEquals(s"$n exists@0", la.exists(_ == la.head), fa.exists(_ == la.head))
      // hit at index length-1
      assertEquals(s"$n iw@last", la.indexWhere(_ == la.last).toLong, fa.indexWhere(_ == la.last).toLong)
      assertEquals(s"$n liw@last", la.lastIndexWhere(_ == la.last).toLong, fa.lastIndexWhere(_ == la.last).toLong)
      // no hit
      assertEquals(s"$n iw-miss", la.indexWhere(_ == 999).toLong, fa.indexWhere(_ == 999).toLong)
      assertEquals(s"$n liw-miss", la.lastIndexWhere(_ == 999).toLong, fa.lastIndexWhere(_ == 999).toLong)
      assertEquals(s"$n forall-true", la.forall(_ < 100), fa.forall(_ < 100))
      // from/end past both ends (negative and > length) across the reverse+concat boundary
      for from <- List(-5, 0, 3, 6, 11, 12, 50) do
        assertEquals(s"$n iw from=$from", la.indexWhere(_ % 2 == 0, from).toLong, fa.indexWhere(_ % 2 == 0, from).toLong)
        assertEquals(s"$n indexOf from=$from", la.indexOf(6, from).toLong, fa.indexOf(6, from).toLong)
        assertEquals(s"$n segLen from=$from", la.segmentLength(_ < 8, from).toLong, fa.segmentLength(_ < 8, from).toLong)
      for end <- List(-5, 0, 3, 6, 11, 12, 50) do
        assertEquals(s"$n liw end=$end", la.lastIndexWhere(_ % 2 == 0, end).toLong, fa.lastIndexWhere(_ % 2 == 0, end).toLong)
        assertEquals(s"$n lastIndexOf end=$end", la.lastIndexOf(6, end).toLong, fa.lastIndexOf(6, end).toLong)
      // every element index probed forward and backward (full boundary sweep)
      for v <- la do
        assertEquals(s"$n iw=$v", la.indexWhere(_ == v).toLong, fa.indexWhere(_ == v).toLong)
        assertEquals(s"$n liw=$v", la.lastIndexWhere(_ == v).toLong, fa.lastIndexWhere(_ == v).toLong)
        assertEquals(s"$n find=$v", la.find(_ == v), fa.find(_ == v))

  // BUILD-FILTERED shape (filter/filterNot) + the prefix wrappers (takeWhile/dropWhile/span) over GENUINE
  // STRUCTURAL node shapes — built via reverse/++/take/drop/padTo/updated, NOT fromIterable (whose flat-leaf
  // output would HIDE the over-a-ReverseNode write-order bug the scan fix taught us about). The critical case:
  // filter over a REVERSED structure must keep the SAME kept elements in REVERSED order — the buildFiltered
  // traverser flips the VISIT at each ReverseNode while the WRITE stays forward out[o++]. Int kind here; the Ref
  // kind is the next test. Every answer is checked vs the equivalent List op.
  @Test def test_buildFiltered_trees_Int(): Unit =
    val flat = (0 until 12).toList
    // shapes built STRUCTURALLY (each constructs a non-leaf node), with the List the SAME structural ops produce.
    val shapes: List[(String, FArray[Int], List[Int])] = List(
      ("concat", FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5), flat),
      ("appendChain", { var a = FArray(0); for i <- 1 until 12 do a = a :+ i; a }, flat),
      ("prependChain", { var a = FArray(11); for i <- (0 until 11).reverse do a = i +: a; a }, flat),
      ("reverse", FArray.tabulate(12)(identity).reverse, flat.reverse),
      ("reverseConcat", (FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5)).reverse, flat.reverse),
      ("reverseAppend", ({ var a = FArray(0); for i <- 1 until 12 do a = a :+ i; a }).reverse, flat.reverse),
      ("doubleReverse", FArray.tabulate(12)(identity).reverse.reverse, flat),
      ("revRevConcat", (FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5)).reverse.reverse, flat),
      // deep-base Slice (slice over a 14-elem Concat base — non-leaf base => applyBoxed arm in the traverser)
      ("sliceDeep", (FArray.tabulate(5)(identity) ++ FArray.tabulate(9)(_ + 5)).slice(2, 13), (0 until 14).toList.slice(2, 13)),
      ("reverseSliceDeep", (FArray.tabulate(5)(identity) ++ FArray.tabulate(9)(_ + 5)).slice(2, 13).reverse, (0 until 14).toList.slice(2, 13).reverse),
      // deep-base Pad (padTo over a Concat base) and its reverse
      ("padDeep", (FArray.tabulate(4)(identity) ++ FArray.tabulate(4)(_ + 4)).padTo(12, 99), (0 until 8).toList ::: List.fill(4)(99)),
      ("reversePadDeep", (FArray.tabulate(4)(identity) ++ FArray.tabulate(4)(_ + 4)).padTo(12, 99).reverse, ((0 until 8).toList ::: List.fill(4)(99)).reverse),
      // deep-base Updated (updated over a Concat base) and its reverse
      ("updatedDeep", (FArray.tabulate(6)(identity) ++ FArray.tabulate(6)(_ + 6)).updated(6, 600), flat.updated(6, 600)),
      ("reverseUpdatedDeep", (FArray.tabulate(6)(identity) ++ FArray.tabulate(6)(_ + 6)).updated(6, 600).reverse, flat.updated(6, 600).reverse)
    )
    for (name, fa, la) <- shapes do
      // predicates of varying selectivity, including all-keep / none-keep (identity-shortcut & Empty edges)
      for (pn, p) <- List[(String, Int => Boolean)](
          ("even", _ % 2 == 0),
          ("gt5", _ > 5),
          ("all", _ => true),
          ("none", _ => false),
          ("eq0", _ == la.headOption.getOrElse(-1)),
          ("multi3", _ % 3 == 0)
        )
      do
        assertEquals(s"$name filter $pn", la.filter(p), fa.filter(p).toList)
        assertEquals(s"$name filterNot $pn", la.filterNot(p), fa.filterNot(p).toList)
        assertCanonical(fa.filter(p), s"$name filter $pn canon")
        assertCanonical(fa.filterNot(p), s"$name filterNot $pn canon")
      // prefix wrappers (takeWhile/dropWhile/span) over the SAME structural shapes
      for (wn, w) <- List[(String, Int => Boolean)](
          ("ltLast", _ < la.lastOption.getOrElse(0)),
          ("ge0", _ >= 0),
          ("falseHead", _ => false),
          ("lt6", _ < 6)
        )
      do
        assertEquals(s"$name takeWhile $wn", la.takeWhile(w), fa.takeWhile(w).toList)
        assertEquals(s"$name dropWhile $wn", la.dropWhile(w), fa.dropWhile(w).toList)
        assertEquals(s"$name span._1 $wn", la.span(w)._1, fa.span(w)._1.toList)
        assertEquals(s"$name span._2 $wn", la.span(w)._2, fa.span(w)._2.toList)

  // Same structural coverage for the Ref (String) element kind — exercises filterLeafRef + the buildFilteredRef
  // traverser (Object[] leaves, applyBoxed deep bases) and the Ref prefix wrappers. Built STRUCTURALLY.
  @Test def test_buildFiltered_trees_Ref(): Unit =
    def s(i: Int): String = i.toString
    val flat = (0 until 12).map(s).toList
    val shapes: List[(String, FArray[String], List[String])] = List(
      ("concat", FArray.tabulate(5)(s) ++ FArray.tabulate(7)(i => s(i + 5)), flat),
      ("reverse", FArray.tabulate(12)(s).reverse, flat.reverse),
      ("reverseConcat", (FArray.tabulate(5)(s) ++ FArray.tabulate(7)(i => s(i + 5))).reverse, flat.reverse),
      ("doubleReverse", FArray.tabulate(12)(s).reverse.reverse, flat),
      ("appendChain", { var a = FArray(s(0)); for i <- 1 until 12 do a = a :+ s(i); a }, flat),
      ("sliceDeep", (FArray.tabulate(5)(s) ++ FArray.tabulate(9)(i => s(i + 5))).slice(2, 13), (0 until 14).map(s).toList.slice(2, 13)),
      ("reverseSliceDeep", (FArray.tabulate(5)(s) ++ FArray.tabulate(9)(i => s(i + 5))).slice(2, 13).reverse, (0 until 14).map(s).toList.slice(2, 13).reverse),
      ("padDeep", (FArray.tabulate(4)(s) ++ FArray.tabulate(4)(i => s(i + 4))).padTo(12, "Z"), (0 until 8).map(s).toList ::: List.fill(4)("Z")),
      (
        "reversePadDeep",
        (FArray.tabulate(4)(s) ++ FArray.tabulate(4)(i => s(i + 4))).padTo(12, "Z").reverse,
        ((0 until 8).map(s).toList ::: List.fill(4)("Z")).reverse
      ),
      ("updatedDeep", (FArray.tabulate(6)(s) ++ FArray.tabulate(6)(i => s(i + 6))).updated(6, "X"), flat.updated(6, "X")),
      ("reverseUpdatedDeep", (FArray.tabulate(6)(s) ++ FArray.tabulate(6)(i => s(i + 6))).updated(6, "X").reverse, flat.updated(6, "X").reverse)
    )
    for (name, fa, la) <- shapes do
      for (pn, p) <- List[(String, String => Boolean)](
          ("len1", _.length == 1),
          ("all", _ => true),
          ("none", _ => false),
          ("eqHead", _ == la.headOption.getOrElse("")),
          ("hasZ", _.contains("Z"))
        )
      do
        assertEquals(s"$name filter $pn", la.filter(p), fa.filter(p).toList)
        assertEquals(s"$name filterNot $pn", la.filterNot(p), fa.filterNot(p).toList)
        assertCanonical(fa.filter(p), s"$name filter $pn canon")
      for (wn, w) <- List[(String, String => Boolean)](
          ("len1", _.length == 1),
          ("always", _ => true),
          ("falseHead", _ => false)
        )
      do
        assertEquals(s"$name takeWhile $wn", la.takeWhile(w), fa.takeWhile(w).toList)
        assertEquals(s"$name dropWhile $wn", la.dropWhile(w), fa.dropWhile(w).toList)
        assertEquals(s"$name span._1 $wn", la.span(w)._1, fa.span(w)._1.toList)
        assertEquals(s"$name span._2 $wn", la.span(w)._2, fa.span(w)._2.toList)

  // Structural-shape parity for the DUAL-output partition / collect / partitionMap shapes (the BuildFiltered family
  // continued). The CRITICAL cases are over a ReverseNode: partition/collect/partitionMap over a reversed structure
  // must keep the SAME split/kept elements in REVERSED order (the partitionFwd traverser flips the VISIT at each
  // ReverseNode while BOTH writes stay forward). Int kind here; Ref kind in the next test. Every answer is checked
  // vs the equivalent List op, AND both outputs are asserted canonical.
  @Test def test_partition_collect_partitionMap_trees_Int(): Unit =
    val flat = (0 until 12).toList
    val shapes: List[(String, FArray[Int], List[Int])] = List(
      ("concat", FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5), flat),
      ("appendChain", { var a = FArray(0); for i <- 1 until 12 do a = a :+ i; a }, flat),
      ("prependChain", { var a = FArray(11); for i <- (0 until 11).reverse do a = i +: a; a }, flat),
      ("reverse", FArray.tabulate(12)(identity).reverse, flat.reverse),
      ("reverseConcat", (FArray.tabulate(5)(identity) ++ FArray.tabulate(7)(_ + 5)).reverse, flat.reverse),
      ("reverseAppend", ({ var a = FArray(0); for i <- 1 until 12 do a = a :+ i; a }).reverse, flat.reverse),
      ("doubleReverse", FArray.tabulate(12)(identity).reverse.reverse, flat),
      ("sliceDeep", (FArray.tabulate(5)(identity) ++ FArray.tabulate(9)(_ + 5)).slice(2, 13), (0 until 14).toList.slice(2, 13)),
      ("reverseSliceDeep", (FArray.tabulate(5)(identity) ++ FArray.tabulate(9)(_ + 5)).slice(2, 13).reverse, (0 until 14).toList.slice(2, 13).reverse),
      ("padDeep", (FArray.tabulate(4)(identity) ++ FArray.tabulate(4)(_ + 4)).padTo(12, 99), (0 until 8).toList ::: List.fill(4)(99)),
      ("reversePadDeep", (FArray.tabulate(4)(identity) ++ FArray.tabulate(4)(_ + 4)).padTo(12, 99).reverse, ((0 until 8).toList ::: List.fill(4)(99)).reverse),
      ("updatedDeep", (FArray.tabulate(6)(identity) ++ FArray.tabulate(6)(_ + 6)).updated(6, 600), flat.updated(6, 600)),
      ("reverseUpdatedDeep", (FArray.tabulate(6)(identity) ++ FArray.tabulate(6)(_ + 6)).updated(6, 600).reverse, flat.updated(6, 600).reverse)
    )
    for (name, fa, la) <- shapes do
      // partition: vary selectivity incl. all-keep / none-keep (the reuse-xs / Empty edges on BOTH outputs)
      for (pn, p) <- List[(String, Int => Boolean)](
          ("even", _ % 2 == 0),
          ("gt5", _ > 5),
          ("all", _ => true),
          ("none", _ => false),
          ("multi3", _ % 3 == 0)
        )
      do
        assertEquals(s"$name partition._1 $pn", la.partition(p)._1, fa.partition(p)._1.toList)
        assertEquals(s"$name partition._2 $pn", la.partition(p)._2, fa.partition(p)._2.toList)
        assertCanonical(fa.partition(p)._1, s"$name partition._1 $pn canon")
        assertCanonical(fa.partition(p)._2, s"$name partition._2 $pn canon")
      // collect: PF whose isDefinedAt filters AND whose body maps (output kind differs: Int -> String, Int -> Long)
      val pfStr: PartialFunction[Int, String] = { case x if x % 2 == 0 => "v" + x }
      assertEquals(s"$name collect->String", la.collect(pfStr), fa.collect(pfStr).toList)
      assertCanonical(fa.collect(pfStr), s"$name collect->String canon")
      val pfLong: PartialFunction[Int, Long] = { case x if x > 5 => x.toLong * 10 }
      assertEquals(s"$name collect->Long", la.collect(pfLong), fa.collect(pfLong).toList)
      assertCanonical(fa.collect(pfLong), s"$name collect->Long canon")
      val pfNone: PartialFunction[Int, Int] = { case x if x < 0 => x }
      assertEquals(s"$name collect none", la.collect(pfNone), fa.collect(pfNone).toList)
      val pfAll: PartialFunction[Int, Int] = { case x => x + 1 }
      assertEquals(s"$name collect all", la.collect(pfAll), fa.collect(pfAll).toList)
      // partitionMap: Left/Right of DIFFERENT kinds (Left:Int, Right:String); plus an all-Left and all-Right f
      val em: Int => Either[Int, String] = x => if x % 2 == 0 then Left(x * 2) else Right("r" + x)
      assertEquals(s"$name partitionMap._1", la.partitionMap(em)._1, fa.partitionMap(em)._1.toList)
      assertEquals(s"$name partitionMap._2", la.partitionMap(em)._2, fa.partitionMap(em)._2.toList)
      assertCanonical(fa.partitionMap(em)._1, s"$name partitionMap._1 canon")
      assertCanonical(fa.partitionMap(em)._2, s"$name partitionMap._2 canon")
      val emL: Int => Either[Int, String] = x => Left(x)
      assertEquals(s"$name partitionMap allLeft._1", la.partitionMap(emL)._1, fa.partitionMap(emL)._1.toList)
      assertEquals(s"$name partitionMap allLeft._2", la.partitionMap(emL)._2, fa.partitionMap(emL)._2.toList)

  // Same dual-output structural coverage for the Ref (String) element kind.
  @Test def test_partition_collect_partitionMap_trees_Ref(): Unit =
    def s(i: Int): String = i.toString
    val flat = (0 until 12).map(s).toList
    val shapes: List[(String, FArray[String], List[String])] = List(
      ("concat", FArray.tabulate(5)(s) ++ FArray.tabulate(7)(i => s(i + 5)), flat),
      ("reverse", FArray.tabulate(12)(s).reverse, flat.reverse),
      ("reverseConcat", (FArray.tabulate(5)(s) ++ FArray.tabulate(7)(i => s(i + 5))).reverse, flat.reverse),
      ("doubleReverse", FArray.tabulate(12)(s).reverse.reverse, flat),
      ("appendChain", { var a = FArray(s(0)); for i <- 1 until 12 do a = a :+ s(i); a }, flat),
      ("sliceDeep", (FArray.tabulate(5)(s) ++ FArray.tabulate(9)(i => s(i + 5))).slice(2, 13), (0 until 14).map(s).toList.slice(2, 13)),
      ("reverseSliceDeep", (FArray.tabulate(5)(s) ++ FArray.tabulate(9)(i => s(i + 5))).slice(2, 13).reverse, (0 until 14).map(s).toList.slice(2, 13).reverse),
      ("padDeep", (FArray.tabulate(4)(s) ++ FArray.tabulate(4)(i => s(i + 4))).padTo(12, "Z"), (0 until 8).map(s).toList ::: List.fill(4)("Z")),
      ("updatedDeep", (FArray.tabulate(6)(s) ++ FArray.tabulate(6)(i => s(i + 6))).updated(6, "X"), flat.updated(6, "X")),
      ("reverseUpdatedDeep", (FArray.tabulate(6)(s) ++ FArray.tabulate(6)(i => s(i + 6))).updated(6, "X").reverse, flat.updated(6, "X").reverse)
    )
    for (name, fa, la) <- shapes do
      for (pn, p) <- List[(String, String => Boolean)](
          ("len1", _.length == 1),
          ("all", _ => true),
          ("none", _ => false),
          ("hasZ", _.contains("Z"))
        )
      do
        assertEquals(s"$name partition._1 $pn", la.partition(p)._1, fa.partition(p)._1.toList)
        assertEquals(s"$name partition._2 $pn", la.partition(p)._2, fa.partition(p)._2.toList)
        assertCanonical(fa.partition(p)._1, s"$name partition._1 $pn canon")
        assertCanonical(fa.partition(p)._2, s"$name partition._2 $pn canon")
      // collect: Ref -> Int (output kind differs) and Ref -> Ref
      val pfLen: PartialFunction[String, Int] = { case x if x.length == 1 => x.length }
      assertEquals(s"$name collect->Int", la.collect(pfLen), fa.collect(pfLen).toList)
      assertCanonical(fa.collect(pfLen), s"$name collect->Int canon")
      val pfUp: PartialFunction[String, String] = { case x if x.length == 1 => x + "!" }
      assertEquals(s"$name collect->Ref", la.collect(pfUp), fa.collect(pfUp).toList)
      assertCanonical(fa.collect(pfUp), s"$name collect->Ref canon")
      // partitionMap: Left:Int, Right:String (different kinds). Key on length (no numeric-parse assumption)
      val em: String => Either[Int, String] = x => if x.length == 1 then Left(x.length) else Right(x + "*")
      assertEquals(s"$name partitionMap._1", la.partitionMap(em)._1, fa.partitionMap(em)._1.toList)
      assertEquals(s"$name partitionMap._2", la.partitionMap(em)._2, fa.partitionMap(em)._2.toList)
      assertCanonical(fa.partitionMap(em)._1, s"$name partitionMap._1 canon")
      assertCanonical(fa.partitionMap(em)._2, s"$name partitionMap._2 canon")

  // ---- fused pipelines (xs.fuse.…) ----
  @Test def test_fuse_identity_int: Unit =
    org.junit.Assert.assertEquals(FArray(1, 2, 3, 4, 5).toList, FArray(1, 2, 3, 4, 5).fuse.toFArray.toList)
  @Test def test_fuse_identity_empty: Unit =
    org.junit.Assert.assertEquals(FArray.empty[Int].toList, FArray.empty[Int].fuse.toFArray.toList)
  @Test def test_fuse_identity_ref: Unit =
    org.junit.Assert.assertEquals(FArray("a", "b", "c").toList, FArray("a", "b", "c").fuse.toFArray.toList)
  @Test def test_fuse_map_int: Unit =
    org.junit.Assert.assertEquals(List(2, 3, 4, 5, 6), FArray(1, 2, 3, 4, 5).fuse.map(_ + 1).toFArray.toList)
  @Test def test_fuse_filter_int: Unit =
    org.junit.Assert.assertEquals(List(2, 4), FArray(1, 2, 3, 4, 5).fuse.filter(_ % 2 == 0).toFArray.toList)
  @Test def test_fuse_map_filter_map_int: Unit =
    org.junit.Assert.assertEquals(
      FArray(1, 2, 3, 4, 5, 6).map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toList,
      FArray(1, 2, 3, 4, 5, 6).fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toFArray.toList)
  @Test def test_fuse_map_changes_kind: Unit = // Int -> String (Ref output)
    org.junit.Assert.assertEquals(List("1!", "2!", "3!"), FArray(1, 2, 3).fuse.map(i => s"$i!").toFArray.toList)
  @Test def test_fuse_ref_map_filter: Unit =
    org.junit.Assert.assertEquals(List("AA", "BBBB"),
      FArray("a", "bb", "b").fuse.filter(_.length <= 2).map(_.toUpperCase).filter(_ != "B").map(s => s + s).toFArray.toList)
  @Test def test_fuse_filter_all_out: Unit =
    org.junit.Assert.assertEquals(List(), FArray(1, 2, 3).fuse.filter(_ > 100).toFArray.toList)
  @Test def test_fuse_long_double: Unit =
    org.junit.Assert.assertEquals(List(4L, 8L), FArray(1L, 2L, 3L, 4L).fuse.filter(_ % 2L == 0L).map(_ * 2L).toFArray.toList)
  @Test def test_fuse_tree_source: Unit = // non-leaf source (Concat) exercises the <kind>At fallback
    org.junit.Assert.assertEquals(List(20, 40, 60),
      (FArray(1, 2, 3) ++ FArray(4, 5, 6)).fuse.filter(_ % 2 == 0).map(_ * 10).toFArray.toList)

  // ---- take / drop (positional) ----
  private val r10 = FArray.tabulate(10)(i => i)         // 0..9
  private val l10 = (0 until 10).toList
  @Test def test_fuse_take: Unit =
    org.junit.Assert.assertEquals(l10.take(3), r10.fuse.take(3).toFArray.toList)
  @Test def test_fuse_drop: Unit =
    org.junit.Assert.assertEquals(l10.drop(3), r10.fuse.drop(3).toFArray.toList)
  @Test def test_fuse_take0: Unit =
    org.junit.Assert.assertEquals(l10.take(0), r10.fuse.take(0).toFArray.toList)
  @Test def test_fuse_take_over: Unit =
    org.junit.Assert.assertEquals(l10.take(99), r10.fuse.take(99).toFArray.toList)
  @Test def test_fuse_drop_over: Unit =
    org.junit.Assert.assertEquals(l10.drop(99), r10.fuse.drop(99).toFArray.toList)
  @Test def test_fuse_take_neg: Unit =
    org.junit.Assert.assertEquals(l10.take(-1), r10.fuse.take(-1).toFArray.toList)
  @Test def test_fuse_drop_take: Unit =
    org.junit.Assert.assertEquals(l10.drop(2).take(3), r10.fuse.drop(2).take(3).toFArray.toList)
  @Test def test_fuse_filter_then_take: Unit = // take bounds the POST-filter stream (real fuse.take)
    org.junit.Assert.assertEquals(l10.filter(_ % 2 == 0).take(2).map(_ * 10),
      r10.fuse.filter(_ % 2 == 0).take(2).map(_ * 10).toFArray.toList)
  @Test def test_fuse_take_then_filter: Unit = // take FIRST (first 5), then filter
    org.junit.Assert.assertEquals(l10.take(5).filter(_ % 2 == 0),
      r10.fuse.take(5).filter(_ % 2 == 0).toFArray.toList)
  @Test def test_fuse_map_filter_take_map: Unit =
    org.junit.Assert.assertEquals(l10.map(_ + 1).filter(_ % 2 == 0).take(2).map(_ * 100),
      r10.fuse.map(_ + 1).filter(_ % 2 == 0).take(2).map(_ * 100).toFArray.toList)
  @Test def test_fuse_take_short_circuits: Unit = // take(3) on a 1e6 range must not read the whole thing
    org.junit.Assert.assertEquals(List(0, 1, 2), FArray.range(0, 1000000).fuse.take(3).toFArray.toList)

  // ---- terminals: foreach / foldLeft / count ----
  @Test def test_fuse_foreach: Unit =
    val sb = new StringBuilder
    r10.fuse.filter(_ % 2 == 0).map(_ + 1).foreach(x => sb.append(x).append(','))
    org.junit.Assert.assertEquals("1,3,5,7,9,", sb.toString)
  @Test def test_fuse_foldLeft: Unit =
    org.junit.Assert.assertEquals(l10.filter(_ % 2 == 0).map(_ * 2).sum,
      r10.fuse.filter(_ % 2 == 0).map(_ * 2).foldLeft(0)(_ + _))
  @Test def test_fuse_foldLeft_string: Unit = // Z is a reference accumulator
    org.junit.Assert.assertEquals("a-b-c-", FArray("a", "b", "c").fuse.foldLeft("")((acc, s) => acc + s + "-"))
  @Test def test_fuse_count: Unit =
    org.junit.Assert.assertEquals(l10.count(_ % 3 == 0), r10.fuse.filter(_ % 3 == 0).count)
  @Test def test_fuse_count_take: Unit =
    org.junit.Assert.assertEquals(2, r10.fuse.filter(_ % 2 == 0).take(2).count)

  // ---- derived terminals (sugar over the base terminals); each must match List ----
  @Test def test_fuse_derived_terminals: Unit =
    val r = FArray(1, 2, 3, 4, 5, 6); val l = List(1, 2, 3, 4, 5, 6)
    // conversions
    org.junit.Assert.assertEquals(l.filter(_ % 2 == 0).map(_ * 10), r.fuse.filter(_ % 2 == 0).map(_ * 10).toList)
    org.junit.Assert.assertEquals(l.map(_ + 1).toVector, r.fuse.map(_ + 1).toVector)
    org.junit.Assert.assertEquals(l.toSeq.toList, r.fuse.toSeq.toList)
    org.junit.Assert.assertEquals(l.filter(_ > 3).toSet, r.fuse.filter(_ > 3).toSet)
    org.junit.Assert.assertEquals(l.map(x => x -> x * 2).toMap, r.fuse.map(x => x -> x * 2).toMap)
    org.junit.Assert.assertEquals(l.map(_ + 1), r.fuse.map(_ + 1).toArray.toList)
    org.junit.Assert.assertEquals(l.filter(_ % 2 == 0).mkString("[", ",", "]"), r.fuse.filter(_ % 2 == 0).mkString("[", ",", "]"))
    org.junit.Assert.assertEquals(l.mkString("-"), r.fuse.mkString("-"))
    // reductions
    org.junit.Assert.assertEquals(l.filter(_ % 2 == 0).sum, r.fuse.filter(_ % 2 == 0).sum)
    org.junit.Assert.assertEquals(l.map(_ + 1).product, r.fuse.map(_ + 1).product)
    org.junit.Assert.assertEquals(l.fold(0)(_ + _), r.fuse.fold(0)(_ + _))
    org.junit.Assert.assertEquals(l.map(_ * 2).reduce(_ + _), r.fuse.map(_ * 2).reduce(_ + _))
    org.junit.Assert.assertEquals(l.map(_ * 2).min, r.fuse.map(_ * 2).min)
    org.junit.Assert.assertEquals(l.map(_ * 2).max, r.fuse.map(_ * 2).max)
    org.junit.Assert.assertEquals(l.minBy(x => -x), r.fuse.minBy(x => -x))
    org.junit.Assert.assertEquals(l.maxBy(x => -x), r.fuse.maxBy(x => -x))
    // last
    org.junit.Assert.assertEquals(l.filter(_ % 2 == 0).last, r.fuse.filter(_ % 2 == 0).last)
    org.junit.Assert.assertEquals(l.filter(_ > 100).lastOption, r.fuse.filter(_ > 100).lastOption)
    org.junit.Assert.assertEquals(l.lastOption, r.fuse.lastOption)
    // predicates / counts
    org.junit.Assert.assertEquals(l.contains(4), r.fuse.contains(4))
    org.junit.Assert.assertEquals(l.contains(99), r.fuse.contains(99))
    org.junit.Assert.assertEquals(l.filter(_ > 100).isEmpty, r.fuse.filter(_ > 100).isEmpty)
    org.junit.Assert.assertEquals(l.nonEmpty, r.fuse.nonEmpty)
    org.junit.Assert.assertEquals(l.filter(_ % 2 == 0).size, r.fuse.filter(_ % 2 == 0).size)
    org.junit.Assert.assertEquals(l.filter(_ % 2 == 0).length, r.fuse.filter(_ % 2 == 0).length)
    // positional / partial (short-circuit)
    org.junit.Assert.assertEquals(l.map(_ + 1).indexWhere(_ == 4), r.fuse.map(_ + 1).indexWhere(_ == 4))
    org.junit.Assert.assertEquals(l.indexWhere(_ > 100), r.fuse.indexWhere(_ > 100))
    org.junit.Assert.assertEquals(l.indexOf(4), r.fuse.indexOf(4))
    org.junit.Assert.assertEquals(l.collectFirst { case x if x > 3 => x * 10 }, r.fuse.collectFirst { case x if x > 3 => x * 10 })
    // empties throw like List
    org.junit.Assert.assertThrows(classOf[UnsupportedOperationException], () => FArray.empty[Int].fuse.reduce(_ + _))
    org.junit.Assert.assertThrows(classOf[NoSuchElementException], () => FArray.empty[Int].fuse.last)
    // Ref + Long kinds
    org.junit.Assert.assertEquals(List("a", "bb", "ccc").filter(_.length <= 2).mkString(","), FArray("a", "bb", "ccc").fuse.filter(_.length <= 2).mkString(","))
    org.junit.Assert.assertEquals(List(1L, 2L, 3L).sum, FArray(1L, 2L, 3L).fuse.sum)

  // ---- kind coverage for take/drop/terminals (Long / Double / Ref) ----
  @Test def test_fuse_long_take_fold: Unit =
    org.junit.Assert.assertEquals((0L until 10L).toList.drop(2).take(4).sum,
      FArray.tabulate(10)(i => i.toLong).fuse.drop(2).take(4).foldLeft(0L)(_ + _))
  @Test def test_fuse_double_pipeline: Unit =
    org.junit.Assert.assertEquals(List(2.0, 4.0),
      FArray(1.0, 2.0, 3.0, 4.0).fuse.filter(_ % 2.0 == 0.0).map(_ * 1.0).take(2).toFArray.toList)
  @Test def test_fuse_ref_take_drop_count: Unit =
    val ss = FArray("a", "bb", "ccc", "dddd", "e")
    org.junit.Assert.assertEquals(List("ccc", "dddd"), ss.fuse.drop(2).take(2).toFArray.toList)
    org.junit.Assert.assertEquals(3, ss.fuse.filter(_.length >= 2).count)
  @Test def test_fuse_ref_tree_take: Unit = // Ref non-leaf (Concat) + take short-circuit
    org.junit.Assert.assertEquals(List("A", "B"),
      (FArray("a", "b") ++ FArray("c", "d")).fuse.map(_.toUpperCase).take(2).toFArray.toList)
  @Test def test_fuse_long_chain: Unit = // 8+ stages must stay correct (and one method under the size limit)
    org.junit.Assert.assertEquals(
      l10.map(_ + 1).filter(_ % 2 == 0).map(_ * 3).filter(_ > 5).drop(1).map(_ - 1).take(3).filter(_ != 17).map(_ + 100),
      r10.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 3).filter(_ > 5).drop(1).map(_ - 1).take(3).filter(_ != 17).map(_ + 100).toFArray.toList)

  // ---- flatMap (nested loops) ----
  @Test def test_fuse_flatMap: Unit =
    org.junit.Assert.assertEquals(l10.flatMap(x => List(x, x * 10)),
      r10.fuse.flatMap(x => FArray(x, x * 10)).toFArray.toList)
  @Test def test_fuse_flatMap_expands: Unit = // output bigger than source -> growable
    org.junit.Assert.assertEquals(l10.flatMap(x => List.fill(x)(x)),
      r10.fuse.flatMap(x => FArray.tabulate(x)(_ => x)).toFArray.toList)
  @Test def test_fuse_flatMap_empty_inners: Unit =
    org.junit.Assert.assertEquals(l10.flatMap(x => if x % 2 == 0 then List(x) else Nil),
      r10.fuse.flatMap(x => if x % 2 == 0 then FArray(x) else FArray.empty[Int]).toFArray.toList)
  @Test def test_fuse_map_flatMap_filter: Unit =
    org.junit.Assert.assertEquals(
      l10.map(_ + 1).flatMap(x => List(x, -x)).filter(_ > 0).map(_ * 2),
      r10.fuse.map(_ + 1).flatMap(x => FArray(x, -x)).filter(_ > 0).map(_ * 2).toFArray.toList)
  @Test def test_fuse_nested_flatMap: Unit =
    org.junit.Assert.assertEquals(
      List(1, 2, 3).flatMap(x => List(x, x).flatMap(y => List(y, y * 100))),
      FArray(1, 2, 3).fuse.flatMap(x => FArray(x, x)).flatMap(y => FArray(y, y * 100)).toFArray.toList)
  @Test def test_fuse_flatMap_kind_change: Unit = // Int -> String inner
    org.junit.Assert.assertEquals(List("1", "x", "2", "x"),
      FArray(1, 2).fuse.flatMap(i => FArray(i.toString, "x")).toFArray.toList)
  @Test def test_fuse_flatMap_take: Unit = // take bounds the FLATTENED stream, breaks across nesting
    org.junit.Assert.assertEquals(List(0, 0, 1, 10, 2),
      r10.fuse.flatMap(x => FArray(x, x * 10)).take(5).toFArray.toList)
  @Test def test_fuse_flatMap_tree_source: Unit =
    org.junit.Assert.assertEquals(List(1, 1, 2, 2, 3, 3, 4, 4),
      (FArray(1, 2) ++ FArray(3, 4)).fuse.flatMap(x => FArray(x, x)).toFArray.toList)

  // ---- short-circuit terminals: find / exists / forall / headOption / head ----
  @Test def test_fuse_find: Unit =
    org.junit.Assert.assertEquals(l10.map(_ + 1).find(_ % 3 == 0),
      r10.fuse.map(_ + 1).find(_ % 3 == 0))
  @Test def test_fuse_find_none: Unit =
    org.junit.Assert.assertEquals(None, r10.fuse.find(_ > 100))
  @Test def test_fuse_exists: Unit =
    org.junit.Assert.assertEquals(l10.exists(_ == 7), r10.fuse.exists(_ == 7))
  @Test def test_fuse_forall: Unit =
    org.junit.Assert.assertEquals(l10.forall(_ < 100) && !l10.forall(_ < 5),
      r10.fuse.forall(_ < 100) && !r10.fuse.forall(_ < 5))
  @Test def test_fuse_headOption: Unit =
    org.junit.Assert.assertEquals(l10.filter(_ > 5).headOption, r10.fuse.filter(_ > 5).headOption)
  @Test def test_fuse_headOption_empty: Unit =
    org.junit.Assert.assertEquals(None, r10.fuse.filter(_ > 100).headOption)
  @Test def test_fuse_head: Unit =
    org.junit.Assert.assertEquals(l10.filter(_ > 5).head, r10.fuse.filter(_ > 5).head)
  @Test def test_fuse_head_empty_throws: Unit =
    org.junit.Assert.assertThrows(classOf[java.util.NoSuchElementException], () => r10.fuse.filter(_ > 100).head)
  @Test def test_fuse_find_across_flatMap: Unit = // short-circuit must escape the inner loop
    org.junit.Assert.assertEquals(Some(30),
      r10.fuse.flatMap(x => FArray(x * 10, x * 100)).find(_ == 30))
  @Test def test_fuse_find_ref: Unit =
    org.junit.Assert.assertEquals(Some("CC"),
      FArray("a", "bb", "ccc").fuse.map(_.toUpperCase).filter(_.length >= 2).find(_.startsWith("CC")).map(_.take(2)))

  // ---- adversarial: counters × flatMap nesting, done-break, kinds, short-circuit ----
  @Test def test_fuse_take_then_flatMap: Unit =
    org.junit.Assert.assertEquals(l10.take(2).flatMap(x => List(x, x)),
      r10.fuse.take(2).flatMap(x => FArray(x, x)).toFArray.toList)
  @Test def test_fuse_drop_then_flatMap: Unit =
    org.junit.Assert.assertEquals(l10.drop(8).flatMap(x => List(x, x * 10)),
      r10.fuse.drop(8).flatMap(x => FArray(x, x * 10)).toFArray.toList)
  @Test def test_fuse_flatMap_drop_take: Unit =
    org.junit.Assert.assertEquals(l10.flatMap(x => List(x, x * 10)).drop(2).take(3),
      r10.fuse.flatMap(x => FArray(x, x * 10)).drop(2).take(3).toFArray.toList)
  @Test def test_fuse_nested_flatMap_take: Unit = // done must break BOTH inner loops
    org.junit.Assert.assertEquals(
      List(1, 2, 3).flatMap(x => List(x, x).flatMap(y => List(y, y * 10))).take(3),
      FArray(1, 2, 3).fuse.flatMap(x => FArray(x, x)).flatMap(y => FArray(y, y * 10)).take(3).toFArray.toList)
  @Test def test_fuse_two_takes: Unit =
    org.junit.Assert.assertEquals(l10.take(5).map(_ + 1).take(2),
      r10.fuse.take(5).map(_ + 1).take(2).toFArray.toList)
  @Test def test_fuse_flatMap_take_boundary: Unit = // 4th element is the 2nd inner of the 2nd outer
    org.junit.Assert.assertEquals(List(0, 0, 0, 1).take(4),
      r10.fuse.flatMap(x => FArray(x, x, x)).take(4).toFArray.toList)
  @Test def test_fuse_flatMap_long: Unit =
    org.junit.Assert.assertEquals(List(1L, 10L, 2L, 20L),
      FArray(1, 2).fuse.flatMap(x => FArray(x.toLong, (x * 10).toLong)).toFArray.toList)
  @Test def test_fuse_flatMap_double_tree_inner: Unit = // inner is a Concat (non-leaf) -> <kind>At fallback
    org.junit.Assert.assertEquals(List(1.0, 2.0, 2.0, 4.0),
      FArray(1.0, 2.0).fuse.flatMap(x => FArray(x) ++ FArray(x * 2)).toFArray.toList)
  @Test def test_fuse_forall_short_circuits: Unit = // stop at first false
    org.junit.Assert.assertEquals(false, FArray(2, 4, 5, 6).fuse.forall(_ % 2 == 0))
  @Test def test_fuse_exists_first: Unit =
    org.junit.Assert.assertEquals(true, r10.fuse.map(_ * 2).exists(_ == 0)) // first element matches
  @Test def test_fuse_head_after_drop: Unit =
    org.junit.Assert.assertEquals(l10.drop(3).head, r10.fuse.drop(3).head)
  @Test def test_fuse_find_flatMap_empty_inners: Unit =
    org.junit.Assert.assertEquals(Some(99),
      r10.fuse.flatMap(x => if x == 3 then FArray(99) else FArray.empty[Int]).find(_ == 99))
  @Test def test_fuse_empty_source_terminals: Unit =
    val e = FArray.empty[Int]
    org.junit.Assert.assertEquals(None, e.fuse.headOption)
    org.junit.Assert.assertEquals(0, e.fuse.count)
    org.junit.Assert.assertEquals(true, e.fuse.forall(_ > 0))
    org.junit.Assert.assertEquals(false, e.fuse.exists(_ > 0))
    org.junit.Assert.assertEquals(List(), e.fuse.flatMap(x => FArray(x, x)).toFArray.toList)

  // ---- Layer B: compute-for-survivors (an expensive independent column computes only past the filter) ----
  @Test def test_fuse_compute_for_survivors: Unit =
    var cheapCalls = 0; var expCalls = 0
    def cheap(x: Int): Int = { cheapCalls += 1; x }
    def expensive(x: Int): Int = { expCalls += 1; x * 1000 }
    val result = FArray(1, 2, 3, 4, 5, 6).fuse
      .map(x => (cheap(x), expensive(x)))   // two independent columns
      .filter(_._1 % 2 == 0)                // uses only the cheap column; keeps 2,4,6
      .map(_._2)                            // uses only the expensive column
      .toFArray.toList
    org.junit.Assert.assertEquals(List(2000, 4000, 6000), result)
    org.junit.Assert.assertEquals("cheap runs for every element", 6, cheapCalls)
    org.junit.Assert.assertEquals("expensive runs ONLY for survivors (sunk past the filter)", 3, expCalls)
  @Test def test_fuse_no_recompute: Unit = // a column used twice binds once (no recomputation regression)
    var calls = 0
    def f(x: Int): Int = { calls += 1; x * 2 }
    val r = FArray(1, 2, 3).fuse.map(x => f(x)).map(y => y + y).toFArray.toList
    org.junit.Assert.assertEquals(List(4, 8, 12), r)
    org.junit.Assert.assertEquals("f bound once per element, not recomputed", 3, calls)
  @Test def test_fuse_cse_across_columns: Unit = // f(x) shared by two tuple components -> computed ONCE (List: twice)
    var fCalls = 0
    def f(x: Int): Int = { fCalls += 1; x * x }
    val r = FArray(1, 2, 3).fuse.map(x => (f(x) + 1, f(x) + 2)).map(t => t._1 + t._2).toFArray.toList
    org.junit.Assert.assertEquals(List(5, 11, 21), r) // (f+1)+(f+2) = 2*x*x + 3
    org.junit.Assert.assertEquals("f(x) CSE'd across both columns", 3, fCalls)
  @Test def test_fuse_cse_within_expr: Unit = // f(x) + f(x) within one expression -> computed once
    var c = 0
    def g(x: Int): Int = { c += 1; x + 10 }
    val r = FArray(1, 2).fuse.map(x => g(x) + g(x)).toFArray.toList
    org.junit.Assert.assertEquals(List(22, 24), r)
    org.junit.Assert.assertEquals("g(x) CSE'd within one expression", 2, c)
  @Test def test_fuse_cse_ref: Unit = // shared String sub-expression
    var c = 0
    def up(s: String): String = { c += 1; s.toUpperCase }
    val r = FArray("ab", "cd").fuse.map(s => up(s) + up(s)).toFArray.toList
    org.junit.Assert.assertEquals(List("ABAB", "CDCD"), r)
    org.junit.Assert.assertEquals("up(s) CSE'd once per element", 2, c)
  @Test def test_fuse_cse_nested_calls: Unit = // f(x) and g(f(x)) both shared
    var fc = 0; var gc = 0
    def f(x: Int): Int = { fc += 1; x + 1 }
    def g(y: Int): Int = { gc += 1; y * 2 }
    val r = FArray(1, 2).fuse.map(x => (g(f(x)), g(f(x)) + f(x))).map(t => t._1 + t._2).toFArray.toList
    org.junit.Assert.assertEquals(List(10, 15), r) // 2*g(f(x)) + f(x)
    org.junit.Assert.assertEquals("f(x) shared", 2, fc)
    org.junit.Assert.assertEquals("g(f(x)) shared", 2, gc)
  @Test def test_fuse_cse_sink: Unit = // a shared expr used only past the filter still sinks (computed for survivors)
    var e = 0
    def exp(x: Int): Int = { e += 1; x * 100 }
    val r = FArray(1, 2, 3, 4).fuse.map(x => (x, exp(x) + 1, exp(x) + 2)).filter(_._1 % 2 == 0).map(t => t._2 + t._3).toFArray.toList
    org.junit.Assert.assertEquals(List(2 * 200 + 3, 2 * 400 + 3), r) // survivors x=2,4
    org.junit.Assert.assertEquals("exp(x) CSE'd AND sunk: once per survivor", 2, e)

  // ---- Layer B adversarial: tuple decomposition / projection / fallback correctness (pure -> == List) ----
  @Test def test_fuse_lb_identity_map: Unit =
    org.junit.Assert.assertEquals(l10.map(x => x).filter(_ > 2), r10.fuse.map(x => x).filter(_ > 2).toFArray.toList)
  @Test def test_fuse_lb_const_map: Unit =
    org.junit.Assert.assertEquals(l10.map(_ => 7), r10.fuse.map(_ => 7).toFArray.toList)
  @Test def test_fuse_lb_filter_const: Unit =
    org.junit.Assert.assertEquals(l10.filter(_ => true).filter(_ => false), r10.fuse.filter(_ => true).filter(_ => false).toFArray.toList)
  @Test def test_fuse_lb_three_tuple: Unit =
    org.junit.Assert.assertEquals(l10.map(x => (x, x + 1, x + 2)).filter(_._2 % 2 == 0).map(t => t._1 + t._3),
      r10.fuse.map(x => (x, x + 1, x + 2)).filter(_._2 % 2 == 0).map(t => t._1 + t._3).toFArray.toList)
  @Test def test_fuse_lb_tuple_of_tuple: Unit =
    org.junit.Assert.assertEquals(
      l10.map(x => ((x, x + 1), x + 2)).map(t => t._1._1 + t._1._2 + t._2),
      r10.fuse.map(x => ((x, x + 1), x + 2)).map(t => t._1._1 + t._1._2 + t._2).toFArray.toList)
  @Test def test_fuse_lb_predicate_two_columns: Unit =
    org.junit.Assert.assertEquals(l10.map(x => (x, x * 2)).filter(t => t._1 + t._2 > 5).map(_._1),
      r10.fuse.map(x => (x, x * 2)).filter(t => t._1 + t._2 > 5).map(_._1).toFArray.toList)
  @Test def test_fuse_lb_same_proj_twice: Unit =
    org.junit.Assert.assertEquals(l10.map(x => (x, x)).filter(t => t._1 > 2 && t._1 < 8).map(_._2),
      r10.fuse.map(x => (x, x)).filter(t => t._1 > 2 && t._1 < 8).map(_._2).toFArray.toList)
  @Test def test_fuse_lb_swap_columns: Unit =
    org.junit.Assert.assertEquals(l10.map(x => (x, x + 100)).map(t => (t._2, t._1)).map(t => t._1 - t._2),
      r10.fuse.map(x => (x, x + 100)).map(t => (t._2, t._1)).map(t => t._1 - t._2).toFArray.toList)
  @Test def test_fuse_lb_whole_param_fallback: Unit = // predicate uses the whole tuple -> materialize fallback
    org.junit.Assert.assertEquals(l10.map(x => (x, x)).filter(t => t == (2, 2)).map(_._1),
      r10.fuse.map(x => (x, x)).filter(t => t == (2, 2)).map(_._1).toFArray.toList)
  @Test def test_fuse_lb_ref_columns: Unit =
    org.junit.Assert.assertEquals(
      List("a", "bb", "ccc").map(s => (s, s.length)).filter(_._2 >= 2).map(_._1),
      FArray("a", "bb", "ccc").fuse.map(s => (s, s.length)).filter(_._2 >= 2).map(_._1).toFArray.toList)
  @Test def test_fuse_lb_col_reused_two_maps: Unit =
    org.junit.Assert.assertEquals(l10.map(x => (x, x + 1)).filter(_._1 > 2).map(_._2).map(y => y * y),
      r10.fuse.map(x => (x, x + 1)).filter(_._1 > 2).map(_._2).map(y => y * y).toFArray.toList)
  @Test def test_fuse_lb_segment_into_flatMap: Unit = // tuple materialized at the flatMap boundary
    org.junit.Assert.assertEquals(l10.map(x => (x, x * 10)).filter(_._1 % 2 == 0).flatMap(t => List(t._1, t._2)),
      r10.fuse.map(x => (x, x * 10)).filter(_._1 % 2 == 0).flatMap(t => FArray(t._1, t._2)).toFArray.toList)
  @Test def test_fuse_lb_segment_into_zip: Unit =
    org.junit.Assert.assertEquals(l10.map(x => x + 1).filter(_ % 2 == 0).zipWithIndex,
      r10.fuse.map(x => x + 1).filter(_ % 2 == 0).zipWithIndex.toFArray.toList)

  // ---- no-blowup: a long fused chain stays correct AND under the JVM HugeMethodLimit (so it JIT-compiles) ----
  @Test def test_fuse_no_blowup: Unit =
    val xs = FArray.tabulate(40)(i => i - 5)
    val expected = xs.toList.map(_ + 1).filter(_ % 2 == 0).map(_ * 3).filter(_ > 0).drop(1).map(_ - 1)
      .take(50).filter(_ != 17).map(_ + 100).filter(_ < 1000).map(_ * 2).filter(_ % 4 == 0).map(_ / 2)
    org.junit.Assert.assertEquals(expected, FuseSize.longChain(xs))
    val sz = FuseSize.codeSizeOf("longChain")
    assert(sz > 0, "could not measure longChain bytecode size")
    assert(sz < 8000, s"longChain is $sz bytecodes — over the HugeMethodLimit (8000); the fused chain would run interpreted")

  // ---- fuzz: a matrix of fused pipelines over random leaf AND tree-shaped inputs, all == List ----
  @Test def test_fuse_fuzz: Unit =
    val rng = new java.util.Random(0xFA5EL)
    val zf = FArray(10, 20, 30, 40, 50, 60); val zl = List(10, 20, 30, 40, 50, 60) // zip operand
    // an FArray equal to `xs`, sometimes a tree (append-chain / Concat) to exercise the <kind>At fallback
    def mk(xs: List[Int]): FArray[Int] = rng.nextInt(3) match
      case 0 => FArray.fromIterable(xs)
      case 1 => xs.foldLeft(FArray.empty[Int])(_ :+ _)
      case _ => val (l, r) = xs.splitAt(xs.length / 2); FArray.fromIterable(l) ++ FArray.fromIterable(r)
    def check[X](label: String)(fa: FArray[Int] => X)(la: List[Int] => X): Unit =
      var t = 0
      while t < 250 do
        val xs = List.tabulate(rng.nextInt(22))(_ => rng.nextInt(16) - 4) // small range → dups/collisions/negatives
        val got = fa(mk(xs)); val exp = la(xs)
        assert(got == exp, s"$label on $xs: $got != $exp")
        t += 1
    check("map.filter.map")(_.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 3).toFArray.toList)(_.map(_ + 1).filter(_ % 2 == 0).map(_ * 3))
    check("filter.take")(_.fuse.filter(_ > 0).take(3).toFArray.toList)(_.filter(_ > 0).take(3))
    check("drop.take.map")(_.fuse.drop(2).take(4).map(_ * 2).toFArray.toList)(_.drop(2).take(4).map(_ * 2))
    check("take.filter.drop")(_.fuse.take(8).filter(_ % 2 == 0).drop(1).toFArray.toList)(_.take(8).filter(_ % 2 == 0).drop(1))
    check("flatMap")(_.fuse.flatMap(x => FArray(x, -x)).toFArray.toList)(_.flatMap(x => List(x, -x)))
    check("flatMap.filter.take")(_.fuse.flatMap(x => FArray(x, x * 2)).filter(_ > 0).take(5).toFArray.toList)(_.flatMap(x => List(x, x * 2)).filter(_ > 0).take(5))
    check("map.flatMap.map")(_.fuse.map(_ + 1).flatMap(x => FArray.tabulate(if x > 0 then 2 else 0)(_ => x)).map(_ * 10).toFArray.toList)(_.map(_ + 1).flatMap(x => List.fill(if x > 0 then 2 else 0)(x)).map(_ * 10))
    check("zipWithIndex.filter.map")(_.fuse.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).toFArray.toList)(_.zipWithIndex.filter(_._2 % 2 == 0).map(_._1))
    check("filter.zipWithIndex")(_.fuse.filter(_ > 0).zipWithIndex.toFArray.toList)(_.filter(_ > 0).zipWithIndex)
    check("zip")(_.fuse.zip(zf).toFArray.toList)(_.zip(zl))
    check("filter.zip.map")(_.fuse.filter(_ % 2 == 0).zip(zf).map((a, b) => a + b).toFArray.toList)(_.filter(_ % 2 == 0).zip(zl).map((a, b) => a + b))
    check("map2")(_.fuse.map2(zf)((a, b) => a * b).toFArray.toList)(_.zip(zl).map((a, b) => a * b))
    check("tuple.sink")(_.fuse.map(x => (x, x * x)).filter(_._1 % 2 == 0).map(_._2).toFArray.toList)(_.map(x => (x, x * x)).filter(_._1 % 2 == 0).map(_._2))
    check("cse")(_.fuse.map(x => (x * x + 1, x * x + 2)).map(t => t._1 + t._2).toFArray.toList)(_.map(x => (x * x + 1, x * x + 2)).map(t => t._1 + t._2))
    check("foldLeft")(_.fuse.filter(_ > 0).map(_ + 1).foldLeft(0)(_ + _))(_.filter(_ > 0).map(_ + 1).foldLeft(0)(_ + _))
    check("count")(_.fuse.filter(_ % 3 == 0).map(_ * 2).count)(_.filter(_ % 3 == 0).map(_ * 2).size)
    check("find")(_.fuse.map(_ + 1).find(_ % 4 == 0))(_.map(_ + 1).find(_ % 4 == 0))
    check("exists")(_.fuse.map(_ * 2).exists(_ == 8))(_.map(_ * 2).exists(_ == 8))
    check("forall")(_.fuse.filter(_ > -10).forall(_ < 100))(_.filter(_ > -10).forall(_ < 100))
    check("headOption")(_.fuse.filter(_ % 2 == 0).map(_ * 3).headOption)(_.filter(_ % 2 == 0).map(_ * 3).headOption)
    check("flatMap.zipWithIndex")(_.fuse.flatMap(x => FArray(x, x)).zipWithIndex.take(6).toFArray.toList)(_.flatMap(x => List(x, x)).zipWithIndex.take(6))
    // case classes / nested products
    check("cc.sink")(_.fuse.map(x => P2(x, x * x)).filter(_.a % 2 == 0).map(_.b).toFArray.toList)(_.filter(_ % 2 == 0).map(x => x * x))
    check("cc.dce")(_.fuse.map(x => P2(x, x * x)).filter(_.a > 0).map(_.a).toFArray.toList)(_.filter(_ > 0))
    check("cc.nested")(_.fuse.map(x => Outer(Inner(x, x + 1), x * 2)).filter(_.inner.x % 2 == 0).map(o => o.inner.y + o.z).toFArray.toList)(_.filter(_ % 2 == 0).map(x => (x + 1) + (x * 2)))
    check("cc.materialize")(_.fuse.map(x => P2(x, x * 10)).filter(_.a > 0).toFArray.toList)(_.filter(_ > 0).map(x => P2(x, x * 10)))
    // Long / Double / Ref element kinds
    var t2 = 0
    while t2 < 200 do
      val xs = List.tabulate(rng.nextInt(20))(_ => rng.nextInt(16) - 4)
      assert(FArray.fromIterable(xs.map(_.toLong)).fuse.filter(_ % 2L == 0L).map(_ * 3L).take(4).toFArray.toList == xs.map(_.toLong).filter(_ % 2 == 0).map(_ * 3).take(4))
      assert(FArray.fromIterable(xs.map(_.toDouble)).fuse.map(_ + 0.5).filter(_ > 0.0).toFArray.toList == xs.map(_.toDouble).map(_ + 0.5).filter(_ > 0.0))
      assert(FArray.fromIterable(xs.map(_.toString)).fuse.filter(_.length <= 2).map(s => (s, s.length)).filter(_._2 >= 1).map(_._1).toFArray.toList == xs.map(_.toString).filter(_.length <= 2).map(s => (s, s.length)).filter(_._2 >= 1).map(_._1))
      t2 += 1

  // ---- case-class / nested-product decomposition (same machinery as tuples, via caseFields) ----
  @Test def test_fuse_caseclass: Unit =
    org.junit.Assert.assertEquals(List(20, 40),
      FArray(1, 2, 3, 4).fuse.map(x => P2(x, x * 10)).filter(_.a % 2 == 0).map(_.b).toFArray.toList)
  @Test def test_fuse_caseclass_dce: Unit = // a discarded case-class field is never computed
    var e = 0; def exp(x: Int): Int = { e += 1; x * 100 }
    val r = FArray(1, 2, 3).fuse.map(x => P2(x, exp(x))).map(_.a).toFArray.toList
    org.junit.Assert.assertEquals(List(1, 2, 3), r)
    org.junit.Assert.assertEquals("the .b field (exp(x)) is dead → never computed", 0, e)
  @Test def test_fuse_caseclass_sink: Unit = // an expensive field used only past the filter → survivors only
    var e = 0; def exp(x: Int): Int = { e += 1; x * 100 }
    val r = FArray(1, 2, 3, 4).fuse.map(x => P2(x, exp(x))).filter(_.a % 2 == 0).map(_.b).toFArray.toList
    org.junit.Assert.assertEquals(List(200, 400), r)
    org.junit.Assert.assertEquals("exp(x) only for survivors", 2, e)
  @Test def test_fuse_caseclass_materialize: Unit = // FArray[P2] output — the product is rebuilt
    org.junit.Assert.assertEquals(List(P2(1, 10), P2(2, 20)),
      FArray(1, 2).fuse.map(x => P2(x, x * 10)).toFArray.toList)
  @Test def test_fuse_caseclass_nested: Unit = // Outer(Inner(...), ...), nested field access
    org.junit.Assert.assertEquals(List(4, 8, 12),
      FArray(1, 2, 3).fuse.map(x => Outer(Inner(x, x * 2), x * 3)).map(o => o.inner.x + o.z).toFArray.toList)
  @Test def test_fuse_caseclass_nested_dce: Unit = // a dead NESTED field (inner.y) is never computed
    var e = 0; def exp(x: Int): Int = { e += 1; x * 100 }
    val r = FArray(1, 2, 3).fuse.map(x => Outer(Inner(x, exp(x)), x * 3)).map(o => o.inner.x + o.z).toFArray.toList
    org.junit.Assert.assertEquals(List(4, 8, 12), r)
    org.junit.Assert.assertEquals("inner.y (exp(x)) is dead → never computed, even nested", 0, e)
  @Test def test_fuse_caseclass_whole: Unit = // case class used whole (rebuilt), then field
    org.junit.Assert.assertEquals(List(P2(2, 20)),
      FArray(1, 2, 3).fuse.map(x => P2(x, x * 10)).filter(p => p == P2(2, 20)).toFArray.toList)

  // ---- zipWithIndex ----
  @Test def test_fuse_zipWithIndex: Unit =
    org.junit.Assert.assertEquals(l10.zipWithIndex, r10.fuse.zipWithIndex.toFArray.toList)
  @Test def test_fuse_zipWithIndex_destructure: Unit = // tuple built then destructured (scalar-replaced)
    org.junit.Assert.assertEquals(l10.zipWithIndex.map((x, i) => x + i * 100),
      r10.fuse.zipWithIndex.map((x, i) => x + i * 100).toFArray.toList)
  @Test def test_fuse_filter_then_zipWithIndex: Unit = // index is the POST-filter position
    org.junit.Assert.assertEquals(l10.filter(_ % 2 == 0).zipWithIndex,
      r10.fuse.filter(_ % 2 == 0).zipWithIndex.toFArray.toList)
  @Test def test_fuse_zipWithIndex_then_filter: Unit = // index-only predicate
    org.junit.Assert.assertEquals(l10.zipWithIndex.filter(_._2 % 3 == 0).map(_._1),
      r10.fuse.zipWithIndex.filter(_._2 % 3 == 0).map(_._1).toFArray.toList)
  @Test def test_fuse_zipWithIndex_take: Unit =
    org.junit.Assert.assertEquals(l10.zipWithIndex.take(3),
      r10.fuse.zipWithIndex.take(3).toFArray.toList)
  @Test def test_fuse_zipWithIndex_ref: Unit =
    org.junit.Assert.assertEquals(List(("a", 0), ("b", 1), ("c", 2)),
      FArray("a", "b", "c").fuse.zipWithIndex.toFArray.toList)
  @Test def test_fuse_zipWithIndex_long: Unit =
    org.junit.Assert.assertEquals(List((10L, 0), (20L, 1)),
      FArray(10L, 20L).fuse.zipWithIndex.toFArray.toList)

  // ---- zip / map2 (stream-level: lock-step with another source) ----
  private val ys5 = FArray(100, 200, 300, 400, 500)
  private val ly5 = List(100, 200, 300, 400, 500)
  @Test def test_fuse_zip: Unit =
    org.junit.Assert.assertEquals(l10.zip(ly5), r10.fuse.zip(ys5).toFArray.toList)
  @Test def test_fuse_zip_map: Unit = // destructured -> pair never built
    org.junit.Assert.assertEquals(l10.zip(ly5).map((a, b) => a + b),
      r10.fuse.zip(ys5).map((a, b) => a + b).toFArray.toList)
  @Test def test_fuse_filter_then_zip: Unit = // zip pairs the POST-filter stream with that(0,1,2,…)
    org.junit.Assert.assertEquals(l10.filter(_ % 2 == 0).zip(ly5),
      r10.fuse.filter(_ % 2 == 0).zip(ys5).toFArray.toList)
  @Test def test_fuse_zip_then_filter: Unit =
    org.junit.Assert.assertEquals(l10.zip(ly5).filter(_._1 % 2 == 0).map(_._2),
      r10.fuse.zip(ys5).filter(_._1 % 2 == 0).map(_._2).toFArray.toList)
  @Test def test_fuse_zip_shorter: Unit = // that shorter than the pipeline -> stop at that
    org.junit.Assert.assertEquals(l10.zip(List(1, 2)), r10.fuse.zip(FArray(1, 2)).toFArray.toList)
  @Test def test_fuse_zip_longer: Unit = // pipeline shorter than that -> stop at the pipeline
    org.junit.Assert.assertEquals(l10.take(3).zip(ly5), r10.fuse.take(3).zip(ys5).toFArray.toList)
  @Test def test_fuse_zip_empty: Unit =
    org.junit.Assert.assertEquals(List(), r10.fuse.zip(FArray.empty[Int]).toFArray.toList)
  @Test def test_fuse_zip_count: Unit =
    org.junit.Assert.assertEquals(l10.zip(ly5).length, r10.fuse.zip(ys5).count)
  @Test def test_fuse_map2: Unit =
    org.junit.Assert.assertEquals(l10.zip(ly5).map((a, b) => a * b), r10.fuse.map2(ys5)((a, b) => a * b).toFArray.toList)
  @Test def test_fuse_zip_ref: Unit = // String × Int
    org.junit.Assert.assertEquals(List(("a", 1), ("b", 2)),
      FArray("a", "b", "c").fuse.zip(FArray(1, 2)).toFArray.toList)
  @Test def test_fuse_zip_long_source: Unit = // Long element × Int that
    org.junit.Assert.assertEquals(List(11L, 22L),
      FArray(1L, 2L).fuse.map2(FArray(10, 20))((a, b) => a + b).toFArray.toList)

  // ---- specialize-or-fail: a primitive-backed FArray widened to a non-specializable type must be rejected
  //      at compile time (not miscompiled into null reads) — mirrors the eager API's Repr evidence. ----
  @Test def test_fuse_rejects_unspecializable: Unit =
    import scala.compiletime.testing.typeChecks
    // concrete kinds compile:
    assert(typeChecks("""farray.FArray(1, 2, 3).fuse.map(x => x).toFArray"""))
    assert(typeChecks("""farray.FArray("a", "b").fuse.map(x => x).toFArray"""))
    // FArray[Any] (primitive-backed, covariantly widened) must NOT compile:
    assert(!typeChecks("""{ val xs: farray.FArray[Any] = farray.FArray(1, 2, 3); xs.fuse.map(x => x).toFArray }"""))
    assert(!typeChecks("""{ val xs: farray.FArray[AnyVal] = farray.FArray(1, 2, 3); xs.fuse.count }"""))

  // ---- snapshot: the EXPANDED generated code (golden file tracked in git) ----
  @Test def test_fuse_snapshots: Unit =
    val ints = FArray.tabulate(8)(i => i)
    val strs = FArray("a", "b", "c")
    val sb = new StringBuilder
    sb.append(
      """// GENERATED golden file (post-typer expansion of fused pipelines) — do not edit by hand.
        |// Regenerate by deleting this file and running the tests (or set UPDATE_SNAPSHOTS=1).
        |// The `Fuse_this`/`$proxy` preamble is the parsed marker chain; it is dead-code-eliminated by
        |// -opt at runtime (the benchmarks allocate nothing for it). The `({ ... })` block is the actual
        |// fused loop that runs: one pass, leaf fast-path + <kind>At fallback, lambdas inlined.
        |""".stripMargin).append('\n')
    def scenario(title: String, code: String): Unit =
      sb.append("=" * 100).append('\n').append("// ").append(title).append('\n')
        .append("=" * 100).append('\n').append(code.strip).append("\n\n")
    scenario("ints.fuse.map(_+1).filter(_%2==0).map(_*2).toFArray",
      FuseDebug.show(ints.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toFArray))
    scenario("ints.fuse.filter(_%2==0).take(2).map(_*10).toFArray  [positional break]",
      FuseDebug.show(ints.fuse.filter(_ % 2 == 0).take(2).map(_ * 10).toFArray))
    scenario("ints.fuse.drop(2).take(3).toFArray",
      FuseDebug.show(ints.fuse.drop(2).take(3).toFArray))
    scenario("ints.fuse.map(_+1).foldLeft(0)(_+_)",
      FuseDebug.show(ints.fuse.map(_ + 1).foldLeft(0)(_ + _)))
    scenario("ints.fuse.filter(_%2==0).count",
      FuseDebug.show(ints.fuse.filter(_ % 2 == 0).count))
    scenario("strs.fuse.map(_.toUpperCase).toFArray  [Ref]",
      FuseDebug.show(strs.fuse.map(_.toUpperCase).toFArray))
    scenario("strs.fuse.filter(_.nonEmpty).foreach(println)  [Ref, inlined op]",
      FuseDebug.show(strs.fuse.filter(_.nonEmpty).foreach(s => System.out.println(s))))
    scenario("ints.fuse.flatMap(x => FArray(x, x*10)).map(_+1).toFArray  [nested loop, growable out]",
      FuseDebug.show(ints.fuse.flatMap(x => FArray(x, x * 10)).map(_ + 1).toFArray))
    scenario("ints.fuse.map(_+1).find(_%3==0)  [short-circuit via done]",
      FuseDebug.show(ints.fuse.map(_ + 1).find(_ % 3 == 0)))
    scenario("ints.fuse.zipWithIndex.filter(_._2%2==0).map(_._1).toFArray  [specialized tuple, idx counter]",
      FuseDebug.show(ints.fuse.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).toFArray))
    scenario("ints.fuse.zip(ys).map((a,b) => a+b).toFArray  [lock-step; reads that(c); no pair built]",
      FuseDebug.show(ints.fuse.zip(FArray(10, 20, 30, 40)).map((a, b) => a + b).toFArray))
    scenario("ints.fuse.map(x => (f(x)+1, f(x)+2)).map(_._1 + _._2).toFArray  [CSE: f(x)=x*x once]",
      FuseDebug.show(ints.fuse.map(x => (x * x + 1, x * x + 2)).map(t => t._1 + t._2).toFArray))
    scenario("ints.fuse.map(x=>(x+1, x*1000)).filter(_._1%2==0).map(_._2).toFArray  [SINK: 2nd col inside the guard]",
      FuseDebug.show(ints.fuse.map(x => (x + 1, x * 1000)).filter(_._1 % 2 == 0).map(_._2).toFArray))
    scenario("PRODUCT: map(x => Outer(Inner(x, x*99), x*3)).map(o => o.inner.x + o.z)  [nested case class, Inner.y DEAD]",
      FuseDebug.show(ints.fuse.map(x => Outer(Inner(x, x * 99), x * 3)).map(o => o.inner.x + o.z).toFArray))
    scenario("DERIVED TERMINAL: ints.fuse.filter(_%2==0).map(_*10).sum  [foldLeft fusion: acc updated in one pass]",
      FuseDebug.show(ints.fuse.filter(_ % 2 == 0).map(_ * 10).sum))
    def expensive(x: Int): Int = { var s = x; var k = 0; while (k < 24) { s = s * 1103515245 + 12345; k += 1 }; s }
    scenario("DCE-1: map(x => (x+1, expensive(x))).map(_._1)  [expensive(x) DEAD]",
      FuseDebug.show(ints.fuse.map(x => (x + 1, expensive(x))).map(_._1).toFArray))
    scenario("DCE-2: zip(ys).map(_._1)  [zipped read DEAD]",
      FuseDebug.show(ints.fuse.zip(FArray(7, 8, 9, 10, 11)).map(_._1).toFArray))
    scenario("DCE-3: zip(ys).map((a,b) => (a, expensive(b))).filter(_._1%4==0).map(_._1)  [expensive(b) AND ys DEAD]",
      FuseDebug.show(ints.fuse.zip(FArray(7, 8, 9, 10, 11)).map((a, b) => (a, expensive(b))).filter(_._1 % 4 == 0).map(_._1).toFArray))
    Snapshots.check("fused-pipeline.snap", sb.toString)

  @Test def test_hashCode_matchesList(): Unit =
    def chk(name: String, fa: FArray[Any], l: List[Any]): Unit =
      assertEquals(name, l.hashCode.toLong, fa.hashCode.toLong)
    chk("flat", FArray(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5))
    chk("concat", FArray(1, 2, 3) ++ FArray(4, 5), List(1, 2, 3, 4, 5))
    chk("append", FArray(1, 2, 3) :+ 4, List(1, 2, 3, 4))
    chk("prepend", 0 +: FArray(1, 2, 3), List(0, 1, 2, 3))
    chk("append chain", FArray(1) :+ 2 :+ 3 :+ 4, List(1, 2, 3, 4))
    chk("prepend chain", 1 +: 2 +: 3 +: FArray(4), List(1, 2, 3, 4))
    chk("range", FArray.range(0, 10), List.range(0, 10))
    chk("range step", FArray.range(2, 20, 3), List.range(2, 20, 3))
    chk("range desc", FArray.range(10, 0, -2), List.range(10, 0, -2))
    chk("reverse flat", FArray(1, 2, 3, 4).reverse, List(1, 2, 3, 4).reverse)
    chk("reverse range", FArray.range(0, 10).reverse, List.range(0, 10).reverse)
    chk("reverse concat", (FArray(1, 2) ++ FArray(3, 4)).reverse, List(4, 3, 2, 1))
    chk("nested concat", (FArray(1) ++ FArray(2)) ++ (FArray(3) ++ FArray(4)), List(1, 2, 3, 4))
    val cpx = ((FArray.range(0, 5) :+ 99) ++ FArray(-1, -2)).reverse
    chk("complex", cpx, (((0 to 4).toList :+ 99) ::: List(-1, -2)).reverse)
    chk("strings", FArray("a", "b", "c") ++ FArray("d"), List("a", "b", "c", "d"))
    chk("strings non-arith", FArray("xy", "q", "zzz"), List("xy", "q", "zzz"))
    chk("longs", FArray(1L, 2L, 3L) :+ 4L, List(1L, 2L, 3L, 4L))
    chk("doubles", FArray(1.5, 2.5) ++ FArray(3.5), List(1.5, 2.5, 3.5))
    chk("long-as-int unify", FArray(1L, 2L, 3L), List(1, 2, 3))
    chk("empty", FArray.empty[Int], List.empty[Int])
    chk("single", FArray(42), List(42))
    // a standalone million-element range hashes in O(1) and still equals the materialised List
    chk("range 1e6", FArray.range(0, 1000000), List.range(0, 1000000))
    // structure-independence: same contents, different shapes, same hash
    assertEquals(
      "struct-indep",
      FArray(1, 2, 3, 4, 5).hashCode.toLong,
      (FArray(1, 2) ++ (FArray(3) :+ 4 :+ 5)).hashCode.toLong
    )

  // regression: sort/reverse/materialize produce Object[]-backed RefArr; map/filter/fold over a concrete
  // reference type must cast the element (Object->A), not the array (Array[A]), or it CCEs.
  @Test def testRefArrayCastAfterMaterialize(): Unit =
    val xs = FArray("b", "a", "c")
    assertEquals("sorted.map", List("A", "B", "C"), xs.sortWith(_ < _).map(_.toUpperCase).toList)
    assertEquals("reverse.map", List("c!", "a!", "b!"), xs.reverse.map(_ + "!").toList)
    assertEquals("sorted.filter", List("a", "b"), xs.sortWith(_ < _).filter(_ < "c").toList)
    assertEquals("sorted.fold", "abc", xs.sortWith(_ < _).foldLeft("")(_ + _))

  @Test def testRange(): Unit = {
    assertEquals("", List.range(0, 5, 1), FArray.range(0, 5, 1).toList)
    assertEquals("", List.range(0, 5, 2), FArray.range(0, 5, 2).toList)
    assertEquals("", List.range(0, 5, 3), FArray.range(0, 5, 3).toList)
    assertEquals("", List.range(0, -5, -1), FArray.range(0, -5, -1).toList)
    assertEquals("", List.range(0, -5, -2), FArray.range(0, -5, -2).toList)
    assertEquals("", List.range(0, -5, -3), FArray.range(0, -5, -3).toList)
    assertEquals("", List.range(1, 5, 1), FArray.range(1, 5, 1).toList)
    assertEquals("", List.range(1, 5, 2), FArray.range(1, 5, 2).toList)
    assertEquals("", List.range(1, 5, 3), FArray.range(1, 5, 3).toList)
    assertEquals("", List.range(1, -5, -1), FArray.range(1, -5, -1).toList)
    assertEquals("", List.range(1, -5, -2), FArray.range(1, -5, -2).toList)
    assertEquals("", List.range(1, -5, -3), FArray.range(1, -5, -3).toList)
    assertEquals("", List.range(0, 6, 1), FArray.range(0, 6, 1).toList)
    assertEquals("", List.range(0, 6, 2), FArray.range(0, 6, 2).toList)
    assertEquals("", List.range(0, 6, 3), FArray.range(0, 6, 3).toList)
    assertEquals("", List.range(0, -6, -1), FArray.range(0, -6, -1).toList)
    assertEquals("", List.range(0, -6, -2), FArray.range(0, -6, -2).toList)
    assertEquals("", List.range(0, -6, -3), FArray.range(0, -6, -3).toList)
    assertEquals("", List.range(1, 6, 1), FArray.range(1, 6, 1).toList)
    assertEquals("", List.range(1, 6, 2), FArray.range(1, 6, 2).toList)
    assertEquals("", List.range(1, 6, 3), FArray.range(1, 6, 3).toList)
    assertEquals("", List.range(1, -6, -1), FArray.range(1, -6, -1).toList)
    assertEquals("", List.range(1, -6, -2), FArray.range(1, -6, -2).toList)
    assertEquals("", List.range(1, -6, -3), FArray.range(1, -6, -3).toList)
    assertEquals("", List.range(0, 0), FArray.range(0, 0).toList)
  }

  @Test def testRangeInvariants: Unit = {
    org.junit.Assert.assertThrows(
      classOf[IllegalArgumentException],
      () => FArray.range(1, -6, 3)
    )
    org.junit.Assert.assertThrows(
      classOf[IllegalArgumentException],
      () => FArray.range(1, -6, 0)
    )
  }

  // ---- size-0/1 canonicalization invariant: every length-0 FArray is the Empty singleton, every length-1
  // FArray is a per-kind `*One` node. No structural node (Concat/Append/Prepend/SliceNode/ReverseNode/Pad/
  // Updated/RangeNode/*Arr) may have length 0 or 1. Tests are in package farray, so they cast to FBase.
  private def asBase[A](fa: FArray[A]): FBase = fa.asInstanceOf[FBase]
  private def kindName[A](fa: FArray[A]): String = asBase(fa).getClass.getSimpleName
  private def isEmpty[A](fa: FArray[A]): Boolean = asBase(fa) eq Empty.INSTANCE
  private def isOne[A](fa: FArray[A]): Boolean = kindName(fa).endsWith("One")
  // a canonical FArray: 0 -> Empty, 1 -> *One, >=2 -> never Empty/One; AND no structural node is len 0/1.
  private def assertCanonical[A](fa: FArray[A], ctx: String): Unit =
    val n = asBase(fa).length
    if n == 0 then assert(isEmpty(fa), s"$ctx: len 0 but ${kindName(fa)} != Empty")
    else if n == 1 then assert(isOne(fa), s"$ctx: len 1 but ${kindName(fa)} not a *One")
    else assert(!isEmpty(fa) && !isOne(fa), s"$ctx: len $n but ${kindName(fa)} is Empty/One")

  @Test def test_invariant_canonical_singletons(): Unit =
    // every constructor's size-0/1 output is exactly Empty / *One
    assert(isOne(FArray(1)), kindName(FArray(1)))
    assert(isOne(FArray("x")))
    assert(isOne(FArray(1L)) && isOne(FArray(1.0)))
    assert(kindName(FArray(1)) == "IntOne" && kindName(FArray("x")) == "RefOne")
    assert(kindName(FArray(1L)) == "LongOne" && kindName(FArray(1.0)) == "DoubleOne")
    assert(isEmpty(FArray.empty[Int]) && isEmpty(FArray.empty[String]))
    // ++ that collapses to length 1
    assert(isOne(FArray(1) ++ FArray.empty[Int]), kindName(FArray(1) ++ FArray.empty[Int]))
    assert(isOne(FArray.empty[Int] ++ FArray(1)))
    assert(isEmpty(FArray.empty[Int] ++ FArray.empty[Int]))
    // width-1 slice / take / drop of a multi-element leaf -> *One (not a SliceNode)
    val xs = FArray(1, 2, 3, 4, 5)
    assert(isOne(xs.take(1)) && kindName(xs.take(1)) == "IntOne")
    assert(isOne(xs.drop(4)) && kindName(xs.drop(4)) == "IntOne")
    assert(isOne(xs.slice(2, 3)) && kindName(xs.slice(2, 3)) == "IntOne")
    assert(isEmpty(xs.take(0)) && isEmpty(xs.drop(5)) && isEmpty(xs.slice(2, 2)))
    // range
    assert(isOne(FArray.range(5, 6)) && kindName(FArray.range(5, 6)) == "IntOne")
    assert(isEmpty(FArray.range(5, 5)))
    assert(!isOne(FArray.range(5, 7)) && !isEmpty(FArray.range(5, 7)))
    // tabulate / fill / fromArray / fromIterable at 0 and 1
    assert(isOne(FArray.tabulate(1)(_ + 10)) && kindName(FArray.tabulate(1)(identity)) == "IntOne")
    assert(isEmpty(FArray.tabulate(0)(identity)))
    assert(isOne(FArray.fill(1)("a")) && isEmpty(FArray.fill(0)("a")))
    assert(isOne(FArray.fromArray(Array(7))) && isEmpty(FArray.fromArray(Array.empty[Int])))
    assert(isOne(FArray.fromIterable(List(7))) && isEmpty(FArray.fromIterable(Nil)))
    // reverse of a One is a One; reverse of Empty is Empty
    assert(isOne(FArray(9).reverse) && isEmpty(FArray.empty[Int].reverse))
    // updated / padTo / :+ / +: that land at length 1
    assert(isOne(FArray(9).updated(0, 5)) && kindName(FArray(9).updated(0, 5)) == "IntOne")
    assert(isOne(FArray.empty[Int].padTo(1, 3)) && kindName(FArray.empty[Int].padTo(1, 3)) == "IntOne")
    assert(isOne(FArray.empty[Int] :+ 4) && isOne(3 +: FArray.empty[Int]))
    // init / last collapses
    assert(isOne(FArray(1, 2).init) && kindName(FArray(1, 2).init) == "IntOne")

  @Test def test_invariant_no_small_structural_nodes(): Unit =
    // Exhaustively build via the structural ops at every small size and assert canonical shapes.
    val rng = new java.util.Random(0xca7L)
    def clamp(k: Int, n: Int): Int = if k < 0 then 0 else if k > n then n else k
    def gen(depth: Int): FArray[Int] =
      if depth <= 0 then FArray((0 until rng.nextInt(4)).map(_ => rng.nextInt(9))*)
      else
        val a = gen(depth - 1)
        rng.nextInt(11) match
          case 0 => a ++ gen(depth - 1)
          case 1 => a :+ rng.nextInt(9)
          case 2 => rng.nextInt(9) +: a
          case 3 => a.reverse
          case 4 => a.drop(clamp(rng.nextInt(a.length + 2), a.length))
          case 5 => a.take(clamp(rng.nextInt(a.length + 2), a.length))
          case 6 =>
            val lo = clamp(rng.nextInt(a.length + 2), a.length); val hi = clamp(lo + rng.nextInt(a.length + 2), a.length)
            a.slice(lo, hi)
          case 7 => a.padTo(a.length + rng.nextInt(3) - 1, rng.nextInt(9))
          case 8 => if a.isEmpty then a else a.updated(rng.nextInt(a.length), rng.nextInt(9))
          case 9 => a.init
          case _ => a.filter(_ % 2 == 0)
    var t = 0
    while t < 20000 do { val fa = gen(rng.nextInt(6)); assertCanonical(fa, s"gen#$t ${fa.toList}"); t += 1 }

  // size-0/1 parity vs List for a representative op set (scan/reduce/fold/aggregate/slice/map/filter/iterator/mkString)
  @Test def test_size01_parity_vs_list(): Unit =
    val cases: Seq[(FArray[Int], List[Int])] = Seq(
      (FArray.empty[Int], Nil),
      (FArray(7), List(7)),
      ((4 +: FArray.empty[Int]), List(4)), // One via prepend
      (FArray(1, 2, 3).take(1), List(1)), // One via take
      (FArray.range(9, 10), List(9)) // One via range
    )
    for (fa, la) <- cases do
      val c = s"${fa.toList} vs $la"
      assert(fa.toList == la, c)
      assert(fa.iterator.toList == la, s"iterator $c")
      assert(fa.reverseIterator.toList == la.reverse, s"reviter $c")
      assert(fa.scanLeft(0)(_ + _).toList == la.scanLeft(0)(_ + _), s"scanLeft $c -> ${fa.scanLeft(0)(_ + _).toList}")
      assert(fa.scan(0)(_ + _).toList == la.scan(0)(_ + _), s"scan $c")
      assert(fa.scanRight(0)(_ + _).toList == la.scanRight(0)(_ + _), s"scanRight $c")
      assert(fa.foldLeft(100)(_ + _) == la.foldLeft(100)(_ + _), s"foldLeft $c")
      assert(fa.foldRight(100)(_ + _) == la.foldRight(100)(_ + _), s"foldRight $c")
      assert(fa.reduceOption(_ + _) == la.reduceOption(_ + _), s"reduceOption $c")
      assert(fa.sum == la.sum && fa.product == la.product, s"sum/product $c")
      assert(fa.map(_ + 1).toList == la.map(_ + 1), s"map $c")
      assert(fa.filter(_ > 0).toList == la.filter(_ > 0), s"filter $c")
      assert(fa.slice(0, 1).toList == la.slice(0, 1), s"slice $c")
      assert(fa.mkString("[", ",", "]") == la.mkString("[", ",", "]"), s"mkString $c")
      assert(fa.count(_ > 0) == la.count(_ > 0), s"count $c")
      assert(fa.exists(_ > 5) == la.exists(_ > 5) && fa.forall(_ > 0) == la.forall(_ > 0), s"exists/forall $c")
      // scan results themselves must be canonical
      assertCanonical(fa.scanLeft(0)(_ + _), s"scanLeft-result $c")
      assertCanonical(fa.map(_ + 1), s"map-result $c")
      assertCanonical(fa.filter(_ > 0), s"filter-result $c")
    // empty fold/scan/sum semantics match List exactly (covered above): foldLeft/foldRight = z, scanLeft = [z],
    // sum = 0, reduceOption = None. (reduce/max-of-empty throwing is a separate, pre-existing applyAt divergence
    // not introduced here — applyAt on Empty returns the kind default rather than throwing; left untouched.)
    assert(FArray.empty[Int].reduceOption(_ + _).isEmpty && Nil.reduceOption[Int](_ + _).isEmpty)

  // scanLeft/scanRight parity vs List over STRUCTURAL shapes (NOT just flat fromIterable leaves). The whole point
  // is to exercise the scan push-traversers' ReverseNode arm and the deep-base Slice/Pad/Updated read order — the
  // gap that hid an AIOOBE on scan over any ReverseNode of len >= 2. Each FArray is paired with the equivalent
  // List; we check scanLeft AND scanRight with an Int accumulator AND a Ref (String) accumulator, and assert the
  // FArray results stay canonical.
  @Test def test_scan_structural_shapes(): Unit =
    def leaf(xs: Int*): FArray[Int] = FArray.tabulate(xs.length)(i => xs(i)) // genuine flat IntArr leaf
    val cases: Seq[FArray[Int]] = Seq(
      FArray(1, 2, 3).reverse, // ReverseNode of a leaf (len 3) — the core repro
      FArray(1, 2).reverse, // ReverseNode of a leaf (len 2, smallest crashing)
      (leaf(0, 1, 2, 3, 4) ++ leaf(5, 6, 7, 8, 9, 10, 11)).reverse, // ReverseNode of a Concat (the review's probe)
      FArray(1, 2, 3, 4, 5).reverse.reverse, // DOUBLE reverse — visit dir must flip back, write fixed
      ((leaf(1, 2, 3) ++ leaf(4, 5)).reverse ++ leaf(6, 7)).reverse, // nested ReverseNodes inside a Concat
      FArray(1, 2, 3, 4, 5, 6).slice(1, 5).reverse, // ReverseNode of a SliceNode (deep base)
      FArray(1, 2, 3).padTo(6, 9).reverse, // ReverseNode of a Pad (deep base)
      FArray(1, 2, 3, 4).updated(1, 99).reverse, // ReverseNode of an Updated (deep base)
      (FArray(1, 2, 3).reverse).slice(0, 2), // SliceNode whose base is a ReverseNode
      leaf(1, 2, 3) ++ leaf(4, 5, 6), // plain Concat (no reverse)
      0 +: 1 +: 2 +: leaf(3, 4, 5), // Prepend spine
      leaf(1, 2, 3) :+ 4 :+ 5 :+ 6, // Append spine
      (0 +: leaf(1, 2, 3) :+ 4).reverse, // reverse over a prepend+append spine
      leaf(), // empty leaf
      FArray.empty[Int].reverse // reverse of empty
    )
    for fa <- cases do
      val la = fa.toList
      val c = s"shape=$la"
      // Int accumulator
      assert(fa.scanLeft(0)(_ + _).toList == la.scanLeft(0)(_ + _), s"scanLeft/Int $c -> ${fa.scanLeft(0)(_ + _).toList}")
      assert(fa.scanRight(0)(_ + _).toList == la.scanRight(0)(_ + _), s"scanRight/Int $c -> ${fa.scanRight(0)(_ + _).toList}")
      assertCanonical(fa.scanLeft(0)(_ + _), s"scanLeft/Int-result $c")
      assertCanonical(fa.scanRight(0)(_ + _), s"scanRight/Int-result $c")
      // Ref (String) accumulator — exercises the generic ${K}ToRefFold scan traversers
      assert(fa.scanLeft("z")(_ + _.toString).toList == la.scanLeft("z")(_ + _.toString), s"scanLeft/Ref $c")
      assert(fa.scanRight("z")(_.toString + _).toList == la.scanRight("z")(_.toString + _), s"scanRight/Ref $c")

  // (skip,take)-WINDOWED deep-base parity vs List, for the per-element shapes the scan/filter structural tests do NOT
  // exercise: foldLeft/foldRight/sum/min/max/mkString, map, foreach, exists/indexWhere — each over a SliceNode /
  // PadNode / UpdatedNode whose BASE is a TREE (concat / reverse / append+prepend spine / reverse-of-each). This is
  // the windowed sub-traverse (reduceWindowFwd/Bwd etc.) replacing the old per-index applyBoxed deep-base loops:
  // Slice -> windowed-recurse(base, off, len); Updated -> recurse(base,0,i)+elem+recurse(base,i+1,rest); Pad -> full
  // base + filler. Inputs are built STRUCTURALLY (++/reverse/take/drop/padTo/updated), never fromIterable, so the
  // base is a genuine tree node and the window clips real leaf runs inside it. Edge cases: slice-over-reverse (window
  // from the END), deep update at index 0 / middle / last, window length 0 and length n, slice-of-slice, slice-of-updated.
  @Test def test_window_deepbase_shapes(): Unit =
    def leaf(xs: Int*): FArray[Int] = FArray.tabulate(xs.length)(i => xs(i)) // genuine flat IntArr leaf
    // TREE bases (each a non-leaf node) paired with the equivalent List.
    val concatBase = leaf(0, 1, 2, 3, 4) ++ leaf(5, 6, 7, 8, 9, 10, 11) // Concat, len 12
    val concatL = (0 to 11).toList
    val revBase = leaf(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).reverse // ReverseNode of leaf, len 12
    val revL = (0 to 11).toList.reverse
    val revConcat = (leaf(0, 1, 2, 3, 4) ++ leaf(5, 6, 7, 8, 9, 10, 11)).reverse // ReverseNode of Concat
    val revConcatL = (0 to 11).toList.reverse
    val spineBase = (90 +: 91 +: leaf(0, 1, 2, 3, 4, 5, 6, 7) :+ 92 :+ 93) // Prepend+Append spine, len 12
    val spineL = List(90, 91, 0, 1, 2, 3, 4, 5, 6, 7, 92, 93)
    val revSpine = (90 +: leaf(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) :+ 91).reverse // reverse over a spine, len 12
    val revSpineL = List(90, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 91).reverse
    val nestedRev = ((leaf(0, 1, 2) ++ leaf(3, 4)).reverse ++ leaf(5, 6, 7, 8, 9, 10, 11)) // nested ReverseNode in Concat
    val nestedRevL = (List(0, 1, 2, 3, 4).reverse ::: List(5, 6, 7, 8, 9, 10, 11))

    val trees: List[(String, FArray[Int], List[Int])] = List(
      ("concat", concatBase, concatL),
      ("reverse", revBase, revL),
      ("reverseConcat", revConcat, revConcatL),
      ("spine", spineBase, spineL),
      ("reverseSpine", revSpine, revSpineL),
      ("nestedRev", nestedRev, nestedRevL)
    )

    // Per tree base, wrap it in a SliceNode / PadNode / UpdatedNode and check every per-element op vs List.
    for (bn, base, bl) <- trees do
      val n = bl.length
      // SliceNode windows: full (0,n), prefix, suffix (from the END — exercises the Bwd reversed-frame clip),
      // interior, length 0 (empty window), length 1, and slice-of-slice.
      val sliceCases: List[(String, (Int, Int))] = List(
        ("full", (0, n)),
        ("prefix", (0, 4)),
        ("suffix", (n - 5, n)),
        ("interior", (3, 9)),
        ("len0", (5, 5)),
        ("len1", (6, 7)),
        ("fromEnd", (n - 1, n))
      )
      for (sn, (from, until)) <- sliceCases do
        val fa = base.slice(from, until)
        val la = bl.slice(from, until)
        checkWindowOps(s"$bn.slice($from,$until)/$sn", fa, la)
        // slice-of-slice: re-window the already-windowed result
        if la.nonEmpty then
          val faa = fa.slice(1, fa.length); val laa = la.slice(1, la.length)
          checkWindowOps(s"$bn.slice($from,$until).slice(1,_)/$sn", faa, laa)

      // PadNode over the tree base: pad to n+5 with a filler.
      locally {
        val fa = base.padTo(n + 5, 999); val la = bl.padTo(n + 5, 999)
        checkWindowOps(s"$bn.padTo(${n + 5},999)", fa, la)
        // slice INTO the padded region (window straddles base + filler) and ITS reverse
        checkWindowOps(s"$bn.padTo.slice(base..fill)", fa.slice(n - 2, n + 3), la.slice(n - 2, n + 3))
        checkWindowOps(s"$bn.padTo.reverse", fa.reverse, la.reverse)
      }

      // UpdatedNode over the tree base at index 0 / middle / last, and slice-of-updated.
      for ui <- List(0, n / 2, n - 1) do
        val fa = base.updated(ui, 777); val la = bl.updated(ui, 777)
        checkWindowOps(s"$bn.updated($ui,777)", fa, la)
        // slice-of-updated: window across the replaced slot (prefix base | elem | suffix base)
        val lo = math.max(0, ui - 2); val hi = math.min(n, ui + 3)
        checkWindowOps(s"$bn.updated($ui).slice($lo,$hi)", fa.slice(lo, hi), la.slice(lo, hi))
        checkWindowOps(s"$bn.updated($ui).reverse", fa.reverse, la.reverse)

  /** Check every per-element op covered by the windowed deep-base traversers against the equivalent List op. */
  private def checkWindowOps(c: String, fa: FArray[Int], la: List[Int]): Unit =
    assert(fa.toList == la, s"toList $c -> ${fa.toList} vs $la")
    assert(fa.foldLeft(100)(_ - _) == la.foldLeft(100)(_ - _), s"foldLeft $c")
    assert(fa.foldRight(100)(_ - _) == la.foldRight(100)(_ - _), s"foldRight $c")
    assert(fa.sum == la.sum, s"sum $c")
    assert(fa.map(_ * 2 + 1).toList == la.map(_ * 2 + 1), s"map $c")
    locally {
      val sb = collection.mutable.ArrayBuffer.empty[Int]
      fa.foreach(sb += _)
      assert(sb.toList == la, s"foreach $c -> ${sb.toList} vs $la")
    }
    assert(fa.exists(_ == 6) == la.exists(_ == 6), s"exists6 $c")
    assert(fa.exists(_ > 1000) == la.exists(_ > 1000), s"existsNone $c")
    assert(fa.indexWhere(_ == 6) == la.indexWhere(_ == 6), s"indexWhere6 $c")
    assert(fa.indexWhere(_ > 1000) == la.indexWhere(_ > 1000), s"indexWhereNone $c")
    assert(fa.mkString("[", ",", "]") == la.mkString("[", ",", "]"), s"mkString $c")
    if la.nonEmpty then
      assert(fa.min == la.min, s"min $c")
      assert(fa.max == la.max, s"max $c")
      assert(fa.reduce(_ + _) == la.reduce(_ + _), s"reduce $c")

  inline def assertEquals[Res1, Res2, To](msg: String, res1: Res1, res2: Res2)(using
      Res1: CompareAs[Res1, To],
      Res2: CompareAs[Res2, To]
  ) =
    org.junit.Assert.assertEquals(msg, Res1(res1), Res2(res2))

  def inputs(base: String): Seq[List[String]] =
    val all = base.toList.map(_.toString)
    all.indices.map(all.take)

  val lists: Seq[List[String]] = inputs("aabcdefg")

  def test1[Res1, Res2, To](
      f: List[String] => Res1
  )(g: FArray[String] => Res2)(using CompareAs[Res1, To], CompareAs[Res2, To]) =
    lists.foreach { list =>
      assertEquals(list.toString, f(list), g(FArray.fromIterable(list)))
    }

  def test1NonEmpty[Res1, Res2, To](
      f: List[String] => Res1
  )(g: FArray[String] => Res2)(using CompareAs[Res1, To], CompareAs[Res2, To]) =
    lists.filterNot(_.isEmpty).foreach { list =>
      assertEquals(list.toString, f(list), g(FArray.fromIterable(list)))
    }

  def test2[Res1, Res2, To](
      f: (List[String], List[String]) => Res1
  )(g: (FArray[String], FArray[String]) => Res2)(using CompareAs[Res1, To], CompareAs[Res2, To]) =
    lists.foreach { list1 =>
      inputs("aadcdfdf112345").foreach { list2 =>
        assertEquals(s"$list1, $list2", f(list1, list2), g(FArray.fromIterable(list1), FArray.fromIterable(list2)))
      }
    }

  def test3[Res1, Res2, To](
      f: (List[String], List[String], List[String]) => Res1
  )(g: (FArray[String], FArray[String], FArray[String]) => Res2)(using CompareAs[Res1, To], CompareAs[Res2, To]) =
    lists.foreach { list1 =>
      inputs("aadcdfdf112345").foreach { list2 =>
        inputs("a999").foreach { list3 =>
          assertEquals(
            s"$list1, $list2 $list3",
            f(list1, list2, list3),
            g(FArray.fromIterable(list1), FArray.fromIterable(list2), FArray.fromIterable(list3))
          )
        }
      }
    }

  // this test compares results from `FList` to those of `List`. Types vary a bit, so this massages both sides into something safe we can compare
  trait CompareAs[From, To] extends (From => To)

  object CompareAs:
    /* base case, can safely be compared */
    given [T <: Comparable[T] | AnyVal]: CompareAs[T, T] = t => t

    given CompareAs[Integer, Int] = x => x
    given CompareAs[Character, Char] = x => x
    given CompareAs[Range, Range] = x => x // todo: should be covered by `Iterable` below

    given [K1, K2, V1, V2](using K: CompareAs[K1, K2], V: CompareAs[V1, V2]): CompareAs[Map[K1, V1], Map[K2, V2]] =
      _.map { case (k, v) => K(k) -> V(v) }
    given [T1, T2](using T: CompareAs[T1, T2]): CompareAs[Option[T1], Option[T2]] = _.map(T)
    given [T1 <: AnyRef, T2](using T: CompareAs[T1, T2]): CompareAs[FArray[T1], List[T2]] = _.toList.map(T)
    given [C[t] <: Iterable[t], T1, T2](using T: CompareAs[T1, T2]): CompareAs[C[T1], List[T2]] = _.map(T).toList
    given [T1, T2](using T: CompareAs[T1, T2]): CompareAs[Iterator[T1], List[T2]] = _.map(T).toList
    given [T1, T2](using T: CompareAs[T1, T2]): CompareAs[Array[T1], List[T2]] = _.toList.map(T)

    given [T1, T2, U1, U2](using T: CompareAs[T1, T2], U: CompareAs[U1, U2]): CompareAs[(T1, U1), (T2, U2)] =
      case (t, u) => (T(t), U(u))

    given [T1, T2, U1, U2, V1, V2](using
        T: CompareAs[T1, T2],
        U: CompareAs[U1, U2],
        V: CompareAs[V1, V2]
    ): CompareAs[(T1, U1, V1), (T2, U2, V2)] =
      case (t, u, v) => (T(t), U(u), V(v))
