package farray

import org.junit.Test

import scala.collection.BuildFrom

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
      assert(FArray(7).copyToArray(d0, 0, 0) == 0 && d0.toList == List(-1, -1, -1))          // len 0
      assert(FArray(7).copyToArray(d0, 3, 5) == 0 && d0.toList == List(-1, -1, -1))          // no room
      val d1 = Array.fill(3)(-1)
      assert(FArray(9).copyToArray(d1, 1, 1) == 1 && d1.toList == List(-1, 9, -1))           // leaf size 1
      val d2 = Array.fill(3)(-1)
      assert((0 +: FArray.empty[Int]).copyToArray(d2, 0, 1) == 1 && d2.toList == List(0, -1, -1)) // One node
      val d3 = Array.fill(3)(-1)
      assert(FArray(1, 2, 3).copyToArray(d3, 0, 1) == 1 && d3.toList == List(1, -1, -1))     // clamp len to 1
      val d4 = Array.fill(2)(-1)
      assert(FArray(1, 2, 3).copyToArray(d4, 1, 9) == 1 && d4.toList == List(-1, 1))         // clamp avail to 1
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
    val f32 = FArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)
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
    else
      assert(!isEmpty(fa) && !isOne(fa), s"$ctx: len $n but ${kindName(fa)} is Empty/One")

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
      ((4 +: FArray.empty[Int]), List(4)),       // One via prepend
      (FArray(1, 2, 3).take(1), List(1)),        // One via take
      (FArray.range(9, 10), List(9))             // One via range
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
