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
    import farray.ListSyntax.*                       // method-local: shadows scala.:: only here
    val xs = 1 :: 2 :: 3 :: Nil
    assert(xs.toList == List(1, 2, 3))
    xs match
      case h :: t => assert(h == 1 && t.toList == List(2, 3))
      case _      => assert(false, "should be non-empty")
    val empty: FArray[Int] = Nil
    assert(empty match { case _ :: _ => false; case _ => true })
    def sum(ys: FArray[Int]): Int = ys match { case h :: t => h + sum(t); case _ => 0 }
    assert(sum(xs) == 6)
    assert(sum(xs.map(_ + 1)) == 9)                  // pick apart after a map (leaf, not Prepend)
    val ss = "a" :: "bb" :: Nil                       // reference elements too
    assert((ss match { case h :: _ => h; case _ => "" }) == "a")
  }
  @Test def test_iterator_trees: Unit = {
    def check(fa: FArray[Int], expected: List[Int]): Unit =
      assert(fa.iterator.toList == expected, s"iter ${fa.iterator.toList} != $expected")
      assert(fa.reverseIterator.toList == expected.reverse, s"reviter ${fa.reverseIterator.toList} != ${expected.reverse}")
    val a = FArray(1, 2, 3); val b = FArray(4, 5, 6)
    check(a, List(1, 2, 3))
    check(a ++ b, List(1, 2, 3, 4, 5, 6))                       // Concat
    check(a :+ 7, List(1, 2, 3, 7))                             // Append
    check(0 +: a, List(0, 1, 2, 3))                             // Prepend
    check(a.reverse, List(3, 2, 1))                             // Reverse
    check(a.drop(1), List(2, 3))                                // Slice
    check((a ++ b).drop(1).take(3), List(2, 3, 4))              // Slice over Concat
    check(0 +: (a ++ b) :+ 9, List(0, 1, 2, 3, 4, 5, 6, 9))     // Prepend + Concat + Append
    check(a.padTo(5, 0), List(1, 2, 3, 0, 0))                   // Pad
    check(a.updated(1, 9), List(1, 9, 3))                       // Updated
    check(((a ++ b) ++ (a ++ b)).reverse, List(1,2,3,4,5,6,1,2,3,4,5,6).reverse) // Reverse of nested Concat
    check(FArray.empty[Int], Nil)
    val s = FArray("a", "b"); assert(((s :+ "c") ++ FArray("d")).iterator.toList == List("a", "b", "c", "d"))  // Ref cursor
  }
  @Test def test_iterator_fuzz: Unit = {
    val rng = new java.util.Random(0xF00DL)
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
          case 0 => val (fb, lb) = gen(depth - 1); (fa ++ fb, la ++ lb)        // Concat
          case 1 => val e = rng.nextInt(1000); (fa :+ e, la :+ e)              // Append
          case 2 => val e = rng.nextInt(1000); (e +: fa, e :: la)             // Prepend
          case 3 => (fa.reverse, la.reverse)                                   // Reverse
          case 4 => val k = clamp(rng.nextInt(la.length + 2), la.length); (fa.drop(k), la.drop(k))
          case 5 => val k = clamp(rng.nextInt(la.length + 2), la.length); (fa.take(k), la.take(k))
          case 6 => { val lo = clamp(rng.nextInt(la.length + 2), la.length); val hi = clamp(lo + rng.nextInt(la.length + 2), la.length); (fa.slice(lo, hi), la.slice(lo, hi)) }  // Slice
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
  @Test def `test_lengthIs >` : Unit = test1(_.lengthIs > 2)(_.lengthIs > 2)
  @Test def `test_lengthIs <=` : Unit = test1(_.lengthIs <= 2)(_.lengthIs <= 2)
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
  @Test def `test_reverse_:::` : Unit = test2(_.reverse_:::(_))(_.reverse_:::(_))
  @Test def test_reverseIterator: Unit = test1(_.reverseIterator.toList)(_.reverseIterator.toList)
  @Test def test_size: Unit = test1(_.size)(_.size)
  @Test def `test_sizeIs >` : Unit = test1(_.sizeIs > 2)(_.sizeIs > 2)
  @Test def `test_sizeIs <=` : Unit = test1(_.sizeIs <= 2)(_.sizeIs <= 2)
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
