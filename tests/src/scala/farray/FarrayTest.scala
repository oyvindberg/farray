package farray

import org.junit.Test

import scala.collection.BuildFrom

class FListTest:
  @Test def test_++ : Unit = test2(_ ++ _)(_ ++ _)
  @Test def test_+: : Unit = test1("x" +: _)("x" +: _)
  @Test def test_:+ : Unit = test1(_ :+ "x")(_ :+ "x")
  @Test def test_:: : Unit = test1("x" :: _)("x" :: _)
  @Test def test_::: : Unit = test2(_ ::: _)(_ ::: _)
  @Test def test_apply: Unit = test1NonEmpty(_(0))(_(0))
  @Test def test_collect: Unit = test1(_.collect { case str if str.nonEmpty => str })(_.collect { case str if str.nonEmpty => str })
  @Test def test_collectFirst: Unit = test1(_.collectFirst { case str if str.nonEmpty => str })(_.collectFirst { case str if str.nonEmpty => str })
  @Test def test_contains: Unit = test1(_.contains("a"))(_.contains("a"))
  @Test def test_corresponds: Unit = test1(xs => xs.corresponds(xs.take(2))(_ == _))(xs => xs.corresponds(xs.take(2))(_ == _))
  @Test def test_count: Unit = test1(_.count(_.contains('a')))(_.count(_.contains('a')))
  @Test def test_diff: Unit = test1(xs => xs.diff(xs.take(2)))(xs => xs.diff(xs.take(2)))
  @Test def test_distinct: Unit = test1(_.distinct)(_.distinct)
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
