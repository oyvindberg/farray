package farray

import org.junit.Test

import scala.collection.BuildFrom

//format: off
object Benchmark:
  def foo(str: String): Either[String, String] = if (str.length > 2) Left(str) else Right(str)

  // not applicable for IArray
  //  @Test def test_:: : Unit = test1("x" :: _)("x" :: _)("x" :: _)
  //  @Test def test_::: : Unit = test2(_ ::: _)(_ ::: _)(_ ::: _)
  //  @Test def `test_reverse_:::` : Unit = test2(_.reverse_:::(_))(_.reverse_:::(_))(_.reverse_:::(_))

  val tests = List(
    test2("test_++ ", _ ++ _)(_ ++ _)(_ ++ _),
    test1("test_+: ", "x" +: _)("x" +: _)("x" +: _),
    test1("test_:+ ", _ :+ "x")(_ :+ "x")(_ :+ "x"),
    test1NonEmpty("test_apply", _(0))(_(0))(_(0)),
    test1("test_collect", _.collect { case str if str.nonEmpty => str })(_.collect { case str if str.nonEmpty => str })(_.collect { case str if str.nonEmpty => str }),
    test1("test_collectFirst", _.collectFirst { case str if str.nonEmpty => str })(_.collectFirst { case str if str.nonEmpty => str })(_.collectFirst { case str if str.nonEmpty => str }),
    test1("test_contains", _.contains("a"))(_.contains("a"))(_.contains("a")),
    test1("test_corresponds", xs => xs.corresponds(xs.take(2))(_ == _))(xs => xs.corresponds(xs.take(2))(_ == _))(xs => xs.corresponds(xs.take(2))(_ == _)),
    test1("test_count", _.count(_.contains('a')))(_.count(_.contains('a')))(_.count(_.contains('a'))),
    test1("test_diff", xs => xs.diff(xs.take(2)))(xs => xs.diff(xs.take(2)))(xs => xs.diff(xs.take(2))),
    test1("test_distinct", _.distinct)(_.distinct)(_.distinct),
    test1("test_distinct_by", _.distinctBy(_.length))(_.distinctBy(_.length))(_.distinctBy(_.length)),
    test1("test_drop", _.drop(1))(_.drop(1))(_.drop(1)),
    test1("test_dropRight", _.dropRight(1))(_.dropRight(1))(_.dropRight(1)),
    test1("test_dropWhile", _.dropWhile(!_.headOption.contains('b')))(_.dropWhile(!_.headOption.contains('b')))(_.dropWhile(!_.headOption.contains('b'))),
    test1("test_endsWith", xs => xs.endsWith(xs.take(2)))(xs => xs.endsWith(xs.take(2)))(xs => xs.endsWith(xs.take(2))),
    test1("test_equals", xs => xs.equals(xs.take(2)))(xs => xs.equals(xs.take(2)))(xs => xs.equals(xs.take(2))),
    test1("test_exists", _.exists(_ == "a"))(_.exists(_ == "a"))(_.exists(_ == "a")),
    test1("test_filter", xs => xs.filter(xs.headOption.contains))(xs => xs.filter(xs.headOption.contains))(xs => xs.filter(xs.headOption.contains)),
    test1("test_filterNot", xs => xs.filterNot(xs.headOption.contains))(xs => xs.filterNot(xs.headOption.contains))(xs => xs.filterNot(xs.headOption.contains)),
    test1("test_find", _.find(_.headOption.contains('a')))(_.find(_.headOption.contains('a')))(_.find(_.headOption.contains('a'))),
    test1("test_flatMap", _.flatMap(str => List(str, str)))(_.flatMap(str => FArray(str, str)))(_.flatMap(str => IArray(str, str))),
    test1("test_flatten", _.map(str => List(str, str)).flatten)(_.map(str => FArray(str, str)).flatten)(_.map(str => IArray(str, str)).flatten),
    test1("test_fold", _.fold("")(_ + _))(_.fold("")(_ + _))(_.fold("")(_ + _)),
    test1("test_foldLeft", _.foldLeft(0)(_ + _.length))(_.foldLeft(0)(_ + _.length))(_.foldLeft(0)(_ + _.length)),
    test1("test_foldRight", _.foldRight("")(_.headOption.getOrElse('x').toString + _))(_.foldRight("")(_.headOption.getOrElse('x').toString + _))(_.foldRight("")(_.headOption.getOrElse('x').toString + _)),
    test1("test_forall", _.forall(_.isEmpty))(_.forall(_.isEmpty))(_.forall(_.isEmpty)),
    test1("test_foreach", { xs => var ret = ""; xs.foreach(ret += _); ret })({ xs => var ret = ""; xs.foreach(ret += _); ret })({ xs => var ret = ""; xs.foreach(ret += _); ret }),
    test1("test_groupBy", _.groupBy(x => x.length))(_.groupBy(x => x.length))(_.groupBy(x => x.length)),
    test1("test_groupMap", _.groupMap(x => x.length)(_ * 2))(_.groupMap(x => x.length)(_ * 2))(_.groupMap(x => x.length)(_ * 2)),
    test1NonEmpty("test_head", _.head)(_.head)(_.head),
    test1("test_headOption", _.headOption)(_.headOption)(_.headOption),
    test1("test_indexOf", _.indexOf("a"))(_.indexOf("a"))(_.indexOf("a")),
    test1("test_indexWhere", _.indexWhere(_ == "a"))(_.indexWhere(_ == "a"))(_.indexWhere(_ == "a")),
    test1("test_indices", _.indices)(_.indices)(_.indices),
    test1NonEmpty("test_init", _.init)(_.init)(_.init),
    test2("test_intersect", _ intersect _)(_ intersect _)(_ intersect _),
    test1("test_isDefinedAt", _.isDefinedAt(1))(_.isDefinedAt(1))(_.isDefinedAt(1)),
    test1("test_isEmpty", _.isEmpty)(_.isEmpty)(_.isEmpty),
    test1("test_iterator", _.iterator)(_.iterator)(_.iterator),
    test1NonEmpty("test_last", _.last)(_.last)(_.last),
    test1("test_lastOption", _.lastOption)(_.lastOption)(_.lastOption),
//    test3("test_lazyZip", _.lazyZip(_).lazyZip(_).toList)(_.lazyZip(_, _))(_.lazyZip(_).lazyZip(_).toList),
    test1("test_lengthCompare", _.lengthCompare(1))(_.lengthCompare(1))(_.lengthCompare(1)),
    test1("`test_lengthIs >` ", _.lengthIs > 2)(_.lengthIs > 2)(_.lengthIs > 2),
    test1("`test_lengthIs <=` ", _.lengthIs <= 2)(_.lengthIs <= 2)(_.lengthIs <= 2),
    test1("test_map", _.map(_.toUpperCase))(_.map(_.toUpperCase))(_.map(_.toUpperCase)),
    test1NonEmpty("test_max", _.max)(_.max)(_.max),
    test1NonEmpty("test_maxBy", _.maxBy(_.length))(_.maxBy(_.length))(_.maxBy(_.length)),
    test1NonEmpty("test_min", _.min)(_.min)(_.min),
    test1NonEmpty("test_minBy", _.minBy(_.length))(_.minBy(_.length))(_.minBy(_.length)),
    test1("test_mkString", _.mkString("_", ";", "-"))(_.mkString("_", ";", "-"))(_.mkString("_", ";", "-")),
    test1("test_nonEmpty", _.nonEmpty)(_.nonEmpty)(_.nonEmpty),
    test1("test_padTo", _.padTo(10, ""))(_.padTo(10, ""))(_.padTo(10, "")),
    test1("test_partition", _.partition(_.length > 2))(_.partition(_.length > 2))(_.partition(_.length > 2)),
    test1("test_partitionMap", _.partitionMap(foo))(_.partitionMap(foo))(_.partitionMap(foo)),
    test1NonEmpty("test_reduce", _.reduce((acc, str) => acc + str))(_.reduce((acc, str) => acc + str))(_.reduce((acc, str) => acc + str)),
    test1NonEmpty("test_reduceLeft", _.reduceLeft((acc, str) => acc + str))(_.reduceLeft((acc, str) => acc + str))(_.reduceLeft((acc, str) => acc + str)),
    test1NonEmpty("test_reduceRight", _.reduceRight((str, acc) => acc + str))(_.reduceRight((str, acc) => acc + str))(_.reduceRight((str, acc) => acc + str)),
    test1("test_reduceOption", _.reduceOption((acc, str) => acc + str))(_.reduceOption((acc, str) => acc + str))(_.reduceOption((acc, str) => acc + str)),
    test1("test_reverse", _.reverse)(_.reverse)(_.reverse),
    test1("test_reverseIterator", _.reverseIterator.toList)(_.reverseIterator.toList)(_.reverseIterator.toList),
    test1("test_size", _.size)(_.size)(_.size),
    test1("`test_sizeIs >` ", _.sizeIs > 2)(_.sizeIs > 2)(_.sizeIs > 2),
    test1("`test_sizeIs <=` ", _.sizeIs <= 2)(_.sizeIs <= 2)(_.sizeIs <= 2),
    test1("test_sortBy", _.sortBy(x => x))(_.sortBy(x => x))(_.sortBy(x => x)),
    test1("test_sorted", _.sorted)(_.sorted)(_.sorted),
    test1("test_sortWith", _.sortWith((x, y) => x < y))(_.sortWith((x, y) => x < y))(_.sortWith((x, y) => x < y)),
    test1("test_span", _.span(_.length > 2))(_.span(_.length > 2))(_.span(_.length > 2)),
    test1("test_splitAt", _.splitAt(2))(_.splitAt(2))(_.splitAt(2)),
    test1("test_startsWith", xs => xs.startsWith(xs.takeRight(2)))(xs => xs.startsWith(xs.takeRight(2)))(xs => xs.startsWith(xs.takeRight(2))),
    test1NonEmpty("test_tail", _.tail)(_.tail)(_.tail),
    test1("test_take", _.take(2))(_.take(2))(_.take(2)),
    test1("test_takeRight", _.takeRight(2))(_.takeRight(2))(_.takeRight(2)),
    test1("test_takeWhile", _.takeWhile(_.length <= 2))(_.takeWhile(_.length <= 2))(_.takeWhile(_.length <= 2)),
    test1("test_toArray", _.toArray)(_.toArray)(IArray.genericWrapArray),
    test1("test_toList", _.toList)(_.toList)(_.toList),
    test1("test_toMap", _.map(x => x -> x).toMap)(_.map(x => x -> x).toMap)(_.map(x => x -> x).toMap),
    test1("test_toSet", _.toSet)(_.toSet)(_.toSet),
    test1("test_toVector", _.toVector)(_.toVector)(_.toVector),
    test1("test_transpose", _.map(_.toList).transpose)(_.map(str => FArray.fromIterable(str.map(c => c: Character))).transpose)(_.map(str => IArray.from(str.map(c => c: Character))).transpose),
    test1("test_unzip", _.map(x => x -> x).unzip)(_.map(x => x -> x).unzip)(_.map(x => x -> x).unzip),
    test1("test_unzip3", _.map(x => (x, x, x)).unzip3)(_.map(x => (x, x, x)).unzip3)(_.map(x => (x, x, x)).unzip3),
    test1NonEmpty("test_updated", _.updated(0, "yy"))(_.updated(0, "yy"))(_.updated(0, "yy")),
    test2("test_zip", _ zip _)(_ zip _)(_ zip _),
    test1("test_zipWithIndex", _.zipWithIndex)(_.zipWithIndex)(_.zipWithIndex),
  )

  def inputs(base: String, nonEmpty: Boolean = false): (List[List[String]], FArray[FArray[String]], IArray[IArray[String]]) =
    val all = (base * 4).toArray.map(_.toString)
    val indices = if nonEmpty then all.indices.drop(1) else all.indices
    val allList = indices.map(all.take).map(_.toList).toList
    val allFArray = FArray.fromIterable(indices.map(all.take).map(FArray.fromArray(_)))
    val allIArray = IArray.from(indices.map(all.take).map[IArray[String]](IArray.unsafeFromArray))
    (allList, allFArray, allIArray)

  trait test {
    def runAllList(): Unit
    def runAllFArray(): Unit
    def runAllIArray(): Unit
  }

  case class test1[Res1, Res2, Res3]
    (name: String, runList: List[String] => Res1)
    (runFArray: FArray[String] => Res2)
    (runIArray: IArray[String] => Res3) extends test {

    val (listInput, farrayInput, iarrayInput) = inputs("aabcdefg")
    def runAllList(): Unit = listInput.foreach(runList)
    def runAllFArray(): Unit = farrayInput.foreach(runFArray(_))
    def runAllIArray(): Unit = iarrayInput.foreach(runIArray)
  }

  case class test1NonEmpty[Res1, Res2, Res3]
    (name: String, runList: List[String] => Res1)
    (runFArray: FArray[String] => Res2)
    (runIArray: IArray[String] => Res3) extends test {

    val (listInput, farrayInput, iarrayInput) = inputs("aabcdefgaabcdefgaabcdefgaabcdefgaabcdefg", nonEmpty = true)
    def runAllList(): Unit = listInput.foreach(runList)
    def runAllFArray(): Unit = farrayInput.foreach(runFArray(_))
    def runAllIArray(): Unit = iarrayInput.foreach(runIArray)
  }

  case class test2[Res1, Res2, Res3]
    (name: String, runList: (List[String], List[String]) => Res1)
    (runFArray: (FArray[String], FArray[String]) => Res2)
    (runIArray: (IArray[String], IArray[String]) => Res3) extends test {
    val (listInput1, farrayInput1, iarrayInput1) = inputs("aabcdefg")
    val (listInput2, farrayInput2, iarrayInput2) = inputs("aadcdfdf112345aadcdfdf112345aadcdfdf112345")

    def runAllList(): Unit = listInput1.foreach(i1 => listInput2.foreach(i2 => runList(i1, i2)))

    def runAllFArray(): Unit = farrayInput1.foreach(i1 => farrayInput2.foreach(i2 => runFArray(i1, i2)))

    def runAllIArray(): Unit = iarrayInput1.foreach(i1 => iarrayInput2.foreach(i2 => runIArray(i1, i2)))
  }

  case class test3[Res1, Res2, Res3]
    (name: String, runList: (List[String], List[String], List[String]) => Res1)
    (runFArray: (FArray[String], FArray[String], FArray[String]) => Res2)
    (runIArray: (IArray[String], IArray[String], IArray[String]) => Res3) extends test {
    val (listInput1, farrayInput1, iarrayInput1) = inputs("aabcdefgxxxxxxxxxxxxxxxxxxaabcdefgxxxxxxxxxxxxxxxxxxaabcdefgxxxxxxxxxxxxxxxxxx")
    val (listInput2, farrayInput2, iarrayInput2) = inputs("aadcdfdf112345")
    val (listInput3, farrayInput3, iarrayInput3) = inputs("a999a999a999a999a999a999a999a999a999a999a999a999a999a999a999")

    def runAllList(): Unit = listInput1.foreach(i1 => listInput2.foreach(i2 => listInput3.foreach(i3 => runList(i1, i2, i3))))

    def runAllFArray(): Unit = farrayInput1.foreach(i1 => farrayInput2.foreach(i2 => farrayInput3.foreach(i3 => runFArray(i1, i2, i3))))

    def runAllIArray(): Unit = iarrayInput1.foreach(i1 => iarrayInput2.foreach(i2 => iarrayInput3.foreach(i3 => runIArray(i1, i2, i3))))
  }

  def timed(title: String)(t: => Unit): Unit = {
    val t0 = System.currentTimeMillis()
    t
    val td = System.currentTimeMillis() - t0
    println(s"$title: $td ms")
  }

  @main
  def flaff = {
    // warmup
    tests.foreach { test =>
      test.runAllFArray()
      test.runAllList()
      test.runAllIArray()
    }
    timed("cold FArray") {
      tests.foreach { test =>
        test.runAllFArray()
      }
    }
    timed("cold List") {
      tests.foreach { test =>
        test.runAllList()
      }
    }
    timed("cold IArray") {
      tests.foreach { test =>
        test.runAllIArray()
      }
    }
    0 to 1000 foreach { _ =>
      tests.foreach { test =>
        test.runAllFArray()
        test.runAllList()
        test.runAllIArray()
      }
    }
    timed("warm FArray") {
      tests.foreach { test =>
        test.runAllFArray()
      }
    }
    timed("warm List") {
      tests.foreach { test =>
        test.runAllList()
      }
    }
    timed("warm IArray") {
      tests.foreach { test =>
        test.runAllIArray()
      }
    }
  }