package farray

/** COMPILE-ONLY gate (project `nulls-tests`, built with `-Yexplicit-nulls`). Every def below expands a macro (`.fuse` pipelines) or an inline op (`distinctBy`,
  * …) at THIS call site; under explicit nulls a bare `null` flowing into a non-nullable type in the expansion is a hard type error. If this project compiles,
  * the generated surface is explicit-nulls-clean and `.fuse` / `distinctBy` are adoptable inside a codebase compiled with `-Yexplicit-nulls` (e.g. the Scala 3
  * compiler itself).
  */
object NullsCompile:

  // ---- fuse pipelines: the element-loop skeleton (per-element read) + varied sinks ----
  def intPipe(xs: FArray[Int]): List[Int] = xs.fuse.filter(_ % 2 == 0).map(_ + 1).run.toList
  def intForeach(xs: FArray[Int]): Int = { var s = 0; xs.fuse.map(_ * 2).foreach(s += _); s }
  def intFold(xs: FArray[Int]): Int = xs.fuse.filter(_ > 0).foldLeft(0)(_ + _)
  def intCount(xs: FArray[Int]): Int = xs.fuse.count(_ % 3 == 0)
  def intExists(xs: FArray[Int]): Boolean = xs.fuse.exists(_ == 7)
  def intSum(xs: FArray[Int]): Int = xs.fuse.map(_ + 1).sum
  def intMkString(xs: FArray[Int]): String = xs.fuse.filter(_ > 1).mkString(",")
  def longPipe(xs: FArray[Long]): List[Long] = xs.fuse.map(_ + 1L).filter(_ > 0L).run.toList
  def doublePipe(xs: FArray[Double]): List[Double] = xs.fuse.map(_ * 2.0).run.toList
  def refPipe(xs: FArray[String]): List[String] = xs.fuse.filter(_.nonEmpty).map(s => s + "!").run.toList
  def refToInt(xs: FArray[String]): List[Int] = xs.fuse.map(_.length).filter(_ > 0).run.toList
  def refToRef(xs: FArray[String]): FArray[String] = xs.fuse.filter(_.nonEmpty).map(s => s).run
  def collectPipe(xs: FArray[Int]): List[Int] = xs.fuse.collect { case n if n > 0 => n * 10 }.run.toList
  def zipIdxPipe(xs: FArray[String]): List[(String, Int)] = xs.fuse.zipWithIndex.run.toList
  def takeRightPipe(xs: FArray[Int]): List[Int] = xs.fuse.filter(_ % 2 == 0).map(_ * 3).takeRight(4).run.toList

  // ---- run-emitting fused sinks (the deferred single-run / grow assemblies) ----
  def foldAdjacent(xs: FArray[Int]): List[(Int, Int)] = xs.fuse.foldAdjacentBy(_ / 10)(0)(_ + _).run.toList
  def groupAdjacent(xs: FArray[Int]): List[List[Int]] = xs.fuse.groupAdjacentBy(_ / 10).run.toList.map(_.toList)
  def grouped(xs: FArray[Int]): List[List[Int]] = xs.fuse.grouped(3).run.toList.map(_.toList)
  def groupAdjReduce(xs: FArray[Int]): List[(Int, Int)] =
    xs.fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).run.toList

  // ---- reduce/min/max/find sinks (the seeded-accumulator assemblies: best/acc/curKey locals) ----
  def reducePipe(xs: FArray[Int]): Int = xs.fuse.map(_ + 1).reduce(_ + _)
  def reduceRef(xs: FArray[String]): String = xs.fuse.reduce(_ + _)
  def minPipe(xs: FArray[Int]): Int = xs.fuse.filter(_ > 0).min
  def maxByPipe(xs: FArray[String]): String = xs.fuse.maxBy(_.length)
  def minOptionPipe(xs: FArray[Int]): Option[Int] = xs.fuse.map(_ * 2).minOption
  def findPipe(xs: FArray[Int]): Option[Int] = xs.fuse.find(_ > 3)
  def headOptionPipe(xs: FArray[String]): Option[String] = xs.fuse.filter(_.nonEmpty).headOption
  def reduceOptionPipe(xs: FArray[Long]): Option[Long] = xs.fuse.reduceOption(_ + _)
  def groupMapReducePipe(xs: FArray[Int]): Map[Int, Int] = xs.fuse.groupMapReduce(_ % 3)(identity)(_ + _)
  def toVectorPipe(xs: FArray[Int]): Vector[Int] = xs.fuse.map(_ + 1).to(Vector)

  // probe: confirms -Yexplicit-nulls is actually active for this project (a bare null must NOT typecheck).
  def explicitNullsProbe: String = "ok"

  // ---- non-fuse inline ops whose inline body expands at THIS call site ----
  def distinctByInt(xs: FArray[Int]): FArray[Int] = xs.distinctBy(_ % 7)
  def distinctByRef(xs: FArray[String]): FArray[String] = xs.distinctBy(_.length)
  def distinctByLong(xs: FArray[Long]): FArray[Long] = xs.distinctBy(_ % 5L)
  def distinctPlain(xs: FArray[Int]): FArray[Int] = xs.distinct

  // ---- sorted: the RefRepr arm materializes into `var src: Array[Object]` (was a bare `null`) ----
  def sortedRef(xs: FArray[String]): FArray[String] = xs.sorted
  def sortedInt(xs: FArray[Int]): FArray[Int] = xs.sorted
  def sortByRef(xs: FArray[String]): FArray[String] = xs.sortBy(_.length)

  // ---- groupBy: the Int-key arm uses a `${K}Group` sentinel (was a bare `null`) ----
  def groupByIntKeyRefVal(xs: FArray[String]): Map[Int, FArray[String]] = xs.groupBy(_.length)
  def groupByIntKeyIntVal(xs: FArray[Int]): Map[Int, FArray[Int]] = xs.groupBy(_ % 4)
  def groupByRefKey(xs: FArray[String]): Map[Char, FArray[String]] = xs.groupBy(_.charAt(0))
