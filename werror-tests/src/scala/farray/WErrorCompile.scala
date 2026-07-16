package farray

/** COMPILE-ONLY gate (project `werror-tests`, built with `-deprecation -Werror`). Each def below expands an inline op body at THIS call site. If any inline
  * body expands a deprecated construct — notably `a -> b` (Predef.ArrowAssoc, deprecated under recent dotty) in `groupBy`/`groupMap` — the deprecation surfaces
  * here as a hard error, exactly as it did in the Scala 3 compiler (a `-Werror` codebase that needed a `-Wconf` exclusion). If this project compiles clean, the
  * inline surface is `-Werror`-safe.
  */
object WErrorCompile:

  def groupByInt(xs: FArray[Int]): Map[Int, FArray[Int]] = xs.groupBy(_ % 3)
  def groupByRef(xs: FArray[String]): Map[Int, FArray[String]] = xs.groupBy(_.length)
  def groupByLong(xs: FArray[Long]): Map[Long, FArray[Long]] = xs.groupBy(_ % 5L)
  // single-element inputs hit the `Map(k -> v)` fast-path arms specifically
  def groupByOne(xs: FArray[Int]): Map[Int, FArray[Int]] = xs.take(1).groupBy(identity)
  def groupMapInt(xs: FArray[Int]): Map[Int, FArray[Int]] = xs.groupMap(_ % 3)(_ + 1)
  def groupMapRef(xs: FArray[String]): Map[Int, FArray[Int]] = xs.groupMap(_.length)(_.size)
  def groupMapOne(xs: FArray[Int]): Map[Int, FArray[Int]] = xs.take(1).groupMap(identity)(_ * 2)
  def groupMapReduceInt(xs: FArray[Int]): Map[Int, Int] = xs.groupMapReduce(_ % 3)(identity)(_ + _)
  def zipWithIndexInt(xs: FArray[Int]): FArray[(Int, Int)] = xs.zipWithIndex
  def zipWithIndexRef(xs: FArray[String]): FArray[(String, Int)] = xs.zipWithIndex
  def distinctByInt(xs: FArray[Int]): FArray[Int] = xs.distinctBy(_ % 7)
