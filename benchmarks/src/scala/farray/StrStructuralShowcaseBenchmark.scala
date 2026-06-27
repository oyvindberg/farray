package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Ref-kind (String) twin of StructuralShowcaseBenchmark. Same VERY LONG INTERLEAVED chain
// of structural ops (prepend +: , append :+ , concat ++ a small fixed leaf, reverse) then a
// final .map(_ + "!") that FORCES materialization — but on Object[]-backed (reference) leaves
// instead of int[]. Proves the structural advantage is kind-agnostic: every farray op is a
// single O(1) tree node (Prepend/Append/Concat/ReverseNode), so the whole chain plus the one
// final traversal in .map is O(n); the array-copying competitors copy the backing array on
// every prepend/append/concat, so they are O(n^2).
//
// FAIRNESS: every impl runs the SAME logical sequence and produces the same logical result;
// element is a fixed string (so the measurement is the STRUCTURE + traversal, not toString),
// growth is STRICTLY LINEAR (concat is a small fixed 3-element leaf, never c ++ c), and every
// impl ends with the same .map so all materialize. Sizes stop at 10000 (the O(n^2) slowness
// at 10000 IS the point).
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class StrStructuralShowcaseBenchmark:
  @Param(Array("1000", "10000"))
  var size: Int = 0

  val elem = "x"

  // The small fixed leaf, built once per impl in @Setup. Concat-ing it keeps growth linear.
  var leafFarray: FArray[String] = _
  var leafIArray: IArray[String] = _
  var leafList: List[String] = _
  var leafVector: Vector[String] = _
  var leafZio: zio.Chunk[String] = _
  var leafFs2: fs2.Chunk[String] = _

  @Setup def setup(): Unit =
    leafFarray = FArray("a", "b", "c")
    leafIArray = IArray("a", "b", "c")
    leafList = List("a", "b", "c")
    leafVector = Vector("a", "b", "c")
    leafZio = zio.Chunk("a", "b", "c")
    leafFs2 = fs2.Chunk("a", "b", "c")

  @Benchmark def farray_showcase(): FArray[String] =
    var c: FArray[String] = FArray(elem)
    var i = 1
    while i < size do
      c = elem +: c
      c = c :+ elem
      if i % 4 == 0 then c = leafFarray ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + "!")

  @Benchmark def iarray_showcase(): IArray[String] =
    var c: IArray[String] = IArray(elem)
    var i = 1
    while i < size do
      c = elem +: c
      c = c :+ elem
      if i % 4 == 0 then c = leafIArray ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + "!")

  @Benchmark def list_showcase(): List[String] =
    var c: List[String] = List(elem)
    var i = 1
    while i < size do
      c = elem +: c
      c = c :+ elem
      if i % 4 == 0 then c = leafList ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + "!")

  @Benchmark def vector_showcase(): Vector[String] =
    var c: Vector[String] = Vector(elem)
    var i = 1
    while i < size do
      c = elem +: c
      c = c :+ elem
      if i % 4 == 0 then c = leafVector ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + "!")

  @Benchmark def ziochunk_showcase(): zio.Chunk[String] =
    var c: zio.Chunk[String] = zio.Chunk(elem)
    var i = 1
    while i < size do
      c = elem +: c
      c = c :+ elem
      if i % 4 == 0 then c = leafZio ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + "!")

  // fs2.Chunk lacks native prepend-one / append-one / reverse; use idiomatic equivalents for
  // the SAME logical result: prepend = singleton(i) ++ c, append = c ++ singleton(i),
  // reverse = re-wrap the reversed backing array.
  @Benchmark def fs2chunk_showcase(): fs2.Chunk[String] =
    var c: fs2.Chunk[String] = fs2.Chunk(elem)
    var i = 1
    while i < size do
      c = fs2.Chunk.singleton(elem) ++ c
      c = c ++ fs2.Chunk.singleton(elem)
      if i % 4 == 0 then c = leafFs2 ++ c
      if i % 8 == 0 then c = fs2.Chunk.array(c.toArray.reverse)
      i += 1
    c.map(_ + "!")
