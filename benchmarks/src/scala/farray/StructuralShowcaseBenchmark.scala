package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// THE grand showcase. Builds a VERY LONG INTERLEAVED chain of structural ops
// (prepend +: , append :+ , concat ++ a small fixed leaf, reverse) and then forces
// materialization with a final .map(_ + 1).
//
// For farray every structural op is O(1) -- it allocates a single tree node
// (Prepend/Append/Concat/ReverseNode) -- so the whole chain plus the single final
// traversal in .map is O(n) total. For the array-copying competitors (IArray, fs2/zio
// Chunk) every prepend/append/concat copies the whole backing array, so the chain is
// O(n^2). List prepend is O(1) but its append (:+) and concat are O(n), so it is also
// O(n^2). Vector is O(log n) per op, ~O(n log n) overall.
//
// FAIRNESS: every impl runs the SAME logical sequence and produces the same logical
// result. The growth is kept STRICTLY LINEAR -- the concat step concats a SMALL FIXED
// 3-element leaf (never `c ++ c`, which would double the size and blow up exponentially).
//
// Sizes stop at 10000: at 10000 the O(n^2) impls are already slow, and that slowness
// IS the point of the showcase.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class StructuralShowcaseBenchmark:
  @Param(Array("1000", "10000"))
  var size: Int = 0

  // The small fixed leaf, built once per impl in @Setup. Concat-ing it keeps growth linear.
  var leafFarray: FArray[Int] = _
  var leafIArray: IArray[Int] = _
  var leafList: List[Int] = _
  var leafVector: Vector[Int] = _
  var leafZio: zio.Chunk[Int] = _
  var leafFs2: fs2.Chunk[Int] = _

  @Setup def setup(): Unit =
    leafFarray = FArray(-1, -2, -3)
    leafIArray = IArray(-1, -2, -3)
    leafList = List(-1, -2, -3)
    leafVector = Vector(-1, -2, -3)
    leafZio = zio.Chunk(-1, -2, -3)
    leafFs2 = fs2.Chunk(-1, -2, -3)

  @Benchmark def farray_showcase(): FArray[Int] =
    var c: FArray[Int] = FArray(0)
    var i = 1
    while i < size do
      c = i +: c
      c = c :+ i
      if i % 4 == 0 then c = leafFarray ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + 1)

  @Benchmark def iarray_showcase(): IArray[Int] =
    var c: IArray[Int] = IArray(0)
    var i = 1
    while i < size do
      c = i +: c
      c = c :+ i
      if i % 4 == 0 then c = leafIArray ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + 1)

  @Benchmark def list_showcase(): List[Int] =
    var c: List[Int] = List(0)
    var i = 1
    while i < size do
      c = i +: c
      c = c :+ i
      if i % 4 == 0 then c = leafList ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + 1)

  @Benchmark def vector_showcase(): Vector[Int] =
    var c: Vector[Int] = Vector(0)
    var i = 1
    while i < size do
      c = i +: c
      c = c :+ i
      if i % 4 == 0 then c = leafVector ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + 1)

  @Benchmark def ziochunk_showcase(): zio.Chunk[Int] =
    var c: zio.Chunk[Int] = zio.Chunk(0)
    var i = 1
    while i < size do
      c = i +: c
      c = c :+ i
      if i % 4 == 0 then c = leafZio ++ c
      if i % 8 == 0 then c = c.reverse
      i += 1
    c.map(_ + 1)

  // fs2.Chunk lacks native prepend-one / append-one / reverse; use idiomatic equivalents
  // for the SAME logical result: prepend = singleton(i) ++ c, append = c ++ singleton(i),
  // reverse = re-wrap the reversed backing array.
  @Benchmark def fs2chunk_showcase(): fs2.Chunk[Int] =
    var c: fs2.Chunk[Int] = fs2.Chunk(0)
    var i = 1
    while i < size do
      c = fs2.Chunk.singleton(i) ++ c
      c = c ++ fs2.Chunk.singleton(i)
      if i % 4 == 0 then c = leafFs2 ++ c
      if i % 8 == 0 then c = fs2.Chunk.array(c.toArray.reverse)
      i += 1
    c.map(_ + 1)
