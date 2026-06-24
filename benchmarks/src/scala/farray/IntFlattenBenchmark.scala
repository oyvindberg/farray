package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import cats.syntax.all.* // fs2.Chunk flatten comes from cats FlatMap

// Replicates fs2 ChunkFlatten + zio flatMap-to-tiny: flatten `numChunks` inner
// collections of `innerSize` Ints each into one flat collection.
// innerSize=1,2 are the deeply-fragmented worst case.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class IntFlattenBenchmark:
  @Param(Array("32", "128")) var numChunks: Int = 32
  @Param(Array("1", "2", "10")) var innerSize: Int = 1

  // outer structures of inner collections, one per impl
  var farrayOuter: FArray[FArray[Int]] = _
  var ziochunkOuter: zio.Chunk[zio.Chunk[Int]] = _
  var fs2chunkOuter: fs2.Chunk[fs2.Chunk[Int]] = _
  var vectorOuter: Vector[Vector[Int]] = _
  var listOuter: List[List[Int]] = _

  @Setup def setup(): Unit =
    farrayOuter = FArray.tabulate(numChunks)(c => FArray.tabulate(innerSize)(i => c * innerSize + i))
    ziochunkOuter = zio.Chunk.fromIterable(
      (0 until numChunks).map(c => zio.Chunk.fromIterable((0 until innerSize).map(i => c * innerSize + i)))
    )
    fs2chunkOuter = fs2.Chunk.from(
      (0 until numChunks).map(c => fs2.Chunk.from((0 until innerSize).map(i => c * innerSize + i)))
    )
    vectorOuter = Vector.tabulate(numChunks)(c => Vector.tabulate(innerSize)(i => c * innerSize + i))
    listOuter = List.tabulate(numChunks)(c => List.tabulate(innerSize)(i => c * innerSize + i))

  @Benchmark def farray_flatten(): FArray[Int] = farrayOuter.flatMap(x => x)
  @Benchmark def ziochunk_flatten(): zio.Chunk[Int] = ziochunkOuter.flatten
  @Benchmark def fs2chunk_flatten(): fs2.Chunk[Int] = fs2chunkOuter.flatten
  @Benchmark def vector_flatten(): Vector[Int] = vectorOuter.flatten
  @Benchmark def list_flatten(): List[Int] = listOuter.flatten
