package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}

// Validates wiring a second primitive (Long): FArray[Long].foldLeft should be an unboxed long[] loop
// (ladd), matching raw IArray[Long] and beating boxed List/Vector.
class LongFoldLeftBenchmark extends CommonParams {
  @Param(Array("1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = 1000

  var listInput: List[Long] = _
  var vectorInput: Vector[Long] = _
  var iarrayInput: IArray[Long] = _
  var farrayInput: FArray[Long] = _
  var fs2ChunkInput: fs2.Chunk[Long] = _
  var zioChunkInput: zio.Chunk[Long] = _

  @Setup def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i.toLong)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.unsafeFromArray(arr.clone())
    farrayInput = FArray.tabulate(size)(i => i.toLong)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }

  @Benchmark def list(): Long = listInput.foldLeft(0L)(_ + _)
  @Benchmark def vector(): Long = vectorInput.foldLeft(0L)(_ + _)
  @Benchmark def iarray(): Long = { var acc = 0L; var i = 0; val a = iarrayInput; while (i < a.length) { acc += a(i); i += 1 }; acc }
  @Benchmark def farray(): Long = farrayInput.foldLeft(0L)(_ + _)
  @Benchmark def fs2chunk(): Long = fs2ChunkInput.foldLeft(0L)(_ + _)
  @Benchmark def ziochunk(): Long = zioChunkInput.foldLeft(0L)(_ + _)
}
