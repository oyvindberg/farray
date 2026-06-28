package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}

// Sum of Ints. `iarray` is the raw unboxed int[] baseline; List/Vector.foldLeft
// all box the Int through a generic Function2; FArray's specialized inline foldLeft should match `iarray`.
class FoldLeftIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.foldLeft(0)(_ + _)
  @Benchmark def farray(): Int = farrayInput.foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.foldLeft(0)(_ + _)
}

// Validates wiring a second primitive (Long): FArray[Long].foldLeft should be an unboxed long[] loop
// (ladd), matching raw IArray[Long] and beating boxed List/Vector.
class FoldLeftLongBenchmark extends CommonParams {
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

// sum-style fold over string lengths
class FoldLeftStrBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = iarrayInput.foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.foldLeft(0)(_ + _.length)
}
