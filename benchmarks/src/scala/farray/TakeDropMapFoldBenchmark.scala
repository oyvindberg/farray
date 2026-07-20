package farray

import org.openjdk.jmh.annotations.Benchmark

// take -> drop -> map -> foldLeft
class TakeDropMapFoldStrBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = iarrayInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def kyochunk(): Int = kyoChunkInput.take(size - 1).drop(1).map(_ + "x").foldLeft(0)(_ + _.length)
  @Benchmark def javastream(): Int = java.util.Arrays.stream(strArrInput).limit(size - 1).skip(1).map(_ + "x").mapToInt(_.length).sum
}

// the Int twin: take/drop are O(1) lazy windows for FArray, then an unboxed map and fold over the slice.
class TakeDropMapFoldIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def farray(): Int = farrayInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def kyochunk(): Int = kyoChunkInput.take(size - 1).drop(1).map(_ * 3).foldLeft(0)(_ + _)
  @Benchmark def javastream(): Int = java.util.Arrays.stream(arrInput).limit(size - 1).skip(1).map(_ * 3).reduce(0, _ + _)
}
