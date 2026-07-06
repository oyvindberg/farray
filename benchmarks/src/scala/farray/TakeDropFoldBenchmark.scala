package farray

import org.openjdk.jmh.annotations.Benchmark

// take -> drop -> foldLeft
class TakeDropFoldStrBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.take(size - 1).drop(1).foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.take(size - 1).drop(1).foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = iarrayInput.take(size - 1).drop(1).foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.take(size - 1).drop(1).foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.take(size - 1).drop(1).foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.take(size - 1).drop(1).foldLeft(0)(_ + _.length)
  @Benchmark def javastream(): Int = java.util.Arrays.stream(strArrInput).limit(size - 1).skip(1).mapToInt(_.length).sum
}

// the Int twin: take/drop are O(1) lazy windows for FArray, then an unboxed fold over the slice.
class TakeDropFoldIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.take(size - 1).drop(1).foldLeft(0)(_ + _)
  @Benchmark def farray(): Int = farrayInput.take(size - 1).drop(1).foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.take(size - 1).drop(1).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.take(size - 1).drop(1).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.take(size - 1).drop(1).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.take(size - 1).drop(1).foldLeft(0)(_ + _)
  @Benchmark def javastream(): Int = java.util.Arrays.stream(arrInput).limit(size - 1).skip(1).reduce(0, _ + _)
}
