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
}
