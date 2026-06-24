package farray

import org.openjdk.jmh.annotations.Benchmark

// sum-style fold over string lengths
class StrFoldLeftBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = iarrayInput.foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.foldLeft(0)(_ + _.length)
}
