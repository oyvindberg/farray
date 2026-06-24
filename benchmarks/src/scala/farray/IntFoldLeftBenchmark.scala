package farray

import org.openjdk.jmh.annotations.Benchmark

// Sum of Ints. `iarray` is the raw unboxed int[] baseline; List/Vector.foldLeft
// all box the Int through a generic Function2; FArray's specialized inline foldLeft should match `iarray`.
class IntFoldLeftBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.foldLeft(0)(_ + _)
  @Benchmark def farray(): Int = farrayInput.foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.foldLeft(0)(_ + _)
}
