package farray

import org.openjdk.jmh.annotations.Benchmark

// flatMap doubling each element. FArray collects the inner arrays, precomputes the total length, allocates
// one flat result and System.arraycopy's each inner in — no deep Concat chain. Then fold to force traversal.
class FlatMapBenchmark extends IntInputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(x => FArray(x, x)).foldLeft(0)(_ + _)
  @Benchmark def list(): Int   = listInput.flatMap(x => List(x, x)).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.flatMap(x => Vector(x, x)).foldLeft(0)(_ + _)
  @Benchmark def array(): Int  = arrayInput.flatMap(x => Array(x, x)).foldLeft(0)(_ + _)
}
