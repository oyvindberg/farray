package farray

import org.openjdk.jmh.annotations.Benchmark

// flatMap doubling each element, then fold to force traversal. One-pass primitive: each f(x) is appended
// straight into a growing output (no inner buffer), so escape analysis drops the inner wrapper.
class FlatMapIntBenchmark extends IntInputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(x => FArray(x, x)).foldLeft(0)(_ + _)
  @Benchmark def list(): Int   = listInput.flatMap(x => List(x, x)).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.flatMap(x => Vector(x, x)).foldLeft(0)(_ + _)
  @Benchmark def array(): Int  = arrayInput.flatMap(x => Array(x, x)).foldLeft(0)(_ + _)
}

class FlatMapStringBenchmark extends Inputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(s => FArray(s, s)).foldLeft(0)((acc, s) => acc + s.length)
  @Benchmark def list(): Int   = listInput.flatMap(s => List(s, s)).foldLeft(0)((acc, s) => acc + s.length)
  @Benchmark def vector(): Int = vectorInput.flatMap(s => Vector(s, s)).foldLeft(0)((acc, s) => acc + s.length)
  @Benchmark def iarray(): Int = iarrayInput.flatMap(s => IArray(s, s)).foldLeft(0)((acc, s) => acc + s.length)
}
