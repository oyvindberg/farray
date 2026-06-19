package farray

import org.openjdk.jmh.annotations.Benchmark

// Sum of Ints. `array` is the raw unboxed int[] while-loop gold standard; List/Vector/IArray.foldLeft
// all box the Int through a generic Function2; FArray's specialized inline foldLeft should match `array`.
class IntFoldLeftBenchmark extends IntInputs {
  @Benchmark def list(): Int   = listInput.foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.foldLeft(0)(_ + _)
  @Benchmark def array(): Int  = { var acc = 0; var i = 0; val a = arrayInput; while (i < a.length) { acc += a(i); i += 1 }; acc }
  @Benchmark def farray(): Int = farrayInput.foldLeft(0)(_ + _)
}
