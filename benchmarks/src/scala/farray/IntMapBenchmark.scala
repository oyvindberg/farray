package farray

import org.openjdk.jmh.annotations.Benchmark

// map(_ + 1) over Ints. IArray.map keeps int[] storage but boxes through the generic map function;
// FArray's specialized inline map stays unboxed int[] -> int[].
class IntMapBenchmark extends IntInputs {
  @Benchmark def list(): List[Int]     = listInput.map(_ + 1)
  @Benchmark def vector(): Vector[Int] = vectorInput.map(_ + 1)
  @Benchmark def iarray(): IArray[Int] = iarrayInput.map(_ + 1)
  @Benchmark def farray(): FArray[Int] = farrayInput.map(_ + 1)
}
