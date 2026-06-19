package farray

import org.openjdk.jmh.annotations.Benchmark

class ReverseBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput.reverse
  @Benchmark def farray(): FArray[String] = farrayInput.reverse
  @Benchmark def iarray(): IArray[String] = iarrayInput.reverse
  @Benchmark def vector(): Vector[String] = vectorInput.reverse
}
