package farray

import org.openjdk.jmh.annotations.Benchmark

class FilterBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput.filter(_.length > 1)
  @Benchmark def farray(): FArray[String] = farrayInput.filter(_.length > 1)
  @Benchmark def iarray(): IArray[String] = iarrayInput.filter(_.length > 1)
  @Benchmark def vector(): Vector[String] = vectorInput.filter(_.length > 1)
}
