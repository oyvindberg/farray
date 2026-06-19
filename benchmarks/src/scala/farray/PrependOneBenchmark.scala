package farray

import org.openjdk.jmh.annotations.Benchmark

// single-element prepend
class PrependOneBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = "x" +: listInput
  @Benchmark def farray(): FArray[String] = "x" +: farrayInput
  @Benchmark def iarray(): IArray[String] = "x" +: iarrayInput
  @Benchmark def vector(): Vector[String] = "x" +: vectorInput
}
