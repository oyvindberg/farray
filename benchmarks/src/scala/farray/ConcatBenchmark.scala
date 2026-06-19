package farray

import org.openjdk.jmh.annotations.Benchmark

// concatenation of the input with itself
class ConcatBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput ++ listInput
  @Benchmark def farray(): FArray[String] = farrayInput ++ farrayInput
  @Benchmark def iarray(): IArray[String] = iarrayInput ++ iarrayInput
  @Benchmark def vector(): Vector[String] = vectorInput ++ vectorInput
}
