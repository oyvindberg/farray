package farray

import org.openjdk.jmh.annotations.Benchmark

class TailBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput.tail
  @Benchmark def farray(): FArray[String] = farrayInput.tail
  @Benchmark def iarray(): IArray[String] = iarrayInput.tail
  @Benchmark def vector(): Vector[String] = vectorInput.tail
}
