package farray

import org.openjdk.jmh.annotations.Benchmark

class DropBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput.drop(size / 2)
  @Benchmark def farray(): FArray[String] = farrayInput.drop(size / 2)
  @Benchmark def iarray(): IArray[String] = iarrayInput.drop(size / 2)
  @Benchmark def vector(): Vector[String] = vectorInput.drop(size / 2)
}
