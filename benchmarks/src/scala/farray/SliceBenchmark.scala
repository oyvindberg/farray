package farray

import org.openjdk.jmh.annotations.Benchmark

class SliceBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput.slice(size / 4, (size * 3) / 4)
  @Benchmark def farray(): FArray[String] = farrayInput.slice(size / 4, (size * 3) / 4)
  @Benchmark def iarray(): IArray[String] = iarrayInput.slice(size / 4, (size * 3) / 4)
  @Benchmark def vector(): Vector[String] = vectorInput.slice(size / 4, (size * 3) / 4)
}
