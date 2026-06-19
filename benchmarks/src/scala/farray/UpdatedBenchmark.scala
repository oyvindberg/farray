package farray

import org.openjdk.jmh.annotations.Benchmark

// single-element update at a non-constant index
class UpdatedBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput.updated(size / 2, "x")
  @Benchmark def farray(): FArray[String] = farrayInput.updated(size / 2, "x")
  @Benchmark def iarray(): IArray[String] = iarrayInput.updated(size / 2, "x")
  @Benchmark def vector(): Vector[String] = vectorInput.updated(size / 2, "x")
}
