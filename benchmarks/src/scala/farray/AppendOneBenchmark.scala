package farray

import org.openjdk.jmh.annotations.Benchmark

// single-element append
class AppendOneBenchmark extends Inputs {
  @Benchmark def list(): List[String]   = listInput :+ "x"
  @Benchmark def farray(): FArray[String] = farrayInput :+ "x"
  @Benchmark def iarray(): IArray[String] = iarrayInput :+ "x"
  @Benchmark def vector(): Vector[String] = vectorInput :+ "x"
}
