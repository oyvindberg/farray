package farray

import org.openjdk.jmh.annotations.Benchmark

class LastBenchmark extends Inputs {
  @Benchmark def list(): String   = listInput.last
  @Benchmark def farray(): String = farrayInput.last
  @Benchmark def iarray(): String = iarrayInput.last
  @Benchmark def vector(): String = vectorInput.last
}
