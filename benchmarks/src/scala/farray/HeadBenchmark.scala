package farray

import org.openjdk.jmh.annotations.Benchmark

class HeadBenchmark extends Inputs {
  @Benchmark def list(): String   = listInput.head
  @Benchmark def farray(): String = farrayInput.head
  @Benchmark def iarray(): String = iarrayInput.head
  @Benchmark def vector(): String = vectorInput.head
}
