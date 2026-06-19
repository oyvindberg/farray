package farray

import org.openjdk.jmh.annotations.Benchmark

// drop -> take -> map
class DropTakeMapBenchmark extends Inputs {
  @Benchmark def list(): List[String]     = listInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def farray(): FArray[String] = farrayInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def iarray(): IArray[String] = iarrayInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def vector(): Vector[String] = vectorInput.drop(1).take(size / 2).map(_ + "!")
}
