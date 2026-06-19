package farray

import org.openjdk.jmh.annotations.Benchmark

// flatMap -> filter -> take
class FlatMapFilterTakeBenchmark extends Inputs {
  @Benchmark def list(): List[String]     = listInput.flatMap(s => List(s, s)).filter(_.length > 1).take(size)
  @Benchmark def farray(): FArray[String] = farrayInput.flatMap(s => FArray(s, s)).filter(_.length > 1).take(size)
  @Benchmark def iarray(): IArray[String] = iarrayInput.flatMap(s => IArray(s, s)).filter(_.length > 1).take(size)
  @Benchmark def vector(): Vector[String] = vectorInput.flatMap(s => Vector(s, s)).filter(_.length > 1).take(size)
}
