package farray

import org.openjdk.jmh.annotations.Benchmark

// concat (++) -> drop -> take
class ConcatDropTakeBenchmark extends Inputs {
  @Benchmark def list(): List[String]     = (listInput ++ listInput).drop(1).take(size)
  @Benchmark def farray(): FArray[String] = (farrayInput ++ farrayInput).drop(1).take(size)
  @Benchmark def iarray(): IArray[String] = (iarrayInput ++ iarrayInput).drop(1).take(size)
  @Benchmark def vector(): Vector[String] = (vectorInput ++ vectorInput).drop(1).take(size)
}
