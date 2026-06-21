package farray

import org.openjdk.jmh.annotations.Benchmark

// drop -> concat (++) -> foldLeft
class DropConcatFoldBenchmark extends Inputs {
  @Benchmark def list(): Int   = (listInput.drop(1) ++ listInput).foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = (farrayInput.drop(1) ++ farrayInput).foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = (iarrayInput.drop(1) ++ iarrayInput).foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = (vectorInput.drop(1) ++ vectorInput).foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = (fs2ChunkInput.drop(1) ++ fs2ChunkInput).foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = (zioChunkInput.drop(1) ++ zioChunkInput).foldLeft(0)(_ + _.length)
}
