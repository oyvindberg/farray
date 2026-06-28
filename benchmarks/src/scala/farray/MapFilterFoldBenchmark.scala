package farray

import org.openjdk.jmh.annotations.Benchmark

// map -> filter -> foldLeft
class MapFilterFoldStrBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = iarrayInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
}
