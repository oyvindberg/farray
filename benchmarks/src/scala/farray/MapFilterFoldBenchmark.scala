package farray

import org.openjdk.jmh.annotations.Benchmark

// map -> filter -> foldLeft
class MapFilterFoldStrBenchmark extends Inputs {
  //start:bench-mapfilterfold
  // the SAME pipeline, only the backing collection differs — that is the whole apples-to-apples premise
  @Benchmark def list(): Int = listInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  //stop:bench-mapfilterfold
  @Benchmark def iarray(): Int = iarrayInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
}
