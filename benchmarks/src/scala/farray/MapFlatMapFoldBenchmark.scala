package farray

import org.openjdk.jmh.annotations.Benchmark

// map -> flatMap -> foldLeft
class MapFlatMapFoldBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.map(_ + "a").flatMap(s => List(s, s)).foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.map(_ + "a").flatMap(s => FArray(s, s)).foldLeft(0)(_ + _.length)
  @Benchmark def iarray(): Int = iarrayInput.map(_ + "a").flatMap(s => IArray(s, s)).foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.map(_ + "a").flatMap(s => Vector(s, s)).foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.map(_ + "a").flatMap(s => fs2.Chunk(s, s)).foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.map(_ + "a").flatMap(s => zio.Chunk(s, s)).foldLeft(0)(_ + _.length)
}
