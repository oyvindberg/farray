package farray

import org.openjdk.jmh.annotations.Benchmark

// flatMap + flatMap + map chained, then fold to force it. Each FArray flatMap produces ONE flat leaf, so
// the next flatMap/map reads a tight array (no Concat-chain pointer-chasing compounding across the chain).

class IntFlatMapChainBenchmark extends IntInputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(x => FArray(x, x + 1)).flatMap(y => FArray(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def list(): Int = listInput.flatMap(x => List(x, x + 1)).flatMap(y => List(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.flatMap(x => Vector(x, x + 1)).flatMap(y => Vector(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def array(): Int = arrayInput.flatMap(x => Array(x, x + 1)).flatMap(y => Array(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.flatMap(x => fs2.Chunk(x, x + 1)).flatMap(y => fs2.Chunk(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.flatMap(x => zio.Chunk(x, x + 1)).flatMap(y => zio.Chunk(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
}

class StrFlatMapChainBenchmark extends Inputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(s => FArray(s, s)).flatMap(t => FArray(t, t + "x")).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def list(): Int = listInput.flatMap(s => List(s, s)).flatMap(t => List(t, t + "x")).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.flatMap(s => Vector(s, s)).flatMap(t => Vector(t, t + "x")).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.flatMap(s => IArray(s, s)).flatMap(t => IArray(t, t + "x")).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.flatMap(s => fs2.Chunk(s, s)).flatMap(t => fs2.Chunk(t, t + "x")).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.flatMap(s => zio.Chunk(s, s)).flatMap(t => zio.Chunk(t, t + "x")).map(_.length).foldLeft(0)(_ + _)
}
