package farray

import org.openjdk.jmh.annotations.Benchmark

// flatMap -> filter -> take
class FlatMapFilterTakeStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.flatMap(s => List(s, s)).filter(_.length > 1).take(size)
  // start:eager-chain
  // an ordinary eager chain — no .fuse, three everyday stages, reference elements
  @Benchmark def farray(): FArray[String] = farrayInput.flatMap(s => FArray(s, s)).filter(_.length > 1).take(size)
  // stop:eager-chain
  @Benchmark def iarray(): IArray[String] = iarrayInput.flatMap(s => IArray(s, s)).filter(_.length > 1).take(size)
  @Benchmark def vector(): Vector[String] = vectorInput.flatMap(s => Vector(s, s)).filter(_.length > 1).take(size)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.flatMap(s => fs2.Chunk(s, s)).filter(_.length > 1).take(size)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.flatMap(s => zio.Chunk(s, s)).filter(_.length > 1).take(size)
}
