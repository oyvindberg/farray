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
  @Benchmark def kyochunk(): kyo.Chunk[String] = kyoChunkInput.flatMap(s => kyo.Chunk(s, s)).filter(_.length > 1).take(size)
}

// the Int twin: flatMap doubles the sequence (boxing the inners for the collections, flat int[]
// leaves for FArray), filter runs unboxed, and the trailing take is an O(1) SliceNode for FArray
// versus a full copy for everyone else.
class FlatMapFilterTakeIntBenchmark extends IntInputs {
  @Benchmark def list(): List[Int] = listInput.flatMap(x => List(x, x + 1)).filter(_ % 2 == 0).take(size)
  @Benchmark def farray(): FArray[Int] = farrayInput.flatMap(x => FArray(x, x + 1)).filter(_ % 2 == 0).take(size)
  @Benchmark def iarray(): IArray[Int] = iarrayInput.flatMap(x => IArray(x, x + 1)).filter(_ % 2 == 0).take(size)
  @Benchmark def vector(): Vector[Int] = vectorInput.flatMap(x => Vector(x, x + 1)).filter(_ % 2 == 0).take(size)
  @Benchmark def fs2chunk(): fs2.Chunk[Int] = fs2ChunkInput.flatMap(x => fs2.Chunk(x, x + 1)).filter(_ % 2 == 0).take(size)
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.flatMap(x => zio.Chunk(x, x + 1)).filter(_ % 2 == 0).take(size)
  @Benchmark def kyochunk(): kyo.Chunk[Int] = kyoChunkInput.flatMap(x => kyo.Chunk(x, x + 1)).filter(_ % 2 == 0).take(size)
}
