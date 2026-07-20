package farray

import org.openjdk.jmh.annotations.Benchmark

// collect = filter + map through one PartialFunction. The PF boundary is generic for EVERYONE
// (isDefinedAt/apply box primitives per element — that tax is inherent to PartialFunction); what
// differs is the storage: FArray reads an int[] and writes the survivors into a growable primitive
// buffer, competitors read and write boxed. (The version with NO PartialFunction at all is
// `.fuse.collect` — the fusion pages.)
class CollectIntBenchmark extends IntInputs {
  @Benchmark def list(): List[Int] = listInput.collect { case x if x % 2 == 0 => x * 2 }
  @Benchmark def farray(): FArray[Int] = farrayInput.collect { case x if x % 2 == 0 => x * 2 }
  @Benchmark def iarray(): IArray[Int] = iarrayInput.collect { case x if x % 2 == 0 => x * 2 }
  @Benchmark def vector(): Vector[Int] = vectorInput.collect { case x if x % 2 == 0 => x * 2 }
  @Benchmark def fs2chunk(): fs2.Chunk[Int] = fs2ChunkInput.collect { case x if x % 2 == 0 => x * 2 }
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.collect { case x if x % 2 == 0 => x * 2 }
  @Benchmark def kyochunk(): kyo.Chunk[Int] = kyoChunkInput.collect { case x if x % 2 == 0 => x * 2 }
}

class CollectStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.collect { case s if s.length > 1 => s + "!" }
  @Benchmark def farray(): FArray[String] = farrayInput.collect { case s if s.length > 1 => s + "!" }
  @Benchmark def iarray(): IArray[String] = iarrayInput.collect { case s if s.length > 1 => s + "!" }
  @Benchmark def vector(): Vector[String] = vectorInput.collect { case s if s.length > 1 => s + "!" }
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.collect { case s if s.length > 1 => s + "!" }
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.collect { case s if s.length > 1 => s + "!" }
  @Benchmark def kyochunk(): kyo.Chunk[String] = kyoChunkInput.collect { case s if s.length > 1 => s + "!" }
}
