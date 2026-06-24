package farray

import org.openjdk.jmh.annotations.Benchmark

// concat (++) -> drop -> take
class StrConcatDropTakeBenchmark extends Inputs {
  @Benchmark def list(): List[String] = (listInput ++ listInput).drop(1).take(size)
  @Benchmark def farray(): FArray[String] = (farrayInput ++ farrayInput).drop(1).take(size)
  @Benchmark def iarray(): IArray[String] = (iarrayInput ++ iarrayInput).drop(1).take(size)
  @Benchmark def vector(): Vector[String] = (vectorInput ++ vectorInput).drop(1).take(size)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = (fs2ChunkInput ++ fs2ChunkInput).drop(1).take(size)
  @Benchmark def ziochunk(): zio.Chunk[String] = (zioChunkInput ++ zioChunkInput).drop(1).take(size)
}
