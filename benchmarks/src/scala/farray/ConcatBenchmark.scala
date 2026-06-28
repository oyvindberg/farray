package farray

import org.openjdk.jmh.annotations.Benchmark

// concatenation of the input with itself
class ConcatStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput ++ listInput
  @Benchmark def farray(): FArray[String] = farrayInput ++ farrayInput
  @Benchmark def iarray(): IArray[String] = iarrayInput ++ iarrayInput
  @Benchmark def vector(): Vector[String] = vectorInput ++ vectorInput
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput ++ fs2ChunkInput
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput ++ zioChunkInput
}
