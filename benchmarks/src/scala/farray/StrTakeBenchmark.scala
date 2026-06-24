package farray

import org.openjdk.jmh.annotations.Benchmark

class StrTakeBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.take(size / 2)
  @Benchmark def farray(): FArray[String] = farrayInput.take(size / 2)
  @Benchmark def iarray(): IArray[String] = iarrayInput.take(size / 2)
  @Benchmark def vector(): Vector[String] = vectorInput.take(size / 2)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.take(size / 2)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.take(size / 2)
}
