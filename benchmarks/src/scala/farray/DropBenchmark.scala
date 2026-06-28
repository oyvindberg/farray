package farray

import org.openjdk.jmh.annotations.Benchmark

class DropStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.drop(size / 2)
  @Benchmark def farray(): FArray[String] = farrayInput.drop(size / 2)
  @Benchmark def iarray(): IArray[String] = iarrayInput.drop(size / 2)
  @Benchmark def vector(): Vector[String] = vectorInput.drop(size / 2)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.drop(size / 2)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.drop(size / 2)
}
