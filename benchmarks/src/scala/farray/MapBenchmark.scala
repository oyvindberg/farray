package farray

import org.openjdk.jmh.annotations.Benchmark

class MapBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.map(x => x + x)
  @Benchmark def farray(): FArray[String] = farrayInput.map(x => x + x)
  @Benchmark def iarray(): IArray[String] = iarrayInput.map(x => x + x)
  @Benchmark def vector(): Vector[String] = vectorInput.map(x => x + x)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.map(x => x + x)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.map(x => x + x)
}
