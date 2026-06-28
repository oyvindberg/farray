package farray

import org.openjdk.jmh.annotations.Benchmark

// drop -> take -> map
class DropTakeMapStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def farray(): FArray[String] = farrayInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def iarray(): IArray[String] = iarrayInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def vector(): Vector[String] = vectorInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.drop(1).take(size / 2).map(_ + "!")
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.drop(1).take(size / 2).map(_ + "!")
}
