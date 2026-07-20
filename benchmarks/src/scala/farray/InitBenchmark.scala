package farray

import org.openjdk.jmh.annotations.Benchmark

class InitStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.init
  @Benchmark def farray(): FArray[String] = farrayInput.init
  @Benchmark def iarray(): IArray[String] = iarrayInput.init
  @Benchmark def vector(): Vector[String] = vectorInput.init
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.init
  @Benchmark def kyochunk(): kyo.Chunk[String] = kyoChunkInput.init
  // fs2.Chunk has no init
}
