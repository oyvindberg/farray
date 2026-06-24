package farray

import org.openjdk.jmh.annotations.Benchmark

class StrInitBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.init
  @Benchmark def farray(): FArray[String] = farrayInput.init
  @Benchmark def iarray(): IArray[String] = iarrayInput.init
  @Benchmark def vector(): Vector[String] = vectorInput.init
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.init
  // fs2.Chunk has no init
}
