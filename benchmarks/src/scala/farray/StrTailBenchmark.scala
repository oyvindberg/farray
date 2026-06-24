package farray

import org.openjdk.jmh.annotations.Benchmark

class StrTailBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.tail
  @Benchmark def farray(): FArray[String] = farrayInput.tail
  @Benchmark def iarray(): IArray[String] = iarrayInput.tail
  @Benchmark def vector(): Vector[String] = vectorInput.tail
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.tail
  // fs2.Chunk has no tail (use drop(1) instead, but no direct tail)
}
