package farray

import org.openjdk.jmh.annotations.Benchmark

// single-element append
class StrAppendOneBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput :+ "x"
  @Benchmark def farray(): FArray[String] = farrayInput :+ "x"
  @Benchmark def iarray(): IArray[String] = iarrayInput :+ "x"
  @Benchmark def vector(): Vector[String] = vectorInput :+ "x"
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput :+ "x"
  // fs2.Chunk has no :+ (append-one)
}
