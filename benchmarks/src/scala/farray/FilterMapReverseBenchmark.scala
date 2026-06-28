package farray

import org.openjdk.jmh.annotations.Benchmark

// filter -> map -> reverse
class FilterMapReverseStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def farray(): FArray[String] = farrayInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def iarray(): IArray[String] = iarrayInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def vector(): Vector[String] = vectorInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  // fs2.Chunk has no reverse
}
