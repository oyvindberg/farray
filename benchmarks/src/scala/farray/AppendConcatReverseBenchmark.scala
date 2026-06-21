package farray

import org.openjdk.jmh.annotations.Benchmark

// append (:+) -> concat (++) -> reverse
class AppendConcatReverseBenchmark extends Inputs {
  @Benchmark def list(): List[String]     = ((listInput :+ "y") ++ listInput).reverse
  @Benchmark def farray(): FArray[String] = ((farrayInput :+ "y") ++ farrayInput).reverse
  @Benchmark def iarray(): IArray[String] = ((iarrayInput :+ "y") ++ iarrayInput).reverse
  @Benchmark def vector(): Vector[String] = ((vectorInput :+ "y") ++ vectorInput).reverse
  @Benchmark def ziochunk(): zio.Chunk[String] = ((zioChunkInput :+ "y") ++ zioChunkInput).reverse
  // fs2.Chunk has no :+ (append-one) nor reverse
}
