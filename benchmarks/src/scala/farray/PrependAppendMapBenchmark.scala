package farray

import org.openjdk.jmh.annotations.Benchmark

// prepend (+:) -> append (:+) -> map
class PrependAppendMapBenchmark extends Inputs {
  @Benchmark def list(): List[String] = (("y" +: listInput) :+ "z").map(_ + "_")
  @Benchmark def farray(): FArray[String] = (("y" +: farrayInput) :+ "z").map(_ + "_")
  @Benchmark def iarray(): IArray[String] = (("y" +: iarrayInput) :+ "z").map(_ + "_")
  @Benchmark def vector(): Vector[String] = (("y" +: vectorInput) :+ "z").map(_ + "_")
  @Benchmark def ziochunk(): zio.Chunk[String] = (("y" +: zioChunkInput) :+ "z").map(_ + "_")
  // fs2.Chunk has no +: (prepend-one) / :+ (append-one)
}
