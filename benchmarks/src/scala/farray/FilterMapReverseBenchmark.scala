package farray

import org.openjdk.jmh.annotations.Benchmark

// filter -> map -> reverse
class FilterMapReverseStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def farray(): FArray[String] = farrayInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def iarray(): IArray[String] = iarrayInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def vector(): Vector[String] = vectorInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.filter(_.nonEmpty).map(_.toUpperCase).reverse
  // java.util.stream has no reverse: the idiomatic equivalent collects then reverses the array (its honest cost).
  @Benchmark def javastream(): Array[Object] = java.util.Arrays.stream(strArrInput).filter(_.nonEmpty).map(_.toUpperCase).toArray.reverse
  // fs2.Chunk has no reverse
}

// the Int twin: branchless unboxed filter -> unboxed map -> O(1) ReverseNode.
class FilterMapReverseIntBenchmark extends IntInputs {
  @Benchmark def list(): List[Int] = listInput.filter(_ % 2 == 0).map(_ * 3).reverse
  @Benchmark def farray(): FArray[Int] = farrayInput.filter(_ % 2 == 0).map(_ * 3).reverse
  @Benchmark def iarray(): IArray[Int] = iarrayInput.filter(_ % 2 == 0).map(_ * 3).reverse
  @Benchmark def vector(): Vector[Int] = vectorInput.filter(_ % 2 == 0).map(_ * 3).reverse
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.filter(_ % 2 == 0).map(_ * 3).reverse
  // java.util.stream has no reverse: the idiomatic equivalent collects then reverses the array (its honest cost).
  @Benchmark def javastream(): Array[Int] = java.util.Arrays.stream(arrInput).filter(_ % 2 == 0).map(_ * 3).toArray.reverse
  // fs2.Chunk has no reverse
}
