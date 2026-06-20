package farray

import org.openjdk.jmh.annotations.Benchmark

// Sort on FArray's own primitive array (materialize + stable mergesort), no Vector of boxed indices.
// sortWith uses an inline comparator (fully unboxed for Int). Scrambled keys so the input is a real sort.
class SortIntBenchmark extends IntInputs {
  private inline def scramble(x: Int): Int = (x * 1103515245 + 12345) & 0x7fffffff
  @Benchmark def farray_sortBy(): FArray[Int] = farrayInput.sortBy(scramble)
  @Benchmark def list_sortBy(): List[Int]     = listInput.sortBy(scramble)
  @Benchmark def vector_sortBy(): Vector[Int] = vectorInput.sortBy(scramble)
  @Benchmark def array_sortBy(): Array[Int]   = arrayInput.sortBy(scramble)
  @Benchmark def farray_sortWith(): FArray[Int] = farrayInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def list_sortWith(): List[Int]     = listInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def vector_sortWith(): Vector[Int] = vectorInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def array_sortWith(): Array[Int]   = arrayInput.sortWith((a, b) => scramble(a) < scramble(b))
}

class SortStringBenchmark extends Inputs {
  @Benchmark def farray_sortBy(): FArray[String] = farrayInput.sortBy(_.reverse)
  @Benchmark def list_sortBy(): List[String]     = listInput.sortBy(_.reverse)
  @Benchmark def vector_sortBy(): Vector[String] = vectorInput.sortBy(_.reverse)
  @Benchmark def iarray_sortBy(): IArray[String] = iarrayInput.sortBy(_.reverse)
  @Benchmark def farray_sortWith(): FArray[String] = farrayInput.sortWith((a, b) => a < b)
  @Benchmark def list_sortWith(): List[String]     = listInput.sortWith((a, b) => a < b)
  @Benchmark def vector_sortWith(): Vector[String] = vectorInput.sortWith((a, b) => a < b)
  @Benchmark def iarray_sortWith(): IArray[String] = iarrayInput.sortWith((a, b) => a < b)
}
