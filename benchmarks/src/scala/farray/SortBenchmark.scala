package farray

import org.openjdk.jmh.annotations.Benchmark

// All sort ops (sortWith / sortBy / sorted), both Int and String, vs IArray / Array / List / Vector.
class SortIntBenchmark extends IntInputs {
  private inline def scramble(x: Int): Int = (x * 1103515245 + 12345) & 0x7fffffff
  @Benchmark def farray_sortWith(): FArray[Int] = farrayInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def iarray_sortWith(): IArray[Int] = iarrayInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def array_sortWith(): Array[Int]   = arrayInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def list_sortWith(): List[Int]     = listInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def vector_sortWith(): Vector[Int] = vectorInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def farray_sortBy(): FArray[Int] = farrayInput.sortBy(scramble)
  @Benchmark def iarray_sortBy(): IArray[Int] = iarrayInput.sortBy(scramble)
  @Benchmark def array_sortBy(): Array[Int]   = arrayInput.sortBy(scramble)
  @Benchmark def list_sortBy(): List[Int]     = listInput.sortBy(scramble)
  @Benchmark def vector_sortBy(): Vector[Int] = vectorInput.sortBy(scramble)
  @Benchmark def farray_sorted(): FArray[Int] = farrayInput.sorted
  @Benchmark def iarray_sorted(): IArray[Int] = iarrayInput.sorted
  @Benchmark def array_sorted(): Array[Int]   = arrayInput.sorted
  @Benchmark def list_sorted(): List[Int]     = listInput.sorted
  @Benchmark def vector_sorted(): Vector[Int] = vectorInput.sorted
}

class SortStringBenchmark extends Inputs {
  @Benchmark def farray_sortWith(): FArray[String] = farrayInput.sortWith((a, b) => a < b)
  @Benchmark def iarray_sortWith(): IArray[String] = iarrayInput.sortWith((a, b) => a < b)
  @Benchmark def list_sortWith(): List[String]     = listInput.sortWith((a, b) => a < b)
  @Benchmark def vector_sortWith(): Vector[String] = vectorInput.sortWith((a, b) => a < b)
  @Benchmark def farray_sortBy(): FArray[String] = farrayInput.sortBy(_.reverse)
  @Benchmark def iarray_sortBy(): IArray[String] = iarrayInput.sortBy(_.reverse)
  @Benchmark def list_sortBy(): List[String]     = listInput.sortBy(_.reverse)
  @Benchmark def vector_sortBy(): Vector[String] = vectorInput.sortBy(_.reverse)
  @Benchmark def farray_sorted(): FArray[String] = farrayInput.sorted
  @Benchmark def iarray_sorted(): IArray[String] = iarrayInput.sorted
  @Benchmark def list_sorted(): List[String]     = listInput.sorted
  @Benchmark def vector_sorted(): Vector[String] = vectorInput.sorted
}
