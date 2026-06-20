package farray

import org.openjdk.jmh.annotations.Benchmark

// Sort an Int sequence. FArray sorts its OWN primitive int[] (materialize once + a stable int[] index
// mergesort) — never a Vector of boxed indices. sortWith takes an inline comparator (fully unboxed);
// sorted/sortBy still box per ord comparison, but the storage and indices stay primitive.
// sortBy uses a scrambling key so the input (already ascending) is a real O(n log n) sort.
class SortBenchmark extends IntInputs {
  private inline def scramble(x: Int): Int = (x * 1103515245 + 12345) & 0x7fffffff

  @Benchmark def farray_sortBy(): FArray[Int] = farrayInput.sortBy(scramble)
  @Benchmark def list_sortBy(): List[Int]     = listInput.sortBy(scramble)
  @Benchmark def vector_sortBy(): Vector[Int] = vectorInput.sortBy(scramble)
  @Benchmark def array_sortBy(): Array[Int]   = arrayInput.sortBy(scramble)

  // scramble in the comparator so the (ascending) input becomes random order — not a TimSort best case
  @Benchmark def farray_sortWith(): FArray[Int] = farrayInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def list_sortWith(): List[Int]     = listInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def vector_sortWith(): Vector[Int] = vectorInput.sortWith((a, b) => scramble(a) < scramble(b))
  @Benchmark def array_sortWith(): Array[Int]   = arrayInput.sortWith((a, b) => scramble(a) < scramble(b))
}
