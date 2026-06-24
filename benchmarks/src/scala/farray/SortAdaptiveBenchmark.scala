package farray

import org.openjdk.jmh.annotations.Benchmark

// The point of the natural (run-detecting) mergesort: already-sorted or reverse-sorted input is O(n), not
// O(n log n). Inputs are 0..N ascending. sortWith(_ < _) is one ascending run; sortWith(_ > _) is one
// strictly-descending run (reversed in place). Both should be near-free vs sorting scrambled data.
class IntSortAdaptiveBenchmark extends IntInputs {
  @Benchmark def farray_sorted(): FArray[Int] = farrayInput.sortWith((a, b) => a < b)
  @Benchmark def iarray_sorted(): IArray[Int] = iarrayInput.sortWith((a, b) => a < b)
  @Benchmark def list_sorted(): List[Int] = listInput.sortWith((a, b) => a < b)
  @Benchmark def vector_sorted(): Vector[Int] = vectorInput.sortWith((a, b) => a < b)
  @Benchmark def farray_reverse(): FArray[Int] = farrayInput.sortWith((a, b) => a > b)
  @Benchmark def iarray_reverse(): IArray[Int] = iarrayInput.sortWith((a, b) => a > b)
  @Benchmark def list_reverse(): List[Int] = listInput.sortWith((a, b) => a > b)
  @Benchmark def vector_reverse(): Vector[Int] = vectorInput.sortWith((a, b) => a > b)

  @Benchmark def ziochunk_sorted(): zio.Chunk[Int] = zioChunkInput.sortWith((a, b) => a < b)
  @Benchmark def ziochunk_reverse(): zio.Chunk[Int] = zioChunkInput.sortWith((a, b) => a > b)
  // fs2.Chunk has no sortWith/sortBy

  // sortBy on the unboxed int[] index path is adaptive too (keys already in order)
  @Benchmark def farray_sortBy(): FArray[Int] = farrayInput.sortBy(x => x)
  @Benchmark def iarray_sortBy(): IArray[Int] = iarrayInput.sortBy(x => x)
  @Benchmark def list_sortBy(): List[Int] = listInput.sortBy(x => x)
  @Benchmark def vector_sortBy(): Vector[Int] = vectorInput.sortBy(x => x)
  @Benchmark def ziochunk_sortBy(): zio.Chunk[Int] = zioChunkInput.sortBy(x => x)
}
