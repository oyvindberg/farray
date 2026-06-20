package farray

import org.openjdk.jmh.annotations.Benchmark

// updated (one element) then map. FArray.updated is an O(1) UpdatedNode; map reads the base with one index
// overridden straight into the result — no copy of the whole sequence is made for the update. List/Vector/
// Array copy the entire sequence to apply the single change, then map it.
class UpdatedMapBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] = farrayInput.updated(size / 2, -1).map(_ + 1)
  @Benchmark def list(): List[Int]     = listInput.updated(size / 2, -1).map(_ + 1)
  @Benchmark def vector(): Vector[Int] = vectorInput.updated(size / 2, -1).map(_ + 1)
  @Benchmark def array(): Array[Int]   = arrayInput.updated(size / 2, -1).map(_ + 1)
}
