package farray

import org.openjdk.jmh.annotations.Benchmark

// drop(N/2) then map. FArray.drop is an O(1) SliceNode window; map reads base[offset..] straight into the
// result — no dropped-suffix copy is made. List walks N/2 nodes; Vector/Array allocate the tail slice first.
class DropMapBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] = farrayInput.drop(size / 2).map(_ + 1)
  @Benchmark def list(): List[Int]     = listInput.drop(size / 2).map(_ + 1)
  @Benchmark def vector(): Vector[Int] = vectorInput.drop(size / 2).map(_ + 1)
  @Benchmark def array(): Array[Int]   = arrayInput.drop(size / 2).map(_ + 1)
}
