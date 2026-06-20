package farray

import org.openjdk.jmh.annotations.Benchmark

// FOUR consecutive updates, then map. For FArray this stacks four UpdatedNodes (a depth-4 chain) that map
// reads through in a single pass — no copy is made for any of the updates. List/Vector/Array copy the whole
// sequence on EACH update (4 full copies) before the map. This is the stress case for lazy-node depth:
// each element read walks up to 4 nodes, so it shows whether avoiding 4 copies beats the chained dispatch.
class Updated4MapBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] =
    farrayInput.updated(0, -1).updated(size / 4, -2).updated(size / 2, -3).updated(size - 1, -4).map(_ + 1)
  @Benchmark def list(): List[Int] =
    listInput.updated(0, -1).updated(size / 4, -2).updated(size / 2, -3).updated(size - 1, -4).map(_ + 1)
  @Benchmark def vector(): Vector[Int] =
    vectorInput.updated(0, -1).updated(size / 4, -2).updated(size / 2, -3).updated(size - 1, -4).map(_ + 1)
  @Benchmark def array(): Array[Int] =
    arrayInput.updated(0, -1).updated(size / 4, -2).updated(size / 2, -3).updated(size - 1, -4).map(_ + 1)
}
