package farray

import org.openjdk.jmh.annotations.Benchmark

// mapConserve where f returns the SAME references (no change) must return the original with NO allocation.
// The current impl maps into a fresh array then discards it — defeating the purpose. Also a partial-change case.
class MapConserveBenchmark extends Inputs {
  @Benchmark def farray_noChange(): FArray[String] = farrayInput.mapConserve(s => s)
  @Benchmark def list_noChange(): List[String]     = listInput.mapConserve(s => s)

  @Benchmark def farray_change(): FArray[String] = farrayInput.mapConserve(s => if s.isEmpty then s else s + "x")
  @Benchmark def list_change(): List[String]     = listInput.mapConserve(s => if s.isEmpty then s else s + "x")
}
