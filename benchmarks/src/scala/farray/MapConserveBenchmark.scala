package farray

import org.openjdk.jmh.annotations.Benchmark

// mapConserve where f returns the SAME references (no change) must return the original with NO allocation.
class MapConserveStringBenchmark extends Inputs {
  @Benchmark def farray_noChange(): FArray[String] = farrayInput.mapConserve(s => s)
  @Benchmark def list_noChange(): List[String]     = listInput.mapConserve(s => s)
  @Benchmark def farray_change(): FArray[String] = farrayInput.mapConserve(s => if s.isEmpty then s else s + "x")
  @Benchmark def list_change(): List[String]     = listInput.mapConserve(s => if s.isEmpty then s else s + "x")
  // fs2.Chunk and zio.Chunk have no mapConserve
}

// On primitives mapConserve cannot conserve (a new Int is never `eq` the old box) — it materialises like map.
// (List.mapConserve is itself AnyRef-only, so there's no List[Int] comparison.) Conserve win is Ref-only.
class MapConserveIntBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] = farrayInput.mapConserve(x => x)
}
