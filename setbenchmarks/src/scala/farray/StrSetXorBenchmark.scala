package farray

import org.openjdk.jmh.annotations.Benchmark

/** Symmetric difference (xor) for String. Lazy `(a ^ b).contains(hit)` vs materialized `.materialize.size`. scala has no native set xor → (a∖b) ∪ (b∖a).
  */
class StrSetXorBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = (fsetA ^ fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = ((sSetA diff sSetB) union (sSetB diff sSetA)).contains(hit)
}

class StrSetMergeXorBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = (fsetA ^ fsetB).materialize.size
  @Benchmark def scalaset(): Int = ((sSetA diff sSetB) union (sSetB diff sSetA)).size
}
