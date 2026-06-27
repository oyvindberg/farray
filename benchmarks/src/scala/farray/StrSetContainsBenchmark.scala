package farray

import org.openjdk.jmh.annotations.Benchmark

/** String `contains` — hit (∈ set) and miss (∉ set), across every Object-set competitor. */
class StrSetContainsBenchmark extends StrSetInputs {
  @Benchmark def fset_hit(): Boolean = fsetA.contains(hit)
  @Benchmark def fset_miss(): Boolean = fsetA.contains(miss)
  @Benchmark def scalaset_hit(): Boolean = sSetA.contains(hit)
  @Benchmark def scalaset_miss(): Boolean = sSetA.contains(miss)
  @Benchmark def scalamut_hit(): Boolean = smSetA.contains(hit)
  @Benchmark def scalamut_miss(): Boolean = smSetA.contains(miss)
  @Benchmark def juhashset_hit(): Boolean = juA.contains(hit)
  @Benchmark def juhashset_miss(): Boolean = juA.contains(miss)
  @Benchmark def jusetof_hit(): Boolean = juOfA.contains(hit)
  @Benchmark def jusetof_miss(): Boolean = juOfA.contains(miss)
  @Benchmark def guava_hit(): Boolean = guavaA.contains(hit)
  @Benchmark def guava_miss(): Boolean = guavaA.contains(miss)
  @Benchmark def eclipse_hit(): Boolean = ecA.contains(hit)
  @Benchmark def eclipse_miss(): Boolean = ecA.contains(miss)
  @Benchmark def fastutil_hit(): Boolean = fuA.contains(hit)
  @Benchmark def fastutil_miss(): Boolean = fuA.contains(miss)
  @Benchmark def hppc_hit(): Boolean = hppcA.contains(hit)
  @Benchmark def hppc_miss(): Boolean = hppcA.contains(miss)
}
