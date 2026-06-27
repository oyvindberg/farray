package farray

import org.openjdk.jmh.annotations.Benchmark

/** Int `contains` — the hot read path — hit (∈ set) and miss (∉ set), across every competitor. */
class IntSetContainsBenchmark extends IntSetInputs {
  @Benchmark def fset_hit(): Boolean = fsetA.contains(hit)
  @Benchmark def fset_miss(): Boolean = fsetA.contains(miss)
  @Benchmark def scalaset_hit(): Boolean = sSetA.contains(hit)
  @Benchmark def scalaset_miss(): Boolean = sSetA.contains(miss)
  @Benchmark def immbitset_hit(): Boolean = immBitA.contains(hit)
  @Benchmark def immbitset_miss(): Boolean = immBitA.contains(miss)
  @Benchmark def jubitset_hit(): Boolean = juBitA.get(hit)
  @Benchmark def jubitset_miss(): Boolean = juBitA.get(miss)
  @Benchmark def fastutil_hit(): Boolean = fuA.contains(hit)
  @Benchmark def fastutil_miss(): Boolean = fuA.contains(miss)
  @Benchmark def hppc_hit(): Boolean = hppcA.contains(hit)
  @Benchmark def hppc_miss(): Boolean = hppcA.contains(miss)
  @Benchmark def eclipsemut_hit(): Boolean = ecMutA.contains(hit)
  @Benchmark def eclipsemut_miss(): Boolean = ecMutA.contains(miss)
  @Benchmark def eclipseimm_hit(): Boolean = ecImmA.contains(hit)
  @Benchmark def eclipseimm_miss(): Boolean = ecImmA.contains(miss)
  @Benchmark def roaring_hit(): Boolean = roarA.contains(hit)
  @Benchmark def roaring_miss(): Boolean = roarA.contains(miss)
}
