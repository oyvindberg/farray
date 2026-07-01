package farray

import org.openjdk.jmh.annotations.Benchmark

/** Int `contains` — HIT (element ∈ set), the hot read path, across every competitor. (Method name = impl name so the W/T/L report groups them; miss is a
  * sibling class.)
  */
class IntSetContainsHitBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = fsetA.contains(hit)
  @Benchmark def scalaset(): Boolean = sSetA.contains(hit)
  @Benchmark def immbitset(): Boolean = immBitA.contains(hit)
  @Benchmark def jubitset(): Boolean = juBitA.get(hit)
  @Benchmark def fastutil(): Boolean = fuA.contains(hit)
  @Benchmark def hppc(): Boolean = hppcA.contains(hit)
  @Benchmark def eclipsemut(): Boolean = ecMutA.contains(hit)
  @Benchmark def eclipseimm(): Boolean = ecImmA.contains(hit)
  @Benchmark def roaring(): Boolean = roarA.contains(hit)
}

/** Int `contains` — MISS (element ∉ set). */
class IntSetContainsMissBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = fsetA.contains(miss)
  @Benchmark def scalaset(): Boolean = sSetA.contains(miss)
  @Benchmark def immbitset(): Boolean = immBitA.contains(miss)
  @Benchmark def jubitset(): Boolean = juBitA.get(miss)
  @Benchmark def fastutil(): Boolean = fuA.contains(miss)
  @Benchmark def hppc(): Boolean = hppcA.contains(miss)
  @Benchmark def eclipsemut(): Boolean = ecMutA.contains(miss)
  @Benchmark def eclipseimm(): Boolean = ecImmA.contains(miss)
  @Benchmark def roaring(): Boolean = roarA.contains(miss)
}
