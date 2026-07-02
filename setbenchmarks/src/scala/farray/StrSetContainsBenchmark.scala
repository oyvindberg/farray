package farray

import org.openjdk.jmh.annotations.Benchmark

/** String `contains` — HIT (element ∈ set), across every Object-set competitor. */
class StrSetContainsHitBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = fsetA.contains(hit)
  @Benchmark def scalaset(): Boolean = sSetA.contains(hit)
  @Benchmark def scalamut(): Boolean = smSetA.contains(hit)
  @Benchmark def juhashset(): Boolean = juA.contains(hit)
  @Benchmark def jusetof(): Boolean = juOfA.contains(hit)
  @Benchmark def guava(): Boolean = guavaA.contains(hit)
  @Benchmark def eclipse(): Boolean = ecA.contains(hit)
  @Benchmark def fastutil(): Boolean = fuA.contains(hit)
  @Benchmark def hppc(): Boolean = hppcA.contains(hit)
}

/** String `contains` — MISS (element ∉ set). */
class StrSetContainsMissBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = fsetA.contains(miss)
  @Benchmark def scalaset(): Boolean = sSetA.contains(miss)
  @Benchmark def scalamut(): Boolean = smSetA.contains(miss)
  @Benchmark def juhashset(): Boolean = juA.contains(miss)
  @Benchmark def jusetof(): Boolean = juOfA.contains(miss)
  @Benchmark def guava(): Boolean = guavaA.contains(miss)
  @Benchmark def eclipse(): Boolean = ecA.contains(miss)
  @Benchmark def fastutil(): Boolean = fuA.contains(miss)
  @Benchmark def hppc(): Boolean = hppcA.contains(miss)
}
