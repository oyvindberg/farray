package farray

import org.openjdk.jmh.annotations.Benchmark

/** Symmetric difference (xor). Lazy: `(a ^ b).contains(hit)` — FSet distributes; competitors must build the
  * symmetric diff. Materialized: `(a ^ b).materialize.size`. scala has no native set xor → (a∖b) ∪ (b∖a). */
class IntSetXorBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = (fsetA ^ fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = ((sSetA diff sSetB) union (sSetB diff sSetA)).contains(hit)
  @Benchmark def immbitset(): Boolean = (immBitA ^ immBitB).contains(hit)
}

class IntSetMergeXorBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = (fsetA ^ fsetB).materialize.size
  @Benchmark def scalaset(): Int = ((sSetA diff sSetB) union (sSetB diff sSetA)).size
  @Benchmark def immbitset(): Int = (immBitA ^ immBitB).size
  @Benchmark def roaring(): Int = org.roaringbitmap.RoaringBitmap.xor(roarA, roarB).getCardinality
}
