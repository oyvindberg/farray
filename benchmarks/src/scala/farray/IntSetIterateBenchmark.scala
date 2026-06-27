package farray

import org.openjdk.jmh.annotations.Benchmark

/** Iterate the whole set (sum the elements) — cache-sequential traversal. */
class IntSetIterateBenchmark extends IntSetInputs {
  @Benchmark def fset(): Long = { var s = 0L; fsetA.foreach(x => s += x); s }
  @Benchmark def scalaset(): Long = { var s = 0L; sSetA.foreach(x => s += x); s }
  @Benchmark def immbitset(): Long = { var s = 0L; immBitA.foreach(x => s += x); s }
  @Benchmark def jubitset(): Long = juBitA.stream().asLongStream().sum()
  @Benchmark def fastutil(): Long = { var s = 0L; val it = fuA.iterator(); while (it.hasNext) s += it.nextInt(); s }
  @Benchmark def hppc(): Long = { var s = 0L; val it = hppcA.iterator(); while (it.hasNext) s += it.next().value; s }
  @Benchmark def eclipsemut(): Long = ecMutA.sum()
  @Benchmark def roaring(): Long = { var s = 0L; val it = roarA.getIntIterator(); while (it.hasNext) s += it.next(); s }
}

/** FSet's headline: build a union as an O(1) LAZY node and answer `contains` over it without ever merging —
  * vs every competitor which must build the whole union first. The build-an-algebra-and-query pattern. */
class IntSetLazyAlgebraBenchmark extends IntSetInputs {
  @Benchmark def fset_lazy_union_contains(): Boolean = (fsetA ++ fsetB).contains(hit)
  @Benchmark def scalaset_eager_union_contains(): Boolean = (sSetA union sSetB).contains(hit)
  @Benchmark def immbitset_eager_union_contains(): Boolean = (immBitA | immBitB).contains(hit)
  @Benchmark def roaring_eager_union_contains(): Boolean = org.roaringbitmap.RoaringBitmap.or(roarA, roarB).contains(hit)
}
