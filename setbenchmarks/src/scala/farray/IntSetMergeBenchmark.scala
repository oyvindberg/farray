package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import com.carrotsearch.hppc.{IntHashSet => HppcIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}
import org.roaringbitmap.RoaringBitmap

/** FAIR materialized merge: actually fold the union/intersect/diff into a usable set and read its size — the opposite of IntSet{Union,..}Benchmark, which
  * measures the LAZY moat `(a&b).contains`. Every impl produces a fresh result set and forces it via `.size`. FSet's `(a++b).materialize` does the unboxed
  * sort-merge; `.size` forces it (and on a no-op-materialize kind would force the fallback walk). This is the benchmark that scores the Ref merge core + the
  * wrap-to-hash promotion. Mutable competitors clone the LHS (immutable-result cost).
  */
class IntSetMergeUnionBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = (fsetA ++ fsetB).materialize.size
  @Benchmark def scalaset(): Int = (sSetA union sSetB).size
  @Benchmark def immbitset(): Int = (immBitA | immBitB).size
  @Benchmark def fastutil(): Int = { val c = new FuIntSet(fuA); c.addAll(fuB); c.size }
  @Benchmark def hppc(): Int = { val c = new HppcIntSet(hppcA); c.addAll(hppcB); c.size }
  @Benchmark def eclipsemut(): Int = { val c = new EcIntSet(); c.addAll(ecMutA); c.addAll(ecMutB); c.size }
  @Benchmark def roaring(): Int = RoaringBitmap.or(roarA, roarB).getCardinality
}

class IntSetMergeIntersectBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = (fsetA & fsetB).materialize.size
  @Benchmark def scalaset(): Int = (sSetA intersect sSetB).size
  @Benchmark def immbitset(): Int = (immBitA & immBitB).size
  @Benchmark def fastutil(): Int = { val c = new FuIntSet(fuA); c.retainAll(fuB); c.size }
  @Benchmark def hppc(): Int = { val c = new HppcIntSet(hppcA); c.retainAll(hppcB); c.size }
  @Benchmark def eclipsemut(): Int = { val c = new EcIntSet(); c.addAll(ecMutA); c.retainAll(ecMutB); c.size }
  @Benchmark def roaring(): Int = RoaringBitmap.and(roarA, roarB).getCardinality
}

class IntSetMergeDiffBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = (fsetA &~ fsetB).materialize.size
  @Benchmark def scalaset(): Int = (sSetA diff sSetB).size
  @Benchmark def immbitset(): Int = (immBitA &~ immBitB).size
  @Benchmark def fastutil(): Int = { val c = new FuIntSet(fuA); c.removeAll(fuB); c.size }
  @Benchmark def hppc(): Int = { val c = new HppcIntSet(hppcA); c.removeAll(hppcB: com.carrotsearch.hppc.IntContainer); c.size }
  @Benchmark def eclipsemut(): Int = { val c = new EcIntSet(); c.addAll(ecMutA); c.removeAll(ecMutB); c.size }
  @Benchmark def roaring(): Int = RoaringBitmap.andNot(roarA, roarB).getCardinality
}
