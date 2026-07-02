package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import com.carrotsearch.hppc.{IntHashSet => HppcIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}
import org.roaringbitmap.RoaringBitmap

/** Bulk set algebra, measured FAIRLY as "combine two sets then answer membership" — the real use case. FSet builds the combination as an O(1) lazy node and
  * `contains` DISTRIBUTES over it (no merge ever happens); every competitor must build the whole result set first, then probe. The mutable sets clone the LHS
  * before the destructive in-place op. (`hit` ∈ A∩B, so it's in the union/intersection and absent from the diff.)
  */
class IntSetUnionBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = (fsetA ++ fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = (sSetA union sSetB).contains(hit)
  @Benchmark def immbitset(): Boolean = (immBitA | immBitB).contains(hit)
  @Benchmark def jubitset(): Boolean = { val b = juBitA.clone().asInstanceOf[java.util.BitSet]; b.or(juBitB); b.get(hit) }
  @Benchmark def fastutil(): Boolean = { val c = new FuIntSet(fuA); c.addAll(fuB); c.contains(hit) }
  @Benchmark def hppc(): Boolean = { val c = new HppcIntSet(hppcA); c.addAll(hppcB); c.contains(hit) }
  @Benchmark def eclipsemut(): Boolean = { val c = new EcIntSet(); c.addAll(ecMutA); c.addAll(ecMutB); c.contains(hit) }
  @Benchmark def roaring(): Boolean = RoaringBitmap.or(roarA, roarB).contains(hit)
}

class IntSetIntersectBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = (fsetA & fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = (sSetA intersect sSetB).contains(hit)
  @Benchmark def immbitset(): Boolean = (immBitA & immBitB).contains(hit)
  @Benchmark def jubitset(): Boolean = { val b = juBitA.clone().asInstanceOf[java.util.BitSet]; b.and(juBitB); b.get(hit) }
  @Benchmark def fastutil(): Boolean = { val c = new FuIntSet(fuA); c.retainAll(fuB); c.contains(hit) }
  @Benchmark def hppc(): Boolean = { val c = new HppcIntSet(hppcA); c.retainAll(hppcB); c.contains(hit) }
  @Benchmark def eclipsemut(): Boolean = { val c = new EcIntSet(); c.addAll(ecMutA); c.retainAll(ecMutB); c.contains(hit) }
  @Benchmark def roaring(): Boolean = RoaringBitmap.and(roarA, roarB).contains(hit)
}

class IntSetDiffBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = (fsetA &~ fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = (sSetA diff sSetB).contains(hit)
  @Benchmark def immbitset(): Boolean = (immBitA &~ immBitB).contains(hit)
  @Benchmark def jubitset(): Boolean = { val b = juBitA.clone().asInstanceOf[java.util.BitSet]; b.andNot(juBitB); b.get(hit) }
  @Benchmark def fastutil(): Boolean = { val c = new FuIntSet(fuA); c.removeAll(fuB); c.contains(hit) }
  @Benchmark def hppc(): Boolean = { val c = new HppcIntSet(hppcA); c.removeAll(hppcB: com.carrotsearch.hppc.IntContainer); c.contains(hit) }
  @Benchmark def eclipsemut(): Boolean = { val c = new EcIntSet(); c.addAll(ecMutA); c.removeAll(ecMutB); c.contains(hit) }
  @Benchmark def roaring(): Boolean = RoaringBitmap.andNot(roarA, roarB).contains(hit)
}
