package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import com.carrotsearch.hppc.{IntHashSet => HppcIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}
import org.roaringbitmap.RoaringBitmap

/** Bulk set algebra on two overlapping Int sets — union/intersect/diff. The mutable specialized sets do
  * DESTRUCTIVE in-place ops, so each clones the LHS first (the realistic immutable-result cost); the immutable
  * sets (FSet/scala/BitSet/Roaring) produce a fresh result directly. FSet materializes the lazy node (the merge
  * is the real work; the O(1) lazy construct is measured separately in IntSetLazyAlgebraBenchmark). */
class IntSetUnionBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = (fsetA ++ fsetB).materialize
  @Benchmark def scalaset(): Any = sSetA union sSetB
  @Benchmark def immbitset(): Any = immBitA | immBitB
  @Benchmark def jubitset(): Any = { val b = juBitA.clone().asInstanceOf[java.util.BitSet]; b.or(juBitB); b }
  @Benchmark def fastutil(): Any = { val c = new FuIntSet(fuA); c.addAll(fuB); c }
  @Benchmark def hppc(): Any = { val c = new HppcIntSet(hppcA); c.addAll(hppcB); c }
  @Benchmark def eclipsemut(): Any = { val c = new EcIntSet(); c.addAll(ecMutA); c.addAll(ecMutB); c }
  @Benchmark def roaring(): Any = RoaringBitmap.or(roarA, roarB)
}

class IntSetIntersectBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = (fsetA & fsetB).materialize
  @Benchmark def scalaset(): Any = sSetA intersect sSetB
  @Benchmark def immbitset(): Any = immBitA & immBitB
  @Benchmark def jubitset(): Any = { val b = juBitA.clone().asInstanceOf[java.util.BitSet]; b.and(juBitB); b }
  @Benchmark def fastutil(): Any = { val c = new FuIntSet(fuA); c.retainAll(fuB); c }
  @Benchmark def hppc(): Any = { val c = new HppcIntSet(hppcA); c.retainAll(hppcB); c }
  @Benchmark def eclipsemut(): Any = { val c = new EcIntSet(); c.addAll(ecMutA); c.retainAll(ecMutB); c }
  @Benchmark def roaring(): Any = RoaringBitmap.and(roarA, roarB)
}

class IntSetDiffBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = (fsetA &~ fsetB).materialize
  @Benchmark def scalaset(): Any = sSetA diff sSetB
  @Benchmark def immbitset(): Any = immBitA &~ immBitB
  @Benchmark def jubitset(): Any = { val b = juBitA.clone().asInstanceOf[java.util.BitSet]; b.andNot(juBitB); b }
  @Benchmark def fastutil(): Any = { val c = new FuIntSet(fuA); c.removeAll(fuB); c }
  @Benchmark def hppc(): Any = { val c = new HppcIntSet(hppcA); c.removeAll(hppcB: com.carrotsearch.hppc.IntContainer); c }
  @Benchmark def eclipsemut(): Any = { val c = new EcIntSet(); c.addAll(ecMutA); c.removeAll(ecMutB); c }
  @Benchmark def roaring(): Any = RoaringBitmap.andNot(roarA, roarB)
}
