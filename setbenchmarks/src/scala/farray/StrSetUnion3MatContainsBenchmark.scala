package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.{ImmutableSet, Sets}
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** The realistic build-once-query-many for STRINGS: materialize the 3-way union ONCE, then probe contains n = size times. This is the pattern where FSet's lazy
  * `Union3Contains` "win" is misleading — here the per-query cost is a materialized-set contains (the F14 inline-key path), so this directly measures whether
  * FSet keeps up with the mutable specialists once you actually merge and query a lot.
  */
class StrSetUnion3MatContainsBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = {
    val u = (fsetA ++ fsetB ++ fsetC).materialize
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def scalaset(): Int = {
    val u = sSetA union sSetB union sSetC
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def scalamut(): Int = {
    val u = smSetA union smSetB union smSetC
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def juhashset(): Int = {
    val u = new java.util.HashSet[String](juA); u.addAll(juB); u.addAll(juC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def guava(): Int = {
    val u = Sets.union(Sets.union(guavaA, guavaB), guavaC).immutableCopy()
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def eclipse(): Int = {
    val u = ecA.union(ecB).union(ecC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def fastutil(): Int = {
    val u = new FuObjSet[String](fuA); u.addAll(fuB); u.addAll(fuC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def hppc(): Int = {
    val u = new HppcObjSet[String](hppcA); u.addAll(hppcB); u.addAll(hppcC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
}
