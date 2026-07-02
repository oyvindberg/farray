package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.{ImmutableSet, Sets}
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** Union THREE String sets, then probe `contains` n = size times. The amortized test (see Int variant). */
class StrSetUnion3ContainsBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = {
    val u = fsetA ++ fsetB ++ fsetC
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
