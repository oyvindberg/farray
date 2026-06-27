package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import com.carrotsearch.hppc.{IntHashSet => HppcIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}
import org.roaringbitmap.RoaringBitmap

/** Union THREE sets, then probe `contains` n = size times (once per input element). The amortized test: FSet
  * builds the 3-way union as O(1) lazy nodes and each `contains` distributes over the depth-2 tree (never
  * materializes); every competitor builds the whole 3-way union once, then answers each `contains` in O(1).
  * As the query count grows the union-build cost amortizes — this is where FSet's per-query tree-walk is tested. */
class IntSetUnion3ContainsBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = {
    val u = fsetA ++ fsetB ++ fsetC
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def scalaset(): Int = {
    val u = sSetA union sSetB union sSetC
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def immbitset(): Int = {
    val u = immBitA | immBitB | immBitC
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def jubitset(): Int = {
    val u = juBitA.clone().asInstanceOf[java.util.BitSet]; u.or(juBitB); u.or(juBitC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.get(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def fastutil(): Int = {
    val u = new FuIntSet(fuA); u.addAll(fuB); u.addAll(fuC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def hppc(): Int = {
    val u = new HppcIntSet(hppcA); u.addAll(hppcB); u.addAll(hppcC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def eclipsemut(): Int = {
    val u = new EcIntSet(); u.addAll(ecMutA); u.addAll(ecMutB); u.addAll(ecMutC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def roaring(): Int = {
    val u = RoaringBitmap.or(roarA, roarB); u.or(roarC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
}
