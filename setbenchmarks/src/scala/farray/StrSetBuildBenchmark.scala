package farray

import org.openjdk.jmh.annotations.Benchmark
import scala.collection.mutable.{HashSet => SMHashSet}
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.ImmutableSet
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** Build a String set from a String[] — the build-once cost. */
class StrSetBuildBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = FSet.fromArray(arrA)
  @Benchmark def scalaset(): Any = arrA.toSet
  @Benchmark def scalamut(): Any = SMHashSet(arrA*)
  @Benchmark def juhashset(): Any = { val s = new java.util.HashSet[String](); var i = 0; while (i < arrA.length) { s.add(arrA(i)); i += 1 }; s }
  @Benchmark def jusetof(): Any = java.util.Set.of(arrA*)
  @Benchmark def guava(): Any = ImmutableSet.copyOf(arrA)
  @Benchmark def eclipse(): Any = UnifiedSet.newSetWith(arrA*)
  @Benchmark def fastutil(): Any = new FuObjSet[String](arrA)
  @Benchmark def hppc(): Any = HppcObjSet.from(arrA*)
}

/** Iterate the String set (sum of element lengths) — full traversal. */
class StrSetIterateBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = { var s = 0; fsetA.foreach(x => s += x.length); s }
  @Benchmark def scalaset(): Int = { var s = 0; sSetA.foreach(x => s += x.length); s }
  @Benchmark def juhashset(): Int = { var s = 0; val it = juA.iterator(); while (it.hasNext) s += it.next().length; s }
  @Benchmark def guava(): Int = { var s = 0; val it = guavaA.iterator(); while (it.hasNext) s += it.next().length; s }
  @Benchmark def eclipse(): Int = { var s = 0; val it = ecA.iterator(); while (it.hasNext) s += it.next().length; s }
  @Benchmark def fastutil(): Int = { var s = 0; val it = fuA.iterator(); while (it.hasNext) s += it.next().length; s }
  @Benchmark def hppc(): Int = { var s = 0; val it = hppcA.iterator(); while (it.hasNext) s += it.next().value.length; s }
}
