package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.{ImmutableSet, Sets}
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** Bulk String-set algebra. Mutable sets clone the LHS (the realistic immutable-result cost); immutable peers
  * (FSet/scala/Guava/Eclipse) produce a fresh result. Guava ∪∩∖ are lazy SetViews → `.immutableCopy()`. */
class StrSetUnionBenchmark extends StrSetInputs {
  // Ref has no unboxed merge yet, so `.materialize` is a no-op (the union stays a lazy node). Force the real
  // dedup work via `.size` (the boxed collectElems path) so this is a FAIR "produce a usable union" measurement.
  @Benchmark def fset(): Any = (fsetA ++ fsetB).materialize.size
  @Benchmark def scalaset(): Any = sSetA union sSetB
  @Benchmark def juhashset(): Any = { val c = new java.util.HashSet[String](juA); c.addAll(juB); c }
  @Benchmark def guava(): Any = Sets.union(guavaA, guavaB).immutableCopy()
  @Benchmark def eclipse(): Any = ecA.union(ecB)
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](fuA); c.addAll(fuB); c }
  @Benchmark def hppc(): Any = { val c = new HppcObjSet[String](hppcA); c.addAll(hppcB); c }
}

class StrSetIntersectBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = (fsetA & fsetB).materialize.size
  @Benchmark def scalaset(): Any = sSetA intersect sSetB
  @Benchmark def juhashset(): Any = { val c = new java.util.HashSet[String](juA); c.retainAll(juB); c }
  @Benchmark def guava(): Any = Sets.intersection(guavaA, guavaB).immutableCopy()
  @Benchmark def eclipse(): Any = ecA.intersect(ecB)
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](fuA); c.retainAll(fuB); c }
  @Benchmark def hppc(): Any = { val c = new HppcObjSet[String](hppcA); c.retainAll(hppcB); c }
}

class StrSetDiffBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = (fsetA &~ fsetB).materialize.size
  @Benchmark def scalaset(): Any = sSetA diff sSetB
  @Benchmark def juhashset(): Any = { val c = new java.util.HashSet[String](juA); c.removeAll(juB); c }
  @Benchmark def guava(): Any = Sets.difference(guavaA, guavaB).immutableCopy()
  @Benchmark def eclipse(): Any = ecA.difference(ecB)
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](fuA); c.removeAll(fuB); c }
  @Benchmark def hppc(): Any = { val c = new HppcObjSet[String](hppcA); c.removeAll(hppcB.asInstanceOf[com.carrotsearch.hppc.ObjectContainer[String]]); c }
}
