package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.{ImmutableSet, Sets}
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** Bulk String-set algebra, measured FAIRLY as "combine then answer membership". FSet builds the combination
  * as an O(1) lazy node and `contains` distributes over it — no merge; every competitor must build the whole
  * result first, then probe. (`hit` ∈ A∩B.) */
class StrSetUnionBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = (fsetA ++ fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = (sSetA union sSetB).contains(hit)
  @Benchmark def juhashset(): Boolean = { val c = new java.util.HashSet[String](juA); c.addAll(juB); c.contains(hit) }
  @Benchmark def guava(): Boolean = Sets.union(guavaA, guavaB).immutableCopy().contains(hit)
  @Benchmark def eclipse(): Boolean = ecA.union(ecB).contains(hit)
  @Benchmark def fastutil(): Boolean = { val c = new FuObjSet[String](fuA); c.addAll(fuB); c.contains(hit) }
  @Benchmark def hppc(): Boolean = { val c = new HppcObjSet[String](hppcA); c.addAll(hppcB); c.contains(hit) }
}

class StrSetIntersectBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = (fsetA & fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = (sSetA intersect sSetB).contains(hit)
  @Benchmark def juhashset(): Boolean = { val c = new java.util.HashSet[String](juA); c.retainAll(juB); c.contains(hit) }
  @Benchmark def guava(): Boolean = Sets.intersection(guavaA, guavaB).immutableCopy().contains(hit)
  @Benchmark def eclipse(): Boolean = ecA.intersect(ecB).contains(hit)
  @Benchmark def fastutil(): Boolean = { val c = new FuObjSet[String](fuA); c.retainAll(fuB); c.contains(hit) }
  @Benchmark def hppc(): Boolean = { val c = new HppcObjSet[String](hppcA); c.retainAll(hppcB); c.contains(hit) }
}

class StrSetDiffBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = (fsetA &~ fsetB).contains(hit)
  @Benchmark def scalaset(): Boolean = (sSetA diff sSetB).contains(hit)
  @Benchmark def juhashset(): Boolean = { val c = new java.util.HashSet[String](juA); c.removeAll(juB); c.contains(hit) }
  @Benchmark def guava(): Boolean = Sets.difference(guavaA, guavaB).immutableCopy().contains(hit)
  @Benchmark def eclipse(): Boolean = ecA.difference(ecB).contains(hit)
  @Benchmark def fastutil(): Boolean = { val c = new FuObjSet[String](fuA); c.removeAll(fuB); c.contains(hit) }
  @Benchmark def hppc(): Boolean = { val c = new HppcObjSet[String](hppcA); c.removeAll(hppcB.asInstanceOf[com.carrotsearch.hppc.ObjectContainer[String]]); c.contains(hit) }
}
