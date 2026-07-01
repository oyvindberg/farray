package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.{ImmutableSet, Sets}
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** FAIR materialized merge for String (the Ref merge core's scorecard). Every impl folds the result and reads its size; FSet's `(a++b).materialize.size` forces
  * the real merge (today a boxed collectElems walk — the measured loss; after the Ref merge core, a cached-hash sort-merge). See the Int variant.
  */
class StrSetMergeUnionBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = (fsetA ++ fsetB).materialize.size
  @Benchmark def scalaset(): Int = (sSetA union sSetB).size
  @Benchmark def juhashset(): Int = { val c = new java.util.HashSet[String](juA); c.addAll(juB); c.size }
  @Benchmark def guava(): Int = Sets.union(guavaA, guavaB).immutableCopy().size
  @Benchmark def eclipse(): Int = ecA.union(ecB).size
  @Benchmark def fastutil(): Int = { val c = new FuObjSet[String](fuA); c.addAll(fuB); c.size }
  @Benchmark def hppc(): Int = { val c = new HppcObjSet[String](hppcA); c.addAll(hppcB); c.size }
}

class StrSetMergeIntersectBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = (fsetA & fsetB).materialize.size
  @Benchmark def scalaset(): Int = (sSetA intersect sSetB).size
  @Benchmark def juhashset(): Int = { val c = new java.util.HashSet[String](juA); c.retainAll(juB); c.size }
  @Benchmark def guava(): Int = Sets.intersection(guavaA, guavaB).immutableCopy().size
  @Benchmark def eclipse(): Int = ecA.intersect(ecB).size
  @Benchmark def fastutil(): Int = { val c = new FuObjSet[String](fuA); c.retainAll(fuB); c.size }
  @Benchmark def hppc(): Int = { val c = new HppcObjSet[String](hppcA); c.retainAll(hppcB); c.size }
}

class StrSetMergeDiffBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = (fsetA &~ fsetB).materialize.size
  @Benchmark def scalaset(): Int = (sSetA diff sSetB).size
  @Benchmark def juhashset(): Int = { val c = new java.util.HashSet[String](juA); c.removeAll(juB); c.size }
  @Benchmark def guava(): Int = Sets.difference(guavaA, guavaB).immutableCopy().size
  @Benchmark def eclipse(): Int = ecA.difference(ecB).size
  @Benchmark def fastutil(): Int = { val c = new FuObjSet[String](fuA); c.removeAll(fuB); c.size }
  @Benchmark def hppc(): Int = { val c = new HppcObjSet[String](hppcA); c.removeAll(hppcB.asInstanceOf[com.carrotsearch.hppc.ObjectContainer[String]]); c.size }
}
