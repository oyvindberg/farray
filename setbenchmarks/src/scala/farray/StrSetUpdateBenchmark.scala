package farray

import org.openjdk.jmh.annotations.Benchmark
import scala.collection.mutable.{HashSet => SMHashSet}
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.google.common.collect.ImmutableSet

/** Persistent single-element update for String: a NEW set with one more (incl) / one fewer (excl) element.
  * FSet's `+`/`-` are O(1) lazy; scala immutable is CHAMP; mutable/Guava clone-or-rebuild (immutable-result cost). */
class StrSetInclBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = fsetA + miss
  @Benchmark def scalaset(): Any = sSetA + miss
  @Benchmark def scalamut(): Any = { val c = smSetA.clone(); c += miss; c }
  @Benchmark def juhashset(): Any = { val c = new java.util.HashSet[String](juA); c.add(miss); c }
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](fuA); c.add(miss); c }
  @Benchmark def guava(): Any = ImmutableSet.builder[String]().addAll(guavaA).add(miss).build()
}

class StrSetExclBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = fsetA - hit
  @Benchmark def scalaset(): Any = sSetA - hit
  @Benchmark def scalamut(): Any = { val c = smSetA.clone(); c -= hit; c }
  @Benchmark def juhashset(): Any = { val c = new java.util.HashSet[String](juA); c.remove(hit); c }
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](fuA); c.remove(hit); c }
}
