package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}

/** Persistent single-element update: produce a NEW set with one more (incl) / one fewer (excl) element. FSet's
  * `+`/`-` are O(1) lazy nodes; scala immutable is CHAMP path-copy; immutable BitSet copies the word array;
  * mutable sets clone-then-mutate (the immutable-result cost). */
class IntSetInclBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = fsetA + miss
  @Benchmark def scalaset(): Any = sSetA + miss
  @Benchmark def immbitset(): Any = immBitA + miss
  @Benchmark def fastutil(): Any = { val c = new FuIntSet(fuA); c.add(miss); c }
  @Benchmark def eclipsemut(): Any = { val c = new EcIntSet(); c.addAll(ecMutA); c.add(miss); c }
}

class IntSetExclBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = fsetA - hit
  @Benchmark def scalaset(): Any = sSetA - hit
  @Benchmark def immbitset(): Any = immBitA - hit
  @Benchmark def fastutil(): Any = { val c = new FuIntSet(fuA); c.remove(hit); c }
  @Benchmark def eclipsemut(): Any = { val c = new EcIntSet(); c.addAll(ecMutA); c.remove(hit); c }
}
