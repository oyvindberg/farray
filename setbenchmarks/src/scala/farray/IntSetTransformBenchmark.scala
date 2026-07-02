package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}

/** map (x+1 — a bijection, so no collapse) and filter (evens) — both produce a NEW set. */
class IntSetMapBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = fsetA.map(x => x + 1)
  @Benchmark def scalaset(): Any = sSetA.map(_ + 1)
  @Benchmark def immbitset(): Any = immBitA.map(_ + 1)
  @Benchmark def fastutil(): Any = { val c = new FuIntSet(); val it = fuA.iterator(); while (it.hasNext) c.add(it.nextInt() + 1); c }
}

/** flatMap (x -> {x, x+1} — dense 2x expansion with heavy overlap). fset/scalaset/immbitset pay a per-element inner-set allocation as the API demands; fastutil
  * has no flatMap, so its idiom is the raw double-add loop (the strongest possible baseline — no inner collections at all).
  */
class IntSetFlatMapBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = fsetA.flatMap(x => FSet(x, x + 1))
  @Benchmark def scalaset(): Any = sSetA.flatMap(x => Set(x, x + 1))
  @Benchmark def immbitset(): Any = immBitA.flatMap(x => Set(x, x + 1))
  @Benchmark def fastutil(): Any = {
    val c = new FuIntSet(); val it = fuA.iterator(); while (it.hasNext) { val v = it.nextInt(); c.add(v); c.add(v + 1) }; c
  }
}

class IntSetFilterBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = fsetA.filter(x => (x & 1) == 0)
  @Benchmark def scalaset(): Any = sSetA.filter(x => (x & 1) == 0)
  @Benchmark def immbitset(): Any = immBitA.filter(x => (x & 1) == 0)
  @Benchmark def fastutil(): Any = {
    val c = new FuIntSet(); val it = fuA.iterator(); while (it.hasNext) { val v = it.nextInt(); if ((v & 1) == 0) c.add(v) }; c
  }
}
