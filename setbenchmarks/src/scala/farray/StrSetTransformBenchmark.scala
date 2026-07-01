package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}

/** map (append "x" — injective, no collapse) and filter (length>2) for String — both produce a NEW set. */
class StrSetMapBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = fsetA.map(s => s + "x")
  @Benchmark def scalaset(): Any = sSetA.map(_ + "x")
  @Benchmark def scalamut(): Any = smSetA.map(_ + "x")
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](); val it = fuA.iterator(); while (it.hasNext) c.add(it.next() + "x"); c }
}

/** flatMap (s -> {s, s+"x"} — injective 2x expansion). fastutil has no flatMap; its idiom is the raw double-add loop (no inner collections — the strongest
  * baseline).
  */
class StrSetFlatMapBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = fsetA.flatMap(s => FSet(s, s + "x"))
  @Benchmark def scalaset(): Any = sSetA.flatMap(s => Set(s, s + "x"))
  @Benchmark def scalamut(): Any = smSetA.flatMap(s => Set(s, s + "x"))
  @Benchmark def fastutil(): Any = {
    val c = new FuObjSet[String](); val it = fuA.iterator(); while (it.hasNext) { val s = it.next(); c.add(s); c.add(s + "x") }; c
  }
}

class StrSetFilterBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = fsetA.filter(s => s.length > 2)
  @Benchmark def scalaset(): Any = sSetA.filter(_.length > 2)
  @Benchmark def scalamut(): Any = smSetA.filter(_.length > 2)
  @Benchmark def fastutil(): Any = {
    val c = new FuObjSet[String](); val it = fuA.iterator(); while (it.hasNext) { val s = it.next(); if (s.length > 2) c.add(s) }; c
  }
}
