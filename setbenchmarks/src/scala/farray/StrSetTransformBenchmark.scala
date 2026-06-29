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

class StrSetFilterBenchmark extends StrSetInputs {
  @Benchmark def fset(): Any = fsetA.filter(s => s.length > 2)
  @Benchmark def scalaset(): Any = sSetA.filter(_.length > 2)
  @Benchmark def scalamut(): Any = smSetA.filter(_.length > 2)
  @Benchmark def fastutil(): Any = { val c = new FuObjSet[String](); val it = fuA.iterator(); while (it.hasNext) { val s = it.next(); if (s.length > 2) c.add(s) }; c }
}
