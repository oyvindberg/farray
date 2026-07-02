package farray

import org.openjdk.jmh.annotations.Benchmark

/** Full-traversal ops for String: forall (all-nonEmpty → no short-circuit), exists (== miss → full scan), count (length>2), foreach (sum of lengths).
  */
class StrSetForallBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = fsetA.forall(s => s.nonEmpty)
  @Benchmark def scalaset(): Boolean = sSetA.forall(_.nonEmpty)
  @Benchmark def scalamut(): Boolean = smSetA.forall(_.nonEmpty)
  @Benchmark def juhashset(): Boolean = { val it = juA.iterator(); var ok = true; while (ok && it.hasNext) ok = it.next().nonEmpty; ok }
  @Benchmark def fastutil(): Boolean = { val it = fuA.iterator(); var ok = true; while (ok && it.hasNext) ok = it.next().nonEmpty; ok }
  @Benchmark def hppc(): Boolean = { val it = hppcA.iterator(); var ok = true; while (ok && it.hasNext) ok = it.next().value.nonEmpty; ok }
}

class StrSetExistsBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = fsetA.exists(s => s == miss)
  @Benchmark def scalaset(): Boolean = sSetA.exists(_ == miss)
  @Benchmark def scalamut(): Boolean = smSetA.exists(_ == miss)
  @Benchmark def juhashset(): Boolean = { val it = juA.iterator(); var f = false; while (!f && it.hasNext) f = it.next() == miss; f }
  @Benchmark def fastutil(): Boolean = { val it = fuA.iterator(); var f = false; while (!f && it.hasNext) f = it.next() == miss; f }
  @Benchmark def hppc(): Boolean = { val it = hppcA.iterator(); var f = false; while (!f && it.hasNext) f = it.next().value == miss; f }
}

class StrSetCountBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = fsetA.count(s => s.length > 2)
  @Benchmark def scalaset(): Int = sSetA.count(_.length > 2)
  @Benchmark def scalamut(): Int = smSetA.count(_.length > 2)
  @Benchmark def juhashset(): Int = { val it = juA.iterator(); var c = 0; while (it.hasNext) if (it.next().length > 2) c += 1; c }
  @Benchmark def fastutil(): Int = { val it = fuA.iterator(); var c = 0; while (it.hasNext) if (it.next().length > 2) c += 1; c }
  @Benchmark def hppc(): Int = { val it = hppcA.iterator(); var c = 0; while (it.hasNext) if (it.next().value.length > 2) c += 1; c }
}

class StrSetForeachBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = { var s = 0; fsetA.foreach(x => s += x.length); s }
  @Benchmark def scalaset(): Int = { var s = 0; sSetA.foreach(x => s += x.length); s }
  @Benchmark def scalamut(): Int = { var s = 0; smSetA.foreach(x => s += x.length); s }
  @Benchmark def juhashset(): Int = { var s = 0; val it = juA.iterator(); while (it.hasNext) s += it.next().length; s }
  @Benchmark def fastutil(): Int = { var s = 0; val it = fuA.iterator(); while (it.hasNext) s += it.next().length; s }
  @Benchmark def hppc(): Int = { var s = 0; val it = hppcA.iterator(); while (it.hasNext) s += it.next().value.length; s }
}
