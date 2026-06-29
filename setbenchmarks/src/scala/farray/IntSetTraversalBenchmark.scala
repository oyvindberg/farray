package farray

import org.openjdk.jmh.annotations.Benchmark

/** Full-traversal ops: forall (all-true → no short-circuit), exists (== miss → never found, full scan),
  * count (evens), foreach (sum). Competitors traverse natively or via their primitive iterator. */
class IntSetForallBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = fsetA.forall(x => x >= 0)
  @Benchmark def scalaset(): Boolean = sSetA.forall(_ >= 0)
  @Benchmark def immbitset(): Boolean = immBitA.forall(_ >= 0)
  @Benchmark def fastutil(): Boolean = { val it = fuA.iterator(); var ok = true; while (ok && it.hasNext) ok = it.nextInt() >= 0; ok }
  @Benchmark def hppc(): Boolean = { val it = hppcA.iterator(); var ok = true; while (ok && it.hasNext) ok = it.next().value >= 0; ok }
}

class IntSetExistsBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = fsetA.exists(x => x == miss)
  @Benchmark def scalaset(): Boolean = sSetA.exists(_ == miss)
  @Benchmark def immbitset(): Boolean = immBitA.exists(_ == miss)
  @Benchmark def fastutil(): Boolean = { val it = fuA.iterator(); var f = false; while (!f && it.hasNext) f = it.nextInt() == miss; f }
  @Benchmark def hppc(): Boolean = { val it = hppcA.iterator(); var f = false; while (!f && it.hasNext) f = it.next().value == miss; f }
}

class IntSetCountBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = fsetA.count(x => (x & 1) == 0)
  @Benchmark def scalaset(): Int = sSetA.count(x => (x & 1) == 0)
  @Benchmark def immbitset(): Int = immBitA.count(x => (x & 1) == 0)
  @Benchmark def fastutil(): Int = { val it = fuA.iterator(); var c = 0; while (it.hasNext) if ((it.nextInt() & 1) == 0) c += 1; c }
  @Benchmark def hppc(): Int = { val it = hppcA.iterator(); var c = 0; while (it.hasNext) if ((it.next().value & 1) == 0) c += 1; c }
}

class IntSetForeachBenchmark extends IntSetInputs {
  @Benchmark def fset(): Long = { var s = 0L; fsetA.foreach(x => s += x); s }
  @Benchmark def scalaset(): Long = { var s = 0L; sSetA.foreach(x => s += x); s }
  @Benchmark def immbitset(): Long = { var s = 0L; immBitA.foreach(x => s += x); s }
  @Benchmark def fastutil(): Long = { var s = 0L; val it = fuA.iterator(); while (it.hasNext) s += it.nextInt(); s }
  @Benchmark def hppc(): Long = { var s = 0L; val it = hppcA.iterator(); while (it.hasNext) s += it.next().value; s }
}
