package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Measures the short-circuit/backward ops on NON-LEAF shapes, where the dfsC/dfsCB/materialize fix matters.
// Before the fix these fell to per-element kindAt: O(n*depth) for the concat tree, O(n^2) for the append chain.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class TraversalShapeBenchmark:
  @Param(Array("4000"))
  var size: Int = 0

  var flat: FArray[Int] = FArray.empty[Int]
  var concatTree: FArray[Int] = FArray.empty[Int]
  var appendChain: FArray[Int] = FArray.empty[Int]

  @Setup def setup(): Unit =
    flat = FArray.tabulate(size)(identity)
    def build(lo: Int, hi: Int): FArray[Int] =
      if hi - lo <= 8 then FArray.tabulate(hi - lo)(i => lo + i)
      else { val mid = (lo + hi) / 2; build(lo, mid) ++ build(mid, hi) }
    concatTree = build(0, size)
    var a = FArray(0); var i = 1; while i < size do { a = a :+ i; i += 1 }; appendChain = a

  // exists with a never-true predicate = worst-case full scan (no early exit helps)
  @Benchmark def exists_flat(): Boolean = flat.exists(_ < 0)
  @Benchmark def exists_concat(): Boolean = concatTree.exists(_ < 0)
  @Benchmark def exists_appendChain(): Boolean = appendChain.exists(_ < 0)

  @Benchmark def sum_flat(): Int = flat.sum
  @Benchmark def sum_concat(): Int = concatTree.sum
  @Benchmark def sum_appendChain(): Int = appendChain.sum

  @Benchmark def foldRight_flat(): Int = flat.foldRight(0)(_ + _)
  @Benchmark def foldRight_concat(): Int = concatTree.foldRight(0)(_ + _)
  @Benchmark def foldRight_appendChain(): Int = appendChain.foldRight(0)(_ + _)
