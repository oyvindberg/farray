package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Specialized (unboxed) primitive comparator — a non-inline alternative to the inline comparator.
trait IntLess { def lt(a: Int, b: Int): Boolean }

// Isolated A/B: identical bottom-up mergesort, comparator INLINE vs a non-inline (unboxed) IntLess call
// per comparison. Measures exactly the cost the inline `sort${k}` avoids. clone() each op so we sort fresh.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class SortDispatchBenchmark {
  @Param(Array("10000")) var size: Int = 10000
  var scrambled: Array[Int] = _
  var sorted: Array[Int] = _
  @Setup def setup(): Unit =
    scrambled = Array.tabulate(size)(i => (i * 1103515245 + 12345) & 0x7fffffff)
    sorted = Array.tabulate(size)(i => i)

  inline def msInline(src0: Array[Int], n: Int)(inline less: (Int, Int) => Boolean): Array[Int] =
    var src = src0; var dst = new Array[Int](n); var width = 1
    while width < n do
      var lo = 0
      while lo < n do
        val mid = math.min(lo + width, n); val hi = math.min(lo + 2 * width, n)
        var i = lo; var j = mid; var k = lo
        while i < mid && j < hi do {
          if less(src(j), src(i)) then { dst(k) = src(j); j += 1 }
          else { dst(k) = src(i); i += 1 };
          k += 1
        }
        while i < mid do { dst(k) = src(i); i += 1; k += 1 }
        while j < hi do { dst(k) = src(j); j += 1; k += 1 }
        lo += 2 * width
      val t = src; src = dst; dst = t; width *= 2
    src

  def msNonInline(src0: Array[Int], n: Int, less: IntLess): Array[Int] =
    var src = src0; var dst = new Array[Int](n); var width = 1
    while width < n do
      var lo = 0
      while lo < n do
        val mid = math.min(lo + width, n); val hi = math.min(lo + 2 * width, n)
        var i = lo; var j = mid; var k = lo
        while i < mid && j < hi do {
          if less.lt(src(j), src(i)) then { dst(k) = src(j); j += 1 }
          else { dst(k) = src(i); i += 1 };
          k += 1
        }
        while i < mid do { dst(k) = src(i); i += 1; k += 1 }
        while j < hi do { dst(k) = src(j); j += 1; k += 1 }
        lo += 2 * width
      val t = src; src = dst; dst = t; width *= 2
    src

  @Benchmark def inline_scrambled(): Array[Int] = msInline(scrambled.clone(), size)((x, y) => x < y)
  @Benchmark def nonInline_scrambled(): Array[Int] = msNonInline(scrambled.clone(), size, (x, y) => x < y)

  // 5 DISTINCT comparator classes (same order, different lambdas) -> the shared non-inline call site is
  // megamorphic (JIT can't inline -> vtable per comparison). The inline version stays 5 monomorphic copies.
  val comps: Array[IntLess] = Array((x, y) => x < y, (x, y) => x <= y && x != y, (x, y) => (x ^ 0) < y, (x, y) => x.compareTo(y) < 0, (x, y) => !(x >= y))
  @Benchmark def nonInline_mega(): Int =
    var acc = 0; var c = 0
    while c < 5 do { acc += msNonInline(scrambled.clone(), size, comps(c))(size / 2); c += 1 }
    acc
  @Benchmark def inline_mega(): Int =
    var acc = 0
    acc += msInline(scrambled.clone(), size)((x, y) => x < y)(size / 2)
    acc += msInline(scrambled.clone(), size)((x, y) => x <= y && x != y)(size / 2)
    acc += msInline(scrambled.clone(), size)((x, y) => (x ^ 0) < y)(size / 2)
    acc += msInline(scrambled.clone(), size)((x, y) => x.compareTo(y) < 0)(size / 2)
    acc += msInline(scrambled.clone(), size)((x, y) => !(x >= y))(size / 2)
    acc
}
