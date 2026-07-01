package farray

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

/** Validates the "logical length <= backing-array length" iteration shape that the generated `{K}Arr`
  * leaves will adopt: a leaf may hold a backing array LONGER than its logical length (so structural ops
  * can share/keep the array instead of copying). Every traversal loop must therefore (a) stop at the
  * logical length and (b) STILL let HotSpot drop the per-element bounds check and vectorize.
  *
  * The canonical shape is:
  *
  *   while (i < arr.length && i < len) { ... arr(i) ... }
  *
  * The first, short-circuiting `i < arr.length` proves `i` is in-bounds for `arr` at the point of the
  * `arr(i)` read (so HotSpot eliminates the bounds check + can vectorize); the second `i < len` caps the
  * trip count at the logical length.
  *
  * `arrLenOnly` is the ideal tight `while (i < arr.length)` loop over an exactly-sized array — the number
  * every other variant must TIE. `compoundNoSlack`/`compoundSlack` are the proposed shape (len == and <
  * arr.length respectively). `lenOnlySlack` (`while (i < len)`, `len` not provably <= arr.length) and
  * `minSlack` (`while (i < min(arr.length, len))`) are the alternatives we suspect are slower — included so
  * the choice is measured, not assumed.
  *
  * Every variant sums EXACTLY `size` ints, so Throughput (ops/s) is directly comparable across all of them.
  */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array(
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC",
    "-XX:-UseAdaptiveSizePolicy",
    "-XX:MaxInlineLevel=20",
    "-XX:InlineSmallCode=1500",
    "-XX:+AlwaysPreTouch"
  )
)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ArrLenIterBenchmark {
  @Param(Array("10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  // Exactly-sized array (physical length == logical length) — for the ideal baseline.
  var arrExact: Array[Int] = _
  // Over-allocated array (physical length == 2*logical) — models a leaf whose backing store outlives a
  // structural op that shrank the logical length without copying.
  var arrSlack: Array[Int] = _
  // Logical length, read from a field so HotSpot can't constant-fold `len == arr.length`.
  var len: Int = _

  @Setup def setup(): Unit = {
    arrExact = Array.tabulate(size)(i => i)
    arrSlack = Array.tabulate(size * 2)(i => i)
    len = size
  }

  // Ideal baseline: JIT-recognized array iteration, bounds checks eliminated, vectorized.
  @Benchmark def arrLenOnly(): Int = {
    val a = arrExact; var i = 0; var s = 0
    while (i < a.length) { s += a(i); i += 1 }
    s
  }

  // Proposed shape, no slack (len == arr.length): isolates the per-iteration cost of the extra `&& i < len`.
  @Benchmark def compoundNoSlack(): Int = {
    val a = arrExact; val n = len; var i = 0; var s = 0
    while (i < a.length && i < n) { s += a(i); i += 1 }
    s
  }

  // Proposed shape, realistic slack (len < arr.length): the actual use case — backing array is 2x logical.
  @Benchmark def compoundSlack(): Int = {
    val a = arrSlack; val n = len; var i = 0; var s = 0
    while (i < a.length && i < n) { s += a(i); i += 1 }
    s
  }

  // Anti-pattern: len-only cap over the over-allocated array. `len` is not provably <= a.length, so the
  // bounds check on a(i) may survive (no vectorization).
  @Benchmark def lenOnlySlack(): Int = {
    val a = arrSlack; val n = len; var i = 0; var s = 0
    while (i < n) { s += a(i); i += 1 }
    s
  }

  // Alternative: precompute min(arr.length, len). Does HotSpot prove the result <= a.length for BCE?
  @Benchmark def minSlack(): Int = {
    val a = arrSlack; val n = math.min(a.length, len); var i = 0; var s = 0
    while (i < n) { s += a(i); i += 1 }
    s
  }
}
