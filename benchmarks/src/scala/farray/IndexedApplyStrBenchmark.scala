package farray

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

/** Mirrors the dotc hot shape driving `FArrayOps.refAt` (+1.44pp self-time in the migrated-compiler JFR):
  * a TIGHT while-loop indexing a small ref sequence — `Hashable.finishHash` (`tps(idx)`), `Types.mapArgs`
  * (`args(i)`/`tparams(i)`). The workload traverses millions of tiny (0-3 element) `RefArr` leaves, indexing
  * each. This bench sums `xs(i).length` in a `while (i < n) …` loop over the whole sequence, at the small
  * sizes the compiler actually sees. Target: FArray parity with IArray/raw array on the RefArr case, beating
  * Vector.apply at all small sizes. NOT a scorecard entry (each impl sums identically; it is an A/B validator
  * for the item-1 inline-leaf fast path).
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
class IndexedApplyStrBenchmark {
  @Param(Array("1", "2", "3", "8", "32", "128"))
  var size: Int = 3

  var listInput: List[String] = _
  var farrayInput: FArray[String] = _
  var iarrayInput: IArray[String] = _
  var vectorInput: Vector[String] = _

  @Setup def setup(): Unit = {
    val arr = Array.tabulate(size)(_.toString)
    listInput = arr.toList
    farrayInput = FArray.tabulate(size)(_.toString)
    iarrayInput = IArray.tabulate(size)(_.toString)
    vectorInput = arr.toVector
  }

  @Benchmark def farray(): Int = {
    val xs = farrayInput; val n = xs.length; var i = 0; var s = 0
    while (i < n) { s += xs(i).length; i += 1 }
    s
  }

  @Benchmark def iarray(): Int = {
    val xs = iarrayInput; val n = xs.length; var i = 0; var s = 0
    while (i < n) { s += xs(i).length; i += 1 }
    s
  }

  @Benchmark def vector(): Int = {
    val xs = vectorInput; val n = xs.length; var i = 0; var s = 0
    while (i < n) { s += xs(i).length; i += 1 }
    s
  }

  // List has no O(1) apply; the compiler's List loops are cons-walks, so the fair List shape is a foreach.
  @Benchmark def list(): Int = {
    var s = 0; var cur = listInput
    while (cur.nonEmpty) { s += cur.head.length; cur = cur.tail }
    s
  }
}
