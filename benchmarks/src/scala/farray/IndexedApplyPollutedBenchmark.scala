package farray

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

/** Megamorphic-node indexed access — the honest test for the item-1 inline-leaf fast path.
  *
  * `FArrayOps.refAt` is a SHARED non-inline method whose body is an 11-case node `instanceof` chain. In the migrated compiler (JFR: refAt +1.44pp self-time) it
  * is called from hundreds of `xs(i)`/`head`/`last` sites with EVERY node shape (RefArr, RefOne, SliceNode, Concat, Prepend, ReverseNode…) flowing through it,
  * so its type profile is polluted and it stays out-of-line: a tight `while (i < n) xs(i)` loop over a flat `RefArr` leaf pays a full megamorphic dispatch per
  * element. A monomorphic microbenchmark CANNOT reproduce this — HotSpot inlines refAt and predicts the first branch — so this bench first POLLUTES refAt's
  * profile with five other node shapes, then measures the dominant hot loop: an indexed sum over a flat `RefArr`.
  *
  * Item 1 peels the `if (xs.isInstanceOf[RefArr]) leaf.arr(i) else refAt(xs, i)` test OUT to the (per-site monomorphic) inlined `applyAtImpl` body, so the
  * RefArr loop reads directly and refAt handles only the tail. `refArrHot` is the number that should move; `mixedShapes` is the pollution driver kept as a
  * control.
  */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 2,
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
class IndexedApplyPollutedBenchmark {
  @Param(Array("2", "3", "8", "32"))
  var size: Int = 3

  // The hot, flat RefArr leaf whose indexed loop should benefit.
  var fArr: FArray[String] = _
  // A flat RefOne singleton — the histogram's #1 shape (63% of ctors) and its single biggest cell
  // (`at`·RefOne·1 = 30.9M/compile). Its element read is the item-1 RefOne-peel target; before the
  // peel `fOne(0)` fails the `isInstanceOf[RefArr]` test and falls to the out-of-line megamorphic refAt.
  var fOne: FArray[String] = _
  // Five other node shapes to pollute the shared refAt's type profile (all FArray[String]).
  var fSlice: FArray[String] = _ // SliceNode over a RefArr
  var fConcat: FArray[String] = _ // Concat tree
  var fPrepend: FArray[String] = _ // RefPrepend
  var fAppend: FArray[String] = _ // RefAppend
  var fReverse: FArray[String] = _ // ReverseNode

  @Setup def setup(): Unit = {
    fArr = FArray.tabulate(size)(_.toString)
    fOne = FArray("solo") // RefOne — length 1 regardless of `size`

    // drop(1) on a size+1 array yields a SliceNode of logical length `size`
    fSlice = FArray.tabulate(size + 1)(_.toString).drop(1)
    val half = FArray.tabulate((size + 1) / 2)(_.toString)
    fConcat = half ++ FArray.tabulate(size - half.length)(i => (i + 100).toString)
    fPrepend = "p" +: FArray.tabulate(size - 1)(_.toString)
    fAppend = FArray.tabulate(size - 1)(_.toString) :+ "a"
    fReverse = FArray.tabulate(size)(_.toString).reverse
  }

  private def sumApply(xs: FArray[String]): Int = {
    val n = xs.length; var i = 0; var s = 0
    while (i < n) { s += xs(i).length; i += 1 }
    s
  }

  // Drives the pollution: one indexed loop over each non-RefArr node shape. Keeps refAt megamorphic.
  @Benchmark def mixedShapes(bh: Blackhole): Unit = {
    bh.consume(sumApply(fSlice))
    bh.consume(sumApply(fConcat))
    bh.consume(sumApply(fPrepend))
    bh.consume(sumApply(fAppend))
    bh.consume(sumApply(fReverse))
  }

  // The hot RefArr loop, run alongside the pollution shapes so the SHARED `refAt` method is megamorphic when
  // the JIT compiles this method (JMH runs one @Benchmark per fork, so pollution must live in-method). The
  // hot loop below is its OWN textual `xs(i)` call site — its inlined `applyAtImpl` sees only RefArr, so the
  // item-1 fast path's `isInstanceOf[RefArr]` is monomorphic-true here and reads directly, while the shared
  // refAt (still called from the polluted `sumApply` sites) stays out-of-line. This isolates the win.
  @Benchmark def refArrHot(bh: Blackhole): Unit = {
    // Pollute refAt's profile with the five non-RefArr node shapes (shared sumApply call site).
    bh.consume(sumApply(fSlice))
    bh.consume(sumApply(fConcat))
    bh.consume(sumApply(fPrepend))
    bh.consume(sumApply(fAppend))
    bh.consume(sumApply(fReverse))
    // Dominant hot RefArr loop — a DISTINCT `xs(i)` site (not via sumApply), repeated 16x to weight it.
    var acc = 0
    var k = 0
    while (k < 16) {
      val xs = fArr; val n = xs.length; var i = 0; var s = 0
      while (i < n) { s += xs(i).length; i += 1 }
      acc += s; k += 1
    }
    bh.consume(acc)
  }

  // The hot RefOne read — the REAL dotc profile (`at`·RefOne·1, the 30.9M single biggest cell). Run
  // alongside the same five pollution shapes so the SHARED `refAt` stays megamorphic and out-of-line.
  // Before the item-1 RefOne peel, `fOne(0)` fails `isInstanceOf[RefArr]` and pays a full megamorphic
  // `refAt` dispatch; after the peel it reads `.elem` inline. `fOne` is length 1, so the read is repeated
  // 16x (mirroring refArrHot's weighting) rather than looped over n. Independent of `size`.
  @Benchmark def refOneHot(bh: Blackhole): Unit = {
    bh.consume(sumApply(fSlice))
    bh.consume(sumApply(fConcat))
    bh.consume(sumApply(fPrepend))
    bh.consume(sumApply(fAppend))
    bh.consume(sumApply(fReverse))
    // Dominant hot RefOne read — a DISTINCT `xs(0)` site (not via sumApply), repeated 16x to weight it.
    var acc = 0
    var k = 0
    while (k < 16) {
      val xs = fOne
      acc += xs(0).length
      k += 1
    }
    bh.consume(acc)
  }
}
