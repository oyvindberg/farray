package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Round-8 item 3 probe: two-operand scans (corresponds/sameElements/eqElements shape) must NOT materialize a
// non-leaf operand. The old matchAll2 fallback `flatOf(that)` allocated one Object[] per non-leaf operand
// (materializeRef — the #1 alloc leaf site, 2.1%), firing even for a RefOne (a 1-element array alloc per
// compare). Shapes (xs is always a flat leaf; `that` varies), all equal-content so the scan runs to the end:
//  - leafLeaf:   that is a leaf         (control — unchanged both-leaf tight loop)
//  - one:        that is a RefOne       (n==1; the "even a RefOne" allocation the walk removes)
//  - prepend:    that = h +: leaf       (Prepend over a leaf — O(1) per-index read, was a materialize)
//  - concat:     that = leafA ++ leafB  (Concat node — was a materialize)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class CorrespondsRefBenchmark:
  @Param(Array("1", "8", "64")) var size: Int = 8

  var xs: FArray[String] = _
  var thatLeaf: FArray[String] = _
  var thatOne: FArray[String] = _
  var thatPrepend: FArray[String] = _
  var thatConcat: FArray[String] = _

  @Setup def setup(): Unit =
    xs = FArray.tabulate(size)(i => "s" + i)
    thatLeaf = FArray.tabulate(size)(i => "s" + i)
    thatOne = FArray("s0") // a RefOne
    // Prepend over a leaf: h +: [s1..s{n-1}] == [s0, s1, ...]
    thatPrepend = "s0" +: FArray.tabulate(if size <= 1 then 0 else size - 1)(i => "s" + (i + 1))
    // Concat of two leaves, total length == size
    val half = size / 2
    thatConcat = FArray.tabulate(half)(i => "s" + i) ++ FArray.tabulate(size - half)(i => "s" + (half + i))

  @Benchmark def corr_leafLeaf(): Boolean = xs.sameElements(thatLeaf)
  @Benchmark def corr_one(): Boolean = xs.take(1).sameElements(thatOne)
  @Benchmark def corr_prepend(): Boolean = xs.sameElements(thatPrepend)
  @Benchmark def corr_concat(): Boolean = xs.sameElements(thatConcat)
