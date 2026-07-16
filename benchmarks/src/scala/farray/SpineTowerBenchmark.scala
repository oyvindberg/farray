package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Round-8 item 1 probe: forward traversal over RIGHT-LEANING spines built via `++` / `+:`.
//  - concatTower:  leaf ++ (leaf ++ (... ++ leaf))  => Concat(leaf, Concat(leaf, ... )) — the shape the
//                  spine-peel targets. WITHOUT the peel, every non-leaf traversal allocates a `new FBase[16]`
//                  DFS stack; WITH it, a right-leaning Concat(leaf, tower) walks the spine with ZERO alloc.
//  - prependTower: e +: (e +: (... +: leaf))  => Prepend(e, Prepend(e, ...)) — the fwd Prepend arm was
//                  already spine-iterative; this is the zero-alloc control.
// Built once in @Setup; the benchmarks traverse repeatedly, so `-prof gc` measures TRAVERSAL allocation
// (the work stack), not construction. exists(_ < 0) never hits -> full traversal.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class SpineTowerIntBenchmark:
  @Param(Array("4", "16", "64")) var depth: Int = 16
  @Param(Array("1", "8")) var leafSize: Int = 1

  var concatTower: FArray[Int] = _
  var prependTower: FArray[Int] = _

  @Setup def setup(): Unit =
    val leaf = FArray.tabulate(leafSize)(i => i)
    var c = leaf
    var i = 1
    while i < depth do { c = leaf ++ c; i += 1 }
    concatTower = c
    var p = leaf
    var j = 0
    while j < depth do { p = j +: p; j += 1 }
    prependTower = p

  @Benchmark def concat_foldLeft(): Int = concatTower.foldLeft(0)(_ + _)
  @Benchmark def concat_exists(): Boolean = concatTower.exists(_ < 0)
  @Benchmark def concat_map(): FArray[Int] = concatTower.map(_ + 1)
  @Benchmark def concat_foreach(): Int =
    var s = 0; concatTower.foreach(x => s += x); s
  @Benchmark def prepend_foldLeft(): Int = prependTower.foldLeft(0)(_ + _)
  @Benchmark def prepend_exists(): Boolean = prependTower.exists(_ < 0)
