package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** Fuse over each source SHAPE the element loop dispatches on: flat leaf (the fast path), a Concat tree (the fallback arm), and a One node (alloc-sensitive
  * tiny case). Gates the elementLoop dedup redesign: leaf entries must tie the old two-arm loop exactly, tree/One entries measure the fallback strategy
  * (per-element `<kind>At` vs null-unswitched read vs materialize-once).
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class FuseSourceShapeBench:
  @Param(Array("1000", "100000"))
  var size: Int = 1000

  var leaf: FArray[Int] = _
  var tree: FArray[Int] = _ // ++ of 8 leaves — stays a Concat node
  var one: FArray[Int] = _
  var last: Int = _

  @Setup
  def setup(): Unit =
    leaf = FArray.tabulate(size)(i => i)
    val seg = size / 8
    var t = FArray.tabulate(seg)(i => i)
    var k = 1
    while k < 8 do
      val base = k * seg
      t = t ++ FArray.tabulate(if k == 7 then size - base else seg)(i => base + i)
      k += 1
    tree = t
    one = FArray(42)
    last = size - 1
    tinyTree = FArray(1, 2) ++ FArray(3, 4)

  // control: flat-leaf source — any redesign must tie the old leaf arm exactly
  @Benchmark def leafMapFilterRun(): FArray[Int] = leaf.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).run
  @Benchmark def leafFold(): Int = leaf.fuse.foldLeft(0)(_ + _)

  // tree source: the fallback arm
  @Benchmark def treeMapFilterRun(): FArray[Int] = tree.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).run
  @Benchmark def treeFold(): Int = tree.fuse.foldLeft(0)(_ + _)
  @Benchmark def treeExistsEarly(): Boolean = tree.fuse.exists(_ == 0)
  @Benchmark def treeExistsLate(): Boolean = tree.fuse.exists(_ == last)

  // One-node source: any per-call allocation shows up here
  @Benchmark def oneMapRun(): FArray[Int] = one.fuse.map(_ + 1).run
  @Benchmark def oneFold(): Int = one.fuse.foldLeft(0)(_ + _)

  // general (non-literal) flatMap inners — these DO go through elementLoop, unlike the literal
  // FArray(a, b) inners in FusionBench/AllOps benches, which the macro splats without a loop.
  // Fresh leaf per element: the EA/scalar-replacement-sensitive case the leaf fast-path exists for.
  @Benchmark def flatMapFreshLeafInner(): Int =
    leaf.fuse.flatMap(x => FArray.tabulate(2)(i => x + i)).foldLeft(0)(_ + _)
  // Shared tiny tree per element: the fallback arm inside a hot nested loop (per-element re-materialize
  // would be an alloc storm here; per-element <kind>At is a cheap shallow walk).
  var tinyTree: FArray[Int] = _
  @Benchmark def flatMapTreeInner(): Int =
    leaf.fuse.flatMap(_ => tinyTree).foldLeft(0)(_ + _)
