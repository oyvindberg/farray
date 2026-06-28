package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** NESTED FUSION — `groupAdjacentReduceBy(key)(prep)(agg)`. For input ALREADY CLUSTERED by key, reduce each maximal run with a FUSED inner sub-pipeline,
  * emitting `(k, result)` per run. When the aggregate is a fold, the run's rows are NEVER materialized: the inner fold runs inline as rows stream past — O(1)
  * memory per group, zero per-group allocation.
  *
  * There is no stdlib operation for "reduce each adjacent run" — so the competitor methods are the realistic hand- written equivalent: a fold that detects key
  * changes and emits `(k, reduced)` per run (the standard "the rows you'd otherwise write"). The List/Vector/IArray variants box every Int, allocate per-run
  * buffers / intermediate collections, and build a tuple per run. FArray fused does ONE unboxed pass with a single per-run accumulator.
  *
  * Input is `i => i / RunLen` clustered: `RunLen` equal keys in a row, so a size-N input has N/RunLen runs.
  */
object NestedFusionBenchmark:
  final val RunLen = 16 // elements per run (so N elements → N/16 runs)

abstract class ClusteredIntInputs extends CommonParams:
  import NestedFusionBenchmark.RunLen
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var farrayInput: FArray[Int] = _

  // key(i) = i / RunLen → contiguous runs of RunLen equal keys. value(i) = i (the element itself).
  inline def keyOf(i: Int): Int = i / RunLen

  @Setup
  def setup(): Unit =
    val arr = Array.tabulate(size)(i => i)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i)
    farrayInput = FArray.tabulate(size)(i => i)

/** ── group-and-SUM per run: the headline fold. Σ over each run, emit (key, sum). ───────────────────────────────── */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntGroupSumBenchmark extends ClusteredIntInputs:

  @Benchmark def farray_groupSum(): FArray[(Int, Int)] =
    farrayInput.fuse.groupAdjacentReduceBy(keyOf)(_.map(identity))(Agg.sum(identity)).run

  // the hand-written adjacent-run sum over a List (boxes, builds a List of tuples).
  @Benchmark def list_groupSum(): List[(Int, Int)] =
    var out = List.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var acc = 0
    var rest = listInput
    while rest.nonEmpty do
      val a = rest.head; rest = rest.tail
      val k = keyOf(a)
      if !started then { curKey = k; acc = a; started = true }
      else if k == curKey then acc += a
      else { out += ((curKey, acc)); curKey = k; acc = a }
    if started then out += ((curKey, acc))
    out.result()

  @Benchmark def vector_groupSum(): Vector[(Int, Int)] =
    val out = Vector.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var acc = 0; var i = 0
    val n = vectorInput.length
    while i < n do
      val a = vectorInput(i); val k = keyOf(a)
      if !started then { curKey = k; acc = a; started = true }
      else if k == curKey then acc += a
      else { out += ((curKey, acc)); curKey = k; acc = a }
      i += 1
    if started then out += ((curKey, acc))
    out.result()

  @Benchmark def iarray_groupSum(): IArray[(Int, Int)] =
    val out = IArray.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var acc = 0; var i = 0
    val n = iarrayInput.length
    while i < n do
      val a = iarrayInput(i); val k = keyOf(a)
      if !started then { curKey = k; acc = a; started = true }
      else if k == curKey then acc += a
      else { out += ((curKey, acc)); curKey = k; acc = a }
      i += 1
    if started then out += ((curKey, acc))
    out.result()

/** ── group-and-sum, then `.map(_._2).sum` — the (K,R) tuple is never even built in the fused pipeline. ──────────── */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntGroupSumProjectBenchmark extends ClusteredIntInputs:

  // fused: the (Int,Int) pair is decomposed — `.map(_._2)` reads only the run sum, the key column is dead, no tuple.
  @Benchmark def farray_groupProject(): Int =
    farrayInput.fuse.groupAdjacentReduceBy(keyOf)(_.map(identity))(Agg.sum(identity)).map(_._2).sum

  @Benchmark def list_groupProject(): Int =
    var total = 0; var started = false; var curKey = 0; var acc = 0
    var rest = listInput
    while rest.nonEmpty do
      val a = rest.head; rest = rest.tail; val k = keyOf(a)
      if !started then { curKey = k; acc = a; started = true }
      else if k == curKey then acc += a
      else { total += acc; curKey = k; acc = a }
    if started then total += acc
    total

  @Benchmark def vector_groupProject(): Int =
    var total = 0; var started = false; var curKey = 0; var acc = 0; var i = 0
    val n = vectorInput.length
    while i < n do
      val a = vectorInput(i); val k = keyOf(a)
      if !started then { curKey = k; acc = a; started = true }
      else if k == curKey then acc += a
      else { total += acc; curKey = k; acc = a }
      i += 1
    if started then total += acc
    total

  @Benchmark def iarray_groupProject(): Int =
    var total = 0; var started = false; var curKey = 0; var acc = 0; var i = 0
    val n = iarrayInput.length
    while i < n do
      val a = iarrayInput(i); val k = keyOf(a)
      if !started then { curKey = k; acc = a; started = true }
      else if k == curKey then acc += a
      else { total += acc; curKey = k; acc = a }
      i += 1
    if started then total += acc
    total

/** ── inner FILTER + count: count only the even rows per run. The inner stage runs inline; no per-run buffer. ────── */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntGroupFilterCountBenchmark extends ClusteredIntInputs:

  @Benchmark def farray_groupFilterCount(): FArray[(Int, Int)] =
    farrayInput.fuse.groupAdjacentReduceBy(keyOf)(_.filter(_ % 2 == 0))(Agg.count).run

  @Benchmark def list_groupFilterCount(): List[(Int, Int)] =
    val out = List.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var cnt = 0
    var rest = listInput
    while rest.nonEmpty do
      val a = rest.head; rest = rest.tail; val k = keyOf(a)
      if !started then { curKey = k; cnt = if a % 2 == 0 then 1 else 0; started = true }
      else if k == curKey then { if a % 2 == 0 then cnt += 1 }
      else { out += ((curKey, cnt)); curKey = k; cnt = if a % 2 == 0 then 1 else 0 }
    if started then out += ((curKey, cnt))
    out.result()

  @Benchmark def vector_groupFilterCount(): Vector[(Int, Int)] =
    val out = Vector.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var cnt = 0; var i = 0
    val n = vectorInput.length
    while i < n do
      val a = vectorInput(i); val k = keyOf(a)
      if !started then { curKey = k; cnt = if a % 2 == 0 then 1 else 0; started = true }
      else if k == curKey then { if a % 2 == 0 then cnt += 1 }
      else { out += ((curKey, cnt)); curKey = k; cnt = if a % 2 == 0 then 1 else 0 }
      i += 1
    if started then out += ((curKey, cnt))
    out.result()

  @Benchmark def iarray_groupFilterCount(): IArray[(Int, Int)] =
    val out = IArray.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var cnt = 0; var i = 0
    val n = iarrayInput.length
    while i < n do
      val a = iarrayInput(i); val k = keyOf(a)
      if !started then { curKey = k; cnt = if a % 2 == 0 then 1 else 0; started = true }
      else if k == curKey then { if a % 2 == 0 then cnt += 1 }
      else { out += ((curKey, cnt)); curKey = k; cnt = if a % 2 == 0 then 1 else 0 }
      i += 1
    if started then out += ((curKey, cnt))
    out.result()

/** ── group-and-MIN per run. The competitor commonly materializes runs then `.min` — fused keeps one running best. ─ */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntGroupMinBenchmark extends ClusteredIntInputs:

  @Benchmark def farray_groupMin(): FArray[(Int, Option[Int])] =
    farrayInput.fuse.groupAdjacentReduceBy(keyOf)(_.map(identity))(Agg.min(identity)).run

  // the idiomatic competitor: buffer each run's rows, then `.min` it (allocates a buffer per run).
  @Benchmark def list_groupMin(): List[(Int, Int)] =
    val out = List.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var best = 0
    var rest = listInput
    while rest.nonEmpty do
      val a = rest.head; rest = rest.tail; val k = keyOf(a)
      if !started then { curKey = k; best = a; started = true }
      else if k == curKey then { if a < best then best = a }
      else { out += ((curKey, best)); curKey = k; best = a }
    if started then out += ((curKey, best))
    out.result()

  @Benchmark def vector_groupMin(): Vector[(Int, Int)] =
    val out = Vector.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var best = 0; var i = 0
    val n = vectorInput.length
    while i < n do
      val a = vectorInput(i); val k = keyOf(a)
      if !started then { curKey = k; best = a; started = true }
      else if k == curKey then { if a < best then best = a }
      else { out += ((curKey, best)); curKey = k; best = a }
      i += 1
    if started then out += ((curKey, best))
    out.result()

  @Benchmark def iarray_groupMin(): IArray[(Int, Int)] =
    val out = IArray.newBuilder[(Int, Int)]
    var started = false; var curKey = 0; var best = 0; var i = 0
    val n = iarrayInput.length
    while i < n do
      val a = iarrayInput(i); val k = keyOf(a)
      if !started then { curKey = k; best = a; started = true }
      else if k == curKey then { if a < best then best = a }
      else { out += ((curKey, best)); curKey = k; best = a }
      i += 1
    if started then out += ((curKey, best))
    out.result()

/** ── the naive idiomatic competitor: `groupBy`-style runs materialized, then reduced — what people ACTUALLY write when they don't hand-roll the fold. This is
  * the "real-world" gap: every run becomes a List, then `.sum`. ───────
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntGroupNaiveBenchmark extends ClusteredIntInputs:

  @Benchmark def farray_groupNaive(): FArray[(Int, Int)] =
    farrayInput.fuse.groupAdjacentReduceBy(keyOf)(_.map(identity))(Agg.sum(identity)).run

  // idiomatic Scala: split into adjacent runs (each a buffered List), then map each run to (key, run.sum).
  @Benchmark def list_groupNaive(): List[(Int, Int)] =
    listInput
      .foldRight(List.empty[(Int, List[Int])]) { (a, acc) =>
        acc match
          case (k, rows) :: t if k == keyOf(a) => (k, a :: rows) :: t
          case _                               => (keyOf(a), a :: Nil) :: acc
      }
      .map((k, rows) => (k, rows.sum))

  @Benchmark def vector_groupNaive(): Vector[(Int, Int)] =
    var out = Vector.empty[(Int, Vector[Int])]
    var i = 0; val n = vectorInput.length
    while i < n do
      val a = vectorInput(i); val k = keyOf(a)
      if out.nonEmpty && out.last._1 == k then out = out.updated(out.length - 1, (k, out.last._2 :+ a))
      else out = out :+ ((k, Vector(a)))
      i += 1
    out.map((k, rows) => (k, rows.sum))

  @Benchmark def iarray_groupNaive(): IArray[(Int, Int)] =
    val runs = scala.collection.mutable.ArrayBuffer.empty[(Int, scala.collection.mutable.ArrayBuffer[Int])]
    var i = 0; val n = iarrayInput.length
    while i < n do
      val a = iarrayInput(i); val k = keyOf(a)
      if runs.nonEmpty && runs.last._1 == k then runs.last._2 += a
      else runs += ((k, scala.collection.mutable.ArrayBuffer(a)))
      i += 1
    IArray.from(runs.map((k, rows) => (k, rows.sum)))
