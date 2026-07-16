package farray

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

// CompilerShapeBenchmark — the acceptance metric for the "listealization" plan (docs/listealization-plan.md).
//
// It reproduces the dotc workload the FArray migration is up against, WEIGHTED from the instrumented shape
// histogram (docs/listealization-histogram.md): millions of 0-3-element REF sequences, built once, traversed
// once, frequently destructured via `case h +: t`, rarely random-indexed. Every op site is fed a MEGAMORPHIC
// mix of node shapes (RefOne / RefArr / RefPrepend / Concat / SliceNode) at the measured size mix, exactly as
// the compiler produces them — so the internal dispatch goes >=3-wide (the histogram shows 4-6 receiver types
// at the hot `at`/traversal sites) and the JIT cannot devirtualise, just like reality.
//
// Candidates run the SAME logical data: List and Vector are built from each FArray's elements. FArray gets its
// natural node-shape diversity; List is always cons, Vector is always Vector — each competitor's own tiny-seq
// representation. Target (per the plan): parity with List on the weighted mix.
//
// Run under bench-lock discipline, e.g.:
//   scripts/bench-lock.sh bleep run benchmarks-runner -- 'CompilerShapeBenchmark' -f 1 -wi 3 -i 5
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class CompilerShapeBenchmark:

  // ---- megamorphic input set: the histogram's shape x size mix (63% size-1, ~18% size-2, ~7% size-3, tail) ----
  // FArray inputs built through DISTINCT construction paths so the array holds several runtime node classes.
  var fa: Array[FArray[String]] = null
  var li: Array[List[String]] = null
  var ve: Array[Vector[String]] = null

  // non-empty inputs for destructuring (>= 3 elements so 1-/2-/3-deep `+:` all match), same shape diversity.
  var faD: Array[FArray[String]] = null
  var liD: Array[List[String]] = null
  var veD: Array[Vector[String]] = null

  // construction operands (set in @Setup so the JIT can't constant-fold the literals).
  var a, b, c, d, e, f, g: String = null

  // D5 worklist items — consed one-by-one into an accumulator, then drained (see worklist_* below).
  var work: Array[String] = null

  @Setup def setup(): Unit =
    a = "alpha"; b = "beta"; c = "gamma"; d = "delta"; e = "eps"; f = "phi"; g = "psi"
    work = Array(a, b, c, d, e, f, g, a, b, c) // 10-item worklist

    // logical mix ~= histogram construction buckets, with FArray NODE-CLASS diversity per size.
    val builders: Array[() => FArray[String]] = Array(
      () => FArray.empty[String], //                          empty
      () => FArray(a), // RefOne
      () => FArray(b), // RefOne
      () => FArray(c), // RefOne
      () => FArray(d), // RefOne
      () => FArray(e), // RefOne
      () => FArray(f), // RefOne
      () => FArray(g), // RefOne
      () => FArray(a, b), // RefArr size-2
      () => FArray(c, d), // RefArr size-2
      () => a +: FArray(b), // RefPrepend size-2
      () => FArray(g, a, b).drop(1), // SliceNode size-2 (drop(1) of a length-3 leaf)
      () => FArray(a, b, c), // RefArr size-3
      () => FArray.concat(FArray(a, b), FArray(c)), // Concat size-3
      () => FArray(g, a, b, c).drop(1), // SliceNode size-3
      () => FArray(a, b, c, d, e, f) // RefArr size-6 (the 4-8 tail)
    )
    fa = builders.map(_())
    li = fa.map(_.iterator.toList)
    ve = fa.map(_.iterator.toVector)

    // destructuring set: all >= 3 elements, same node-class diversity.
    val dBuilders: Array[() => FArray[String]] = Array(
      () => FArray(a, b, c), // RefArr
      () => FArray(a, b, c, d), // RefArr
      () => a +: FArray(b, c), // RefPrepend over RefArr
      () => a +: b +: FArray(c, d), // RefPrepend tower
      () => FArray.concat(FArray(a, b), FArray(c, d)), // Concat
      () => FArray(g, a, b, c, d).drop(1), // SliceNode length-4
      () => FArray(a, b, c, d, e, f), // RefArr size-6
      () => FArray.concat(FArray(a), FArray(b, c)) // Concat size-3
    )
    faD = dBuilders.map(_())
    liD = faD.map(_.iterator.toList)
    veD = faD.map(_.iterator.toVector)

  // ============================ construction (literal arities, histogram-weighted) ============================
  // ~8 x size-1, 2 x size-2, 2 x size-3, 1 x size-6 per invocation (mirrors the 63/18/7/7 ctor bucket split).
  @Benchmark def construct_farray(bh: Blackhole): Unit =
    bh.consume(FArray(a)); bh.consume(FArray(b)); bh.consume(FArray(c)); bh.consume(FArray(d))
    bh.consume(FArray(e)); bh.consume(FArray(f)); bh.consume(FArray(g)); bh.consume(FArray(a))
    bh.consume(FArray(a, b)); bh.consume(FArray(c, d))
    bh.consume(FArray(a, b, c)); bh.consume(FArray(d, e, f))
    bh.consume(FArray(a, b, c, d, e, f))
  @Benchmark def construct_list(bh: Blackhole): Unit =
    bh.consume(List(a)); bh.consume(List(b)); bh.consume(List(c)); bh.consume(List(d))
    bh.consume(List(e)); bh.consume(List(f)); bh.consume(List(g)); bh.consume(List(a))
    bh.consume(List(a, b)); bh.consume(List(c, d))
    bh.consume(List(a, b, c)); bh.consume(List(d, e, f))
    bh.consume(List(a, b, c, d, e, f))
  @Benchmark def construct_vector(bh: Blackhole): Unit =
    bh.consume(Vector(a)); bh.consume(Vector(b)); bh.consume(Vector(c)); bh.consume(Vector(d))
    bh.consume(Vector(e)); bh.consume(Vector(f)); bh.consume(Vector(g)); bh.consume(Vector(a))
    bh.consume(Vector(a, b)); bh.consume(Vector(c, d))
    bh.consume(Vector(a, b, c)); bh.consume(Vector(d, e, f))
    bh.consume(Vector(a, b, c, d, e, f))

  // ================================== foreach (megamorphic shape mix) =========================================
  @Benchmark def foreach_farray(bh: Blackhole): Unit =
    var acc = 0; var i = 0
    while i < fa.length do { fa(i).foreach(s => acc += s.length); i += 1 }
    bh.consume(acc)
  @Benchmark def foreach_list(bh: Blackhole): Unit =
    var acc = 0; var i = 0
    while i < li.length do { li(i).foreach(s => acc += s.length); i += 1 }
    bh.consume(acc)
  @Benchmark def foreach_vector(bh: Blackhole): Unit =
    var acc = 0; var i = 0
    while i < ve.length do { ve(i).foreach(s => acc += s.length); i += 1 }
    bh.consume(acc)

  // ================================== foldLeft ================================================================
  @Benchmark def foldLeft_farray(bh: Blackhole): Unit =
    var i = 0
    while i < fa.length do { bh.consume(fa(i).foldLeft(0)((z, s) => z + s.length)); i += 1 }
  @Benchmark def foldLeft_list(bh: Blackhole): Unit =
    var i = 0
    while i < li.length do { bh.consume(li(i).foldLeft(0)((z, s) => z + s.length)); i += 1 }
  @Benchmark def foldLeft_vector(bh: Blackhole): Unit =
    var i = 0
    while i < ve.length do { bh.consume(ve(i).foldLeft(0)((z, s) => z + s.length)); i += 1 }

  // ================================== map =====================================================================
  @Benchmark def map_farray(bh: Blackhole): Unit =
    var i = 0
    while i < fa.length do { bh.consume(fa(i).map(s => s.substring(0))); i += 1 }
  @Benchmark def map_list(bh: Blackhole): Unit =
    var i = 0
    while i < li.length do { bh.consume(li(i).map(s => s.substring(0))); i += 1 }
  @Benchmark def map_vector(bh: Blackhole): Unit =
    var i = 0
    while i < ve.length do { bh.consume(ve(i).map(s => s.substring(0))); i += 1 }

  // ================================== mapConserve (identity -> conserve fast path) ============================
  // List/Vector have no mapConserve; their nearest tiny-seq analogue is `map` (identity), the fair reference.
  @Benchmark def mapConserve_farray(bh: Blackhole): Unit =
    var i = 0
    while i < fa.length do { bh.consume(fa(i).mapConserve(s => s)); i += 1 }
  @Benchmark def mapConserve_list(bh: Blackhole): Unit =
    var i = 0
    while i < li.length do { bh.consume(li(i).map(s => s)); i += 1 }
  @Benchmark def mapConserve_vector(bh: Blackhole): Unit =
    var i = 0
    while i < ve.length do { bh.consume(ve(i).map(s => s)); i += 1 }

  // ================================== filterConserve (predicate keeps all -> conserve) ========================
  @Benchmark def filterConserve_farray(bh: Blackhole): Unit =
    var i = 0
    while i < fa.length do { bh.consume(fa(i).filterConserve(s => s.nonEmpty)); i += 1 }
  @Benchmark def filterConserve_list(bh: Blackhole): Unit =
    var i = 0
    while i < li.length do { bh.consume(li(i).filter(s => s.nonEmpty)); i += 1 }
  @Benchmark def filterConserve_vector(bh: Blackhole): Unit =
    var i = 0
    while i < ve.length do { bh.consume(ve(i).filter(s => s.nonEmpty)); i += 1 }

  // ================================== `case h +: t` destructuring (1-, 2-, 3-deep) ============================
  @Benchmark def destructure1_farray(bh: Blackhole): Unit =
    import farray.`+:`
    var i = 0
    while i < faD.length do
      faD(i) match { case h +: t => bh.consume(h); bh.consume(t); case _ => () }
      i += 1
  @Benchmark def destructure1_list(bh: Blackhole): Unit =
    var i = 0
    while i < liD.length do
      liD(i) match { case h :: t => bh.consume(h); bh.consume(t); case _ => () }
      i += 1
  @Benchmark def destructure1_vector(bh: Blackhole): Unit =
    var i = 0
    while i < veD.length do
      veD(i) match { case h +: t => bh.consume(h); bh.consume(t); case _ => () }
      i += 1

  @Benchmark def destructure2_farray(bh: Blackhole): Unit =
    import farray.`+:`
    var i = 0
    while i < faD.length do
      faD(i) match { case x +: y +: rest => bh.consume(x); bh.consume(y); bh.consume(rest); case _ => () }
      i += 1
  @Benchmark def destructure2_list(bh: Blackhole): Unit =
    var i = 0
    while i < liD.length do
      liD(i) match { case x :: y :: rest => bh.consume(x); bh.consume(y); bh.consume(rest); case _ => () }
      i += 1
  @Benchmark def destructure2_vector(bh: Blackhole): Unit =
    var i = 0
    while i < veD.length do
      veD(i) match { case x +: y +: rest => bh.consume(x); bh.consume(y); bh.consume(rest); case _ => () }
      i += 1

  @Benchmark def destructure3_farray(bh: Blackhole): Unit =
    import farray.`+:`
    var i = 0
    while i < faD.length do
      faD(i) match
        case x +: y +: z +: rest => bh.consume(x); bh.consume(y); bh.consume(z); bh.consume(rest)
        case _                   => ()
      i += 1
  @Benchmark def destructure3_list(bh: Blackhole): Unit =
    var i = 0
    while i < liD.length do
      liD(i) match
        case x :: y :: z :: rest => bh.consume(x); bh.consume(y); bh.consume(z); bh.consume(rest)
        case _                   => ()
      i += 1
  @Benchmark def destructure3_vector(bh: Blackhole): Unit =
    var i = 0
    while i < veD.length do
      veD(i) match
        case x +: y +: z +: rest => bh.consume(x); bh.consume(y); bh.consume(z); bh.consume(rest)
        case _                   => ()
      i += 1

  // ============================ D5 worklist parity (listealization-plan §D5) ==================================
  // Build a sequence by consing items one-by-one via `+:` onto an accumulator, then fully drain it via
  // `case h +: t`. On FArray this is a RefPrepend cons chain (O(1), length-carrying cons cell); draining a
  // Prepend via `h +: t` sets `t = base` with NO allocation (round-8 spine peeling) — the exact List
  // cons/uncons shape. This is the D5 verification: if FArray reaches List parity here, the destructure gap
  // is EXCLUSIVELY the flat-scrutinee/SliceNode case (confirming D2b's Phase-2 scope). If it does NOT, the
  // gap diagnosis (extractor view / isEmpty / length maintenance) changes D2b's design inputs.
  @Benchmark def worklist_farray(bh: Blackhole): Unit =
    import farray.`+:`
    var acc: FArray[String] = FArray.empty[String]
    var i = 0
    while i < work.length do { acc = work(i) +: acc; i += 1 }
    var sum = 0
    var cur = acc
    while cur.nonEmpty do cur match { case h +: t => sum += h.length; cur = t; case _ => cur = FArray.empty[String] }
    bh.consume(sum)
  @Benchmark def worklist_list(bh: Blackhole): Unit =
    var acc: List[String] = Nil
    var i = 0
    while i < work.length do { acc = work(i) :: acc; i += 1 }
    var sum = 0
    var cur = acc
    while cur.nonEmpty do cur match { case h :: t => sum += h.length; cur = t; case _ => cur = Nil }
    bh.consume(sum)
  // Vector drains via head/tail (its native uncons): the `+:` extractor is ambiguous here because this file
  // is `package farray`, so farray's `+:` is visible unqualified — head/tail is Vector's equivalent cons drain.
  @Benchmark def worklist_vector(bh: Blackhole): Unit =
    var acc: Vector[String] = Vector.empty
    var i = 0
    while i < work.length do { acc = work(i) +: acc; i += 1 }
    var sum = 0
    var cur = acc
    while cur.nonEmpty do { sum += cur.head.length; cur = cur.tail }
    bh.consume(sum)
