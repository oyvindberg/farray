package farray

import org.junit.Test
import org.junit.Assert.*

/** `groupAdjacentReduceBy` — NESTED FUSION. For key-clustered input, reduce each maximal run with a FUSED inner sub-pipeline (prep stages + a folding
  * aggregate), emitting `(k, result)` per run with the run's rows NEVER materialized. Parity vs a hand-written adjacent-grouping over `List` followed by the
  * equivalent reduce (per-RUN, order-honoring — NOT `groupBy`). Covers: sum/count/min/max/avg/fold/minBy/reduce, inner map/filter/collect stages, the epilogue
  * flushing the final run, empty/single-run, short-circuit under `take`, runs across chunk boundaries, the (K,R) pair never materialized for `.map(_._2)`, and
  * the never-materialized (call-count) proof.
  */
class GroupReduceAdjacentTest:

  /** the reference: split into maximal runs of equal key (order-preserving), as a List of (key, rows). */
  private def runs[A, K](xs: List[A])(key: A => K): List[(K, List[A])] =
    xs.foldLeft(List.empty[(K, List[A])]) { (out, a) =>
      val k = key(a)
      out match
        case (k0, rows) :: t if k0 == k => (k0, rows :+ a) :: t
        case _                          => (k, List(a)) :: out
    }.reverse

  final class CountingSource[A](chunks: List[FArray[A]]) extends Source[A]:
    private var rest = chunks
    var pulls = 0
    def pullChunk(): FArray[A] | Source.End =
      pulls += 1
      rest match
        case h :: t => rest = t; h
        case Nil    => Source.End

  // ── in-memory parity, the folding aggregates ────────────────────────────────────────────────────────────
  @Test def sumPerRun(): Unit =
    val xs = List(0, 1, 2, 10, 11, 20, 21, 22, 30) // keyed by /10 → runs [0,1,2],[10,11],[20,21,22],[30]
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.sum)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).run.toList
    )

  @Test def sumOfMappedField(): Unit =
    // the inner map projects a field; the agg sums it — the run's rows never materialized.
    val xs = List(0, 1, 2, 10, 11, 20)
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.map(_ * 3).sum)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(_ * 3))(Agg.sum(identity)).run.toList
    )

  @Test def countPerRun(): Unit =
    val xs = List(5, 5, 5, 9, 9, 1, 7, 7)
    assertEquals(
      runs(xs)(identity).map((k, rows) => (k, rows.size)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(identity)(_.map(identity))(Agg.count).run.toList
    )

  @Test def countWithInnerFilter(): Unit =
    // count only the even rows per run — inner filter, then count.
    val xs = List(1, 2, 4, 10, 11, 12, 13, 20)
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.count(_ % 2 == 0))),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.filter(_ % 2 == 0))(Agg.count).run.toList
    )

  @Test def minMaxPerRun(): Unit =
    val xs = List(3, 1, 2, 10, 19, 11, 25)
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.min)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.min(identity)).run.toList.map((k, o) => (k, o.get))
    )
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.max)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.max(identity)).run.toList.map((k, o) => (k, o.get))
    )

  @Test def foldPerRun(): Unit =
    val xs = List(1, 2, 3, 10, 20, 21)
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.foldLeft(100)(_ + _))),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.fold(100)(_ + _)).run.toList
    )

  @Test def avgPerRun(): Unit =
    val xs = List(2, 4, 6, 10, 30)
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.sum.toDouble / rows.size)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.avg(_.toDouble)).run.toList
    )

  @Test def reducePerRun(): Unit =
    val xs = List(1, 2, 3, 10, 11)
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.reduce(_ * _))),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.reduce[Int, Int](_ * _)).run.toList.map((k, o) => (k, o.get))
    )

  @Test def minByPerRun(): Unit =
    // best ELEMENT by key — retains the winning element.
    val xs = List("bb", "a", "ccc", "zzzz", "y")
    assertEquals(
      runs(xs)(_.length / 2).map((k, rows) => (k, rows.minBy(_.length))),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_.length / 2)(_.map(identity))(Agg.minBy(_.length)).run.toList.map((k, o) => (k, o.get))
    )

  // ── edge cases ──────────────────────────────────────────────────────────────────────────────────────────
  @Test def empty(): Unit =
    assertEquals(Nil, FArray.empty[Int].fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).run.toList)

  @Test def singleRun_fromEpilogue(): Unit =
    val xs = List(7, 7, 7, 7) // all one key → sole output via the epilogue flush
    assertEquals(List((0, 28)), FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).run.toList)

  @Test def lastRunFlushedByEpilogue(): Unit =
    val xs = List(0, 0, 1, 1, 2) // ends on a fresh final run [2]
    assertEquals(
      List((0, 0), (1, 2), (2, 2)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(identity)(_.map(identity))(Agg.sum(identity)).run.toList
    )

  // ── streaming, runs spanning chunk boundaries ───────────────────────────────────────────────────────────
  @Test def streaming_runsAcrossChunks(): Unit =
    // a run [10,11,12] is split across two chunks — the per-run accumulator must survive pullChunk().
    val xs = List(1, 1, 10, 11, 12, 12, 20)
    val src = CountingSource(List(FArray(1, 1, 10), FArray(11, 12), FArray(12, 20)))
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.sum)),
      src.fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).run.toList
    )

  // ── short-circuit under a downstream take ───────────────────────────────────────────────────────────────
  @Test def take_stopsEarly_inclEpilogue(): Unit =
    val xs = List(0, 1, 10, 11, 20, 30, 40) // runs by /10: [0,1],[10,11],[20],[30],[40]
    assertEquals(
      runs(xs)(_ / 10).map((k, rows) => (k, rows.sum)).take(2),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).take(2).run.toList
    )

  @Test def take_streaming_stopsPulling(): Unit =
    // taking 2 runs from chunk 1 must not pull chunk 3.
    val src = CountingSource(List(FArray(0, 0, 1, 1, 2), FArray(2, 3), FArray(4, 4)))
    val out = src.fuse.groupAdjacentReduceBy(_ / 1)(_.map(identity))(Agg.count).take(2).run.toList
    assertEquals(List((0, 2), (1, 2)), out)
    assertTrue(s"pulled ${src.pulls} chunks", src.pulls <= 2)

  // ── downstream composition: (K,R) never materialized ────────────────────────────────────────────────────
  @Test def mapSecond_noTuple(): Unit =
    val xs = List(0, 1, 2, 10, 11, 20)
    assertEquals(
      runs(xs)(_ / 10).map((_, rows) => rows.sum),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_ / 10)(_.map(identity))(Agg.sum(identity)).map(_._2).run.toList
    )

  // ── the never-materialized proof: each row is visited exactly ONCE, no per-run buffer ───────────────────
  @Test def neverMaterialized_eachRowVisitedOnce(): Unit =
    val xs = List(0, 1, 2, 10, 11, 20, 21, 22)
    var calls = 0
    val total = FArray
      .fromIterable(xs)
      .fuse
      .groupAdjacentReduceBy(_ / 10)(_.map { x => calls += 1; x })(Agg.sum(identity))
      .map(_._2)
      .sum
    assertEquals(xs.size, calls) // each row touched exactly once — the inner map runs inline, rows not buffered+rescanned
    assertEquals(xs.sum, total)

  // ── reference keys ──────────────────────────────────────────────────────────────────────────────────────
  @Test def stringKeys(): Unit =
    val xs = List("apple", "avocado", "banana", "blueberry", "cherry")
    assertEquals(
      runs(xs)(_.head).map((k, rows) => (k, rows.map(_.length).sum)),
      FArray.fromIterable(xs).fuse.groupAdjacentReduceBy(_.head)(_.map(_.length))(Agg.sum(identity)).run.toList
    )
