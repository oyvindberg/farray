package farray

import org.junit.Test
import org.junit.Assert.*

/** `foldAdjacentBy` — streaming group-aggregate for key-clustered input. Parity vs a hand-written adjacent fold over `List` (NOT `groupBy` — the contract is
  * per-RUN, honoring order). Covers: multiple runs, the epilogue flushing the final run, empty/single-run, short-circuit under `take`, runs spanning chunk
  * boundaries, and downstream composition (the (K,B) pair never materialized for `.map(_._2)`).
  */
class FoldAdjacentTest:

  /** the reference: fold each maximal run of equal keys, left to right. */
  private def ref[A, K, B](xs: List[A])(key: A => K)(seed: B)(combine: (B, A) => B): List[(K, B)] =
    xs.foldLeft(List.empty[(K, B)]) { (out, a) =>
      val k = key(a)
      out match
        case (k0, acc) :: t if k0 == k => (k0, combine(acc, a)) :: t
        case _                         => (k, combine(seed, a)) :: out
    }.reverse

  /** a multi-chunk source recording pulls (for short-circuit assertions and cross-chunk-boundary runs). */
  final class CountingSource[A](chunks: List[FArray[A]]) extends Source[A]:
    private var rest = chunks
    var pulls = 0
    def pullChunk(): FArray[A] | Source.End =
      pulls += 1
      rest match
        case h :: t => rest = t; h
        case Nil    => Source.End

  // ── in-memory parity ────────────────────────────────────────────────────────────────────────────────────
  @Test def parity_sumPerRun(): Unit =
    val xs = List(0, 1, 2, 10, 11, 20, 21, 22, 30) // keyed by /10 → runs [0,1,2],[10,11],[20,21,22],[30]
    assertEquals(
      ref(xs)(_ / 10)(0)(_ + _),
      FArray.fromIterable(xs).fuse.foldAdjacentBy(_ / 10)(0)(_ + _).run.toList
    )

  @Test def parity_countPerRun(): Unit =
    val xs = List(5, 5, 5, 9, 9, 1, 7, 7)
    assertEquals(
      ref(xs)(identity)(0)((c, _) => c + 1),
      FArray.fromIterable(xs).fuse.foldAdjacentBy(identity)(0)((c, _) => c + 1).run.toList
    )

  @Test def empty(): Unit =
    assertEquals(Nil, FArray.empty[Int].fuse.foldAdjacentBy(_ / 10)(0)(_ + _).run.toList)

  @Test def singleRun_comesFromEpilogue(): Unit =
    // all one key → no in-loop key-change ever fires; the sole output is the epilogue flush.
    val xs = List(7, 7, 7, 7)
    assertEquals(List((0, 28)), FArray.fromIterable(xs).fuse.foldAdjacentBy(_ / 10)(0)(_ + _).run.toList)

  @Test def lastRunFlushed(): Unit =
    // ends on a fresh final run [2] → 3 outputs, the last only via the epilogue.
    val xs = List(0, 0, 1, 1, 2)
    assertEquals(List((0, 0), (1, 2), (2, 2)), FArray.fromIterable(xs).fuse.foldAdjacentBy(identity)(0)(_ + _).run.toList)

  // ── streaming parity, incl. runs spanning chunk boundaries ──────────────────────────────────────────────
  @Test def streaming_parity_runsAcrossChunks(): Unit =
    // a run [10,11,12] is SPLIT across two chunks → proves cross-chunk state persistence.
    val chunks = List(List(0, 1), List(10, 11), List(12, 20), List(21, 30, 31))
    val flat = chunks.flatten
    val src = new CountingSource(chunks.map(FArray.fromIterable))
    assertEquals(ref(flat)(_ / 10)(0)(_ + _), src.fuse.foldAdjacentBy(_ / 10)(0)(_ + _).run.toList)

  @Test def streaming_foldTerminal_isO1(): Unit =
    // a folding terminal over the runs — no output buffer, constant memory.
    val chunks = List(List(0, 1, 2), List(2, 3), List(10, 11))
    val flat = chunks.flatten
    val src = new CountingSource(chunks.map(FArray.fromIterable))
    val refSum = ref(flat)(_ / 10)(0)(_ + _).map(_._2).sum
    assertEquals(refSum, src.fuse.foldAdjacentBy(_ / 10)(0)(_ + _).foldLeft(0)((s, kv) => s + kv._2))

  // ── short-circuit: take(n) yields exactly the first n runs ──────────────────────────────────────────────
  @Test def take_yieldsFirstNRuns(): Unit =
    val xs = List(0, 0, 1, 1, 1, 2, 3, 3) // runs: (0,0),(1,3),(2,2),(3,6)
    val first2 = FArray.fromIterable(xs).fuse.foldAdjacentBy(identity)(0)(_ + _).take(2).run.toList
    assertEquals(List((0, 0), (1, 3)), first2)

  @Test def take_allRuns_inclPendingViaEpilogue(): Unit =
    // take(n) == total runs: the LAST run is the pending one (only the epilogue emits it) → must still be admitted.
    val xs = List(0, 0, 1, 1, 2) // 3 runs
    val all = FArray.fromIterable(xs).fuse.foldAdjacentBy(identity)(0)(_ + _).take(3).run.toList
    assertEquals(List((0, 0), (1, 2), (2, 2)), all)

  @Test def take_streaming_stopsPulling(): Unit =
    // take(2) over a multi-chunk source: once 2 runs are formed, stop pulling further chunks.
    val chunks = List(List(0, 0), List(1, 1), List(2, 2), List(3, 3))
    val src = new CountingSource(chunks.map(FArray.fromIterable))
    val got = src.fuse.foldAdjacentBy(identity)(0)(_ + _).take(2).run.toList
    assertEquals(List((0, 0), (1, 2)), got)
    assertTrue(s"take(2) pulled ${src.pulls} chunks — should stop early", src.pulls <= 3)

  // ── downstream composition: the (K,B) pair is decomposed (DCE) ──────────────────────────────────────────
  @Test def map_projectAcc_noTuple(): Unit =
    val xs = List(0, 1, 10, 11, 12, 20)
    assertEquals(
      ref(xs)(_ / 10)(0)(_ + _).map(_._2),
      FArray.fromIterable(xs).fuse.foldAdjacentBy(_ / 10)(0)(_ + _).map(_._2).run.toList
    )

  @Test def filter_onRunResult(): Unit =
    val xs = List(0, 1, 2, 10, 20, 21, 22)
    assertEquals(
      ref(xs)(_ / 10)(0)(_ + _).filter(_._2 > 3),
      FArray.fromIterable(xs).fuse.foldAdjacentBy(_ / 10)(0)(_ + _).filter(_._2 > 3).run.toList
    )

  // ── reference keys/values ───────────────────────────────────────────────────────────────────────────────
  @Test def stringKeys(): Unit =
    val xs = List("apple", "avocado", "banana", "blueberry", "cherry")
    assertEquals(
      ref(xs)(_.head.toString)("")(_ + _),
      FArray.fromIterable(xs).fuse.foldAdjacentBy(_.head.toString)("")(_ + _).run.toList
    )

end FoldAdjacentTest
