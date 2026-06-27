package farray

import org.junit.Test
import org.junit.Assert.*

/** S1 acceptance: the chunk driver carries the existing fused surface over an incremental `Source` — same answers as the eager `FArray`, constant memory, and
  * `done` gating the next pull (so `take` reads only what it needs).
  */
class SourceTest:

  /** a multi-chunk source that records how many times it was pulled — proves the OUTER chunk loop and the pull-count short-circuit. Each chunk is a separate
    * FArray leaf.
    */
  final class CountingSource[A](chunks: List[FArray[A]]) extends Source[A]:
    private var rest = chunks
    var pulls = 0
    def pullChunk(): FArray[A] | Source.End =
      pulls += 1
      rest match
        case h :: t => rest = t; h
        case Nil    => Source.End

  private def intChunks(cs: List[List[Int]]) = new CountingSource(cs.map(FArray.fromIterable))

  // ── parity: a streamed source == the eager FArray over the full stage/terminal surface ──────────────────
  @Test def parity_intStages(): Unit =
    val data = List(List(-3, 4, 7), List(2, -1, 8, 5), List(0, 6), List(9, -2))
    val flat = data.flatten
    def src = intChunks(data)
    // terminals over a multi-stage pipeline
    assertEquals(flat.filter(_ > 0).map(_ * 2).sum, src.fuse.filter(_ > 0).map(_ * 2).sum)
    assertEquals(flat.filter(_ % 2 == 0).foldLeft(100)(_ + _), src.fuse.filter(_ % 2 == 0).foldLeft(100)(_ + _))
    assertEquals(flat.count(_ > 3), src.fuse.count(_ > 3))
    assertEquals(flat.map(_ + 1).toList, src.fuse.map(_ + 1).run.toList)
    assertEquals(flat.filter(_ > 0).map(_ * 3).toList, src.fuse.filter(_ > 0).map(_ * 3).run.toList)
    assertEquals(flat.maxOption, src.fuse.maxOption)
    assertEquals(flat.minByOption(x => -x), src.fuse.minByOption(x => -x))

  @Test def parity_takeDrop(): Unit =
    val data = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10))
    val flat = data.flatten
    assertEquals(flat.take(5), intChunks(data).fuse.take(5).run.toList)
    assertEquals(flat.drop(4), intChunks(data).fuse.drop(4).run.toList)
    assertEquals(flat.drop(2).take(4), intChunks(data).fuse.drop(2).take(4).run.toList)
    assertEquals(flat.filter(_ % 2 == 0).take(3), intChunks(data).fuse.filter(_ % 2 == 0).take(3).run.toList)

  @Test def parity_shortCircuit(): Unit =
    val data = List(List(1, 2), List(3, 4), List(5, 6))
    val flat = data.flatten
    assertEquals(flat.find(_ > 3), intChunks(data).fuse.find(_ > 3))
    assertEquals(flat.exists(_ == 5), intChunks(data).fuse.exists(_ == 5))
    assertEquals(flat.forall(_ < 100), intChunks(data).fuse.forall(_ < 100))
    assertEquals(flat.headOption, intChunks(data).fuse.headOption)

  @Test def parity_refKind(): Unit =
    val data = List(List("a", "bb"), List("ccc", "d"), List("ee"))
    val flat = data.flatten
    assertEquals(
      flat.filter(_.length <= 2).map(_.toUpperCase).toList,
      new CountingSource(data.map(FArray.fromIterable)).fuse.filter(_.length <= 2).map(_.toUpperCase).run.toList
    )

  @Test def empty_and_singleChunk(): Unit =
    assertEquals(0, intChunks(Nil).fuse.sum)
    assertEquals(Nil, intChunks(Nil).fuse.map(_ + 1).run.toList)
    assertEquals(List(2, 3, 4), intChunks(List(List(1, 2, 3))).fuse.map(_ + 1).run.toList)
    // a source whose chunks include an empty leaf in the middle
    assertEquals(List(1, 2, 3), intChunks(List(List(1), Nil, List(2, 3))).fuse.run.toList)

  // ── the constant-memory / short-circuit guarantee: `done` gates the next pull ───────────────────────────
  /** `take(n)` over a multi-chunk source must stop pulling once it has n — it must NOT drain every chunk. take(2) is satisfied within the first chunk → at most
    * 2 pulls (the first chunk + the end check it doesn't even reach, since done fires mid-chunk).
    */
  @Test def take_doesNotDrainAllChunks(): Unit =
    val s = intChunks(List(List(1, 2, 3, 4), List(5, 6), List(7, 8), List(9, 10)))
    val got = s.fuse.take(2).run.toList
    assertEquals(List(1, 2), got)
    // take(2) is satisfied inside chunk 1 → exactly ONE pull (the outer loop's `&& !done` blocks the 2nd pull).
    assertEquals("take(2) must pull only the first chunk", 1, s.pulls)

  /** find short-circuits across chunks: the element is in chunk 2 → exactly 2 pulls, not 4. */
  @Test def find_stopsAtMatchingChunk(): Unit =
    val s = intChunks(List(List(1, 2), List(3, 4), List(5, 6), List(7, 8)))
    assertEquals(Some(4), s.fuse.find(_ == 4))
    assertEquals("find(==4) must pull chunks 1 and 2 only", 2, s.pulls)

  /** a non-short-circuit terminal (sum) drains everything: pulls = chunks + 1 (the final End pull). */
  @Test def sum_drainsAllChunksPlusEnd(): Unit =
    val s = intChunks(List(List(1, 2), List(3, 4), List(5, 6)))
    assertEquals(21, s.fuse.sum)
    assertEquals("sum pulls every chunk + the End", 4, s.pulls)

  // ── constant memory: fold a huge stream (many chunks) without retaining it ──────────────────────────────
  /** a generated source of N chunks of 10k ints each — sum folds in O(chunk), retaining no chunk. The source itself allocates one leaf at a time; a leak would
    * be O(N·chunk).
    */
  @Test def constantMemory_hugeStream(): Unit =
    val nChunks = 2000; val chunkSize = 10000
    val src = new Source[Long]:
      private var i = 0
      def pullChunk(): FArray[Long] | Source.End =
        if i >= nChunks then Source.End
        else { val base = i.toLong * chunkSize; i += 1; FArray.tabulate(chunkSize)(j => base + j) }
    val total = src.fuse.filter(_ % 2L == 0L).map(_ * 2L).sum
    // value sanity: sum over 20M elements of the doubled evens
    var ref = 0L; var k = 0L; val n = nChunks.toLong * chunkSize
    while k < n do { if k % 2L == 0L then ref += k * 2L; k += 1 }
    assertEquals(ref, total)
    // RETENTION check (immune to transient fold churn): after the stream is fully consumed and GC'd, LIVE memory
    // must be tiny — the source held one 10k-long chunk (~80 KB) at a time. Retaining all 20M longs would be
    // ~160 MB of live data surviving the GC. Measure live (not an alloc delta around the fold, which is flaky).
    val rt = Runtime.getRuntime
    System.gc(); Thread.sleep(30); System.gc(); Thread.sleep(30)
    val live = rt.totalMemory - rt.freeMemory
    assertTrue(s"live memory ${live / 1000000} MB — the stream may be retaining chunks (should be O(1 chunk))", live < 80_000_000L)

  // ── the source constructors (sugar over the chunk driver) ───────────────────────────────────────────────
  @Test def fromIterator_parity(): Unit =
    val xs = (1 to 100).toList
    // tiny chunk size to force MANY chunks through the driver
    assertEquals(xs.filter(_ % 3 == 0).map(_ * 2).sum, Source.fromIterator(xs.iterator, chunkSize = 7).fuse.filter(_ % 3 == 0).map(_ * 2).sum)
    assertEquals(xs.filter(_ > 50).take(5), Source.fromIterable(xs, chunkSize = 4).fuse.filter(_ > 50).take(5).run.toList)
    assertEquals(Nil, Source.fromIterator(Iterator.empty[Int]).fuse.map(_ + 1).run.toList)

  @Test def unfoldChunk_and_unfold(): Unit =
    // unfoldChunk: emit [0,1,2],[3,4,5],… up to 9
    val s = Source.unfoldChunk(0) { i => if i >= 9 then None else Some((FArray(i, i + 1, i + 2), i + 3)) }
    assertEquals((0 to 8).toList, s.fuse.run.toList)
    // unfold (element-at-a-time, chunked): the first 20 squares
    val sq = Source.unfold(1, chunkSize = 6)(n => Some((n * n, n + 1)))
    assertEquals((1 to 20).map(n => n * n).toList, sq.fuse.take(20).run.toList)

  @Test def iterate_repeat_range_bounded(): Unit =
    // iterate is UNBOUNDED → take bounds it (and must not loop forever)
    assertEquals(List(1, 2, 4, 8, 16, 32), Source.iterate(1)(_ * 2).fuse.take(6).run.toList)
    assertEquals(List(7, 7, 7, 7), Source.repeat(7).fuse.take(4).run.toList)
    assertEquals((0 until 25).toList, Source.range(0, 25, chunkSize = 8).fuse.run.toList)
    assertEquals((10 to 1 by -1).toList, Source.range(10, 0, step = -1).fuse.run.toList)
    // takeWhile over an unbounded source
    assertEquals(List(1, 2, 4, 8), Source.iterate(1)(_ * 2).fuse.takeWhile(_ < 10).run.toList)

  /** an UNBOUNDED source folded under `take` is constant memory and terminates — the headline streaming case. */
  @Test def unbounded_take_terminates(): Unit =
    val n = 5_000_000
    val sum = Source.iterate(0L)(_ + 1L).fuse.take(n).filter(_ % 2L == 0L).sum
    var ref = 0L; var k = 0L; while k < n do { if k % 2L == 0L then ref += k; k += 1 }
    assertEquals(ref, sum)

end SourceTest
