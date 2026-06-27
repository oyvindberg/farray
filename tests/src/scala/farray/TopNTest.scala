package farray

import org.junit.Test
import org.junit.Assert.*

/** standalone top-N terminals (`topN`/`bottomN`/`topNBy`/`bottomNBy`) + the `Agg.topNBy`/`largest`/`smallest` agg specs: the `n` best elements by a key in ONE
  * fused pass via a bounded size-`n` heap — same answer as `List.sortBy(key).take(n)` (best-first), without a full sort or an O(N) buffer.
  */
class TopNTest:
  import TopNTest.Rec

  private val recs = List(
    Rec(1, 50.0),
    Rec(2, 10.0),
    Rec(3, 90.0),
    Rec(4, 30.0),
    Rec(5, 70.0),
    Rec(6, 20.0),
    Rec(7, 80.0),
    Rec(8, 40.0),
    Rec(9, 60.0),
    Rec(10, 100.0)
  )
  private val xs = FArray.fromIterable(recs)

  /** topNBy(key): the n largest by key, best-first. */
  @Test def topNBy_largest(): Unit =
    val got = xs.fuse.topNBy(3)(_.amount).toList
    val ref = recs.sortBy(-_.amount).take(3)
    assertEquals(ref, got)

  /** bottomNBy(key): the n smallest by key, best-first (smallest first). */
  @Test def bottomNBy_smallest(): Unit =
    val got = xs.fuse.bottomNBy(3)(_.amount).toList
    val ref = recs.sortBy(_.amount).take(3)
    assertEquals(ref, got)

  /** topN / bottomN over a primitive element by natural ordering — returns a FLAT primitive leaf (unboxed). */
  @Test def topN_bottomN_primitive(): Unit =
    val ns = FArray(5, 2, 9, 1, 7, 3, 8, 4, 6, 0)
    assertEquals(List(9, 8, 7), ns.fuse.topN(3).toList)
    assertEquals(List(0, 1, 2), ns.fuse.bottomN(3).toList)

  /** n larger than the input: returns ALL elements, still best-first. */
  @Test def n_exceeds_size(): Unit =
    val got = xs.fuse.topNBy(100)(_.amount).toList
    assertEquals(recs.sortBy(-_.amount), got)

  /** empty input → empty result. */
  @Test def empty(): Unit =
    assertEquals(Nil, FArray.empty[Rec].fuse.topNBy(5)(_.amount).toList)
    assertEquals(Nil, FArray.empty[Int].fuse.topN(5).toList)

  /** composes with upstream stages: filter then topNBy operates on survivors only. */
  @Test def withFilter(): Unit =
    val got = xs.fuse.filter(_.id % 2 == 0).topNBy(2)(_.amount).toList
    val ref = recs.filter(_.id % 2 == 0).sortBy(-_.amount).take(2)
    assertEquals(ref, got)

  /** integrates with `agg`: topNBy as one aggregate alongside sum/count, one fused pass. */
  @Test def insideAgg(): Unit =
    val (top2, total, n) = xs.fuse.agg(Agg.topNBy(2)(_.amount), Agg.sum(_.amount), Agg.count)
    assertEquals(recs.sortBy(-_.amount).take(2), top2.toList)
    assertEquals(recs.map(_.amount).sum, total, 1e-9)
    assertEquals(recs.length, n)

  /** Agg.largest/smallest over a primitive element by natural ordering. */
  @Test def aggLargestSmallest(): Unit =
    val ns = FArray(5, 2, 9, 1, 7)
    val (big, small) = ns.fuse.agg(Agg.largest(2), Agg.smallest(2))
    assertEquals(List(9, 7), big.toList)
    assertEquals(List(1, 2), small.toList)

  /** the heap RETAINS only n elements: top-3 over a large input, repeated, must not hold the whole input. We measure LIVE (post-GC) memory growth — transient
    * per-element boxing is collected; an O(N) retained buffer (a sortBy-the-whole-input implementation) would survive the GC and balloon by hundreds of MB.
    */
  @Test def topN_bounded_memory(): Unit =
    val big = FArray.tabulate(200000)(i => i.toLong)
    var sink = 0L; var iter = 0
    while iter < 50 do { sink += big.fuse.topN(3).apply(0); iter += 1 }
    assertEquals(199999L, sink / 50)
    val rt = Runtime.getRuntime
    System.gc(); Thread.sleep(30); System.gc(); Thread.sleep(30)
    val live = rt.totalMemory - rt.freeMemory
    // the only large live object should be `big` itself (~1.6 MB LongArr). An O(N) retained buffer per pass,
    // or retaining all 50 results, would push live memory far past this.
    assertTrue(s"live memory ${live / 1000000} MB — topN may be retaining O(N) instead of O(n)", live < 80_000_000L)

  /** the PRIMITIVE-KEY path must not box the key per element. With the heap full after the first n records, every later record is rejected by a single key
    * comparison — that comparison runs on a `double[]`/`long[]`, never a boxed Double/Long. A boxing regression would allocate ~N wrappers per pass (hundreds
    * of MB over 50 passes).
    */
  @Test def topNBy_primitiveKey_doesnt_box(): Unit =
    val big = FArray.tabulate(200000)(i => Rec(i.toLong, (i % 997).toDouble))
    val rt = Runtime.getRuntime
    System.gc(); Thread.sleep(20)
    val before = rt.totalMemory - rt.freeMemory
    var sink = 0.0; var iter = 0
    // ascending-ish key churn so the heap is challenged but settles → most offers are rejected by ONE compare.
    while iter < 50 do { sink += big.fuse.topNBy(5)(_.amount).apply(0).amount; iter += 1 }
    val after = rt.totalMemory - rt.freeMemory
    assertTrue(s"sink=$sink", sink > 0.0)
    // unboxed key compare ≈ 0 alloc/element; a boxed-Double compare would be ~10M wrappers → >150 MB. Allow slack.
    assertTrue(s"alloc grew ${(after - before) / 1000000} MB — primitive topN key likely boxing", (after - before) < 80_000_000L)

end TopNTest

object TopNTest:
  final case class Rec(id: Long, amount: Double)
