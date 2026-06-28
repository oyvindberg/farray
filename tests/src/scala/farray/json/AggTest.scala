package farray.json

import org.junit.Test
import org.junit.Assert.*
import farray.{FArray, Agg}

/** `agg` multi-aggregate terminal: several aggregations in ONE fused pass, columns merged automatically. */
class AggTest:
  import AggTest.Rec

  private val xs = FArray(Rec(1, 10, 100.0), Rec(2, 20, 200.0), Rec(3, 30, 300.0), Rec(4, 40, 400.0))

  private val recs = List(Rec(1, 10, 100.0), Rec(2, 20, 200.0), Rec(3, 30, 300.0), Rec(4, 40, 400.0))

  /** sum/count/max in one pass match the separate computations. */
  @Test def threeAggs_match_separate(): Unit =
    val (sumA, n, maxB) = xs.fuse.agg(Agg.sum(_.a), Agg.count, Agg.max(_.b))
    assertEquals(recs.map(_.a).sum, sumA)
    assertEquals(recs.length, n)
    assertEquals(Some(recs.map(_.b).max), maxB)

  /** filter applies to all aggregates (shared survivor set). */
  @Test def withFilter_sharedSurvivors(): Unit =
    val (s, c) = xs.fuse.filter(_.a > 2).agg(Agg.sum(_.a), Agg.count)
    assertEquals(3 + 4, s)
    assertEquals(2, c)

  /** a field NO aggregate reads is never computed — DCE through agg. Proven by a call-count on a dead field. */
  @Test def deadField_notComputed(): Unit =
    var deadCalls = 0
    def dead(x: Int): Int = { deadCalls += 1; x * 99 }
    val (s, n) = FArray(1, 2, 3, 4).fuse.map(x => Rec(x, dead(x), 0.0)).agg(Agg.sum(_.a), Agg.count)
    assertEquals(1 + 2 + 3 + 4, s)
    assertEquals(4, n)
    assertEquals(0, deadCalls) // `b`/`dead` is dead — the union of read fields is {a}

  /** a field read by SEVERAL aggregates is computed once (CSE) — checked via a call-count. */
  @Test def sharedField_computedOnce(): Unit =
    var calls = 0
    def f(r: Rec): Int = { calls += 1; r.a }
    // both aggregates read f(r); CSE binds it once per element.
    val (s, m) = xs.fuse.map(r => Rec(f(r), r.b, r.c)).agg(Agg.sum(_.a), Agg.max(_.a))
    assertEquals(1 + 2 + 3 + 4, s)
    assertEquals(Some(4), m)
    assertEquals(recs.length, calls) // f called once per element, not twice

  /** min / avg / fold variants. */
  @Test def min_avg_fold(): Unit =
    val (mn, av) = xs.fuse.agg(Agg.min(_.a), Agg.avg(_.c))
    assertEquals(Some(1), mn)
    assertEquals((100.0 + 200.0 + 300.0 + 400.0) / 4, av, 1e-9)
    val (sumViaFold, cnt) = xs.fuse.agg(Agg.fold(0)((acc, r) => acc + r.a), Agg.count)
    assertEquals(10, sumViaFold)
    assertEquals(4, cnt)

  /** empty input: count 0, sum zero, min/max None, avg 0. */
  @Test def empty(): Unit =
    val (s, n, mx) = FArray.empty[Rec].fuse.agg(Agg.sum(_.a), Agg.count, Agg.max(_.b))
    assertEquals(0, s); assertEquals(0, n); assertEquals(None, mx)

  /** four aggregates (max arity). */
  @Test def fourAggs(): Unit =
    val (s, n, mn, mx) = xs.fuse.agg(Agg.sum(_.a), Agg.count, Agg.min(_.b), Agg.max(_.b))
    assertEquals(10, s); assertEquals(4, n); assertEquals(Some(10), mn); assertEquals(Some(40), mx)

end AggTest

object AggTest:
  final case class Rec(a: Int, b: Int, c: Double)
