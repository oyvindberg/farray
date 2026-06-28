package farray.json

import org.junit.Test
import org.junit.Assert.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

/** The MACRO-DRIVEN path: `Json.ndjson[Rec](buf).fuse.filter(...).map(...).<terminal>` must produce the same
 *  answers as the hand-written scanner and jsoniter — proving the EXISTING fuse optimizer (filter→if, map,
 *  sink, DCE, terminals) now genuinely drives byte-sourced parsing. */
class JsonFuseTest:

  given recCodec: JsonValueCodec[Rec] = JsonCodecMaker.make

  private val N = 5000
  private val threshold = 150.0
  private val buf = JsonGen.ndjson(N)

  private def jsoniterRecs: Vector[Rec] =
    val out = Vector.newBuilder[Rec]; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      out += readFromSubArray[Rec](buf, start, end); start = end + 1
    out.result()

  /** aggregate: filter(amount > t).map(amount).sum — should fuse to a zero-alloc scan, fold into a var. */
  @Test def fused_sum_matches_jsoniter(): Unit =
    val ref = jsoniterRecs.filter(_.amount > threshold).map(_.amount).sum
    val got = Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(_.amount).sum
    assertEquals(ref, got, 1e-6)

  /** projection + lazy string: filter(amount > t).map(category).toList. */
  @Test def fused_category_matches_jsoniter(): Unit =
    val ref = jsoniterRecs.filter(_.amount > threshold).map(_.category).toList
    val got = Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(_.category).toList
    assertEquals(ref, got)

  /** count of survivors. */
  @Test def fused_count_matches_jsoniter(): Unit =
    val ref = jsoniterRecs.count(_.amount > threshold)
    val got = Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).count
    assertEquals(ref, got)

  /** multiple fields + compute-for-survivors: filter on one numeric, sum of others (decoded only for survivors). */
  @Test def fused_multifield_matches_jsoniter(): Unit =
    val ref = jsoniterRecs.filter(_.amount > threshold).map(r => r.age + r.count).sum
    val got = Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(r => r.age + r.count).sum
    assertEquals(ref, got)

  /** a String predicate + a numeric projection. */
  @Test def fused_stringfilter_matches_jsoniter(): Unit =
    val ref = jsoniterRecs.filter(_.status == "active").map(_.amount).sum
    val got = Json.ndjson[Rec](buf).fuse.filter(_.status == "active").map(_.amount).sum
    assertEquals(ref, got, 1e-6)

  /** DCE-to-terminal: `map(_.category).count` must NOT decode the category strings (the mapped value is dead
   *  for count). Correctness: the count must still match; the no-decode is verified by the benchmark's 0 B/op. */
  @Test def fused_mapCount_dce_correct(): Unit =
    val ref = jsoniterRecs.count(_.amount > threshold)
    val got = Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(_.category).count
    assertEquals(ref, got)

  /** the general (in-memory) form: `map(expensive).count` must not call `expensive` — DCE to the count
   *  terminal. A call-count proves the mapped function never runs. */
  @Test def inMemory_mapCount_skips_map(): Unit =
    var calls = 0
    val n = farray.FArray(1, 2, 3, 4, 5, 6).fuse.filter(_ % 2 == 0).map(x => { calls += 1; x * 1000 }).count
    assertEquals(3, n)       // 3 even numbers survive
    assertEquals(0, calls)   // but the map function was never invoked — its result is dead for count

  /** fold-op decomposition: `foldLeft((acc, r) => acc + r.field)` reads only the projected column — it does NOT
   *  rebuild the product or compute the other (dead) fields. A call-count on a dead field proves it. */
  @Test def inMemory_foldOp_decomposes_product(): Unit =
    var deadCalls = 0
    def dead(x: Int): Int = { deadCalls += 1; x * 99 } // a field nobody reads in the fold
    val sumA = farray.FArray(1, 2, 3, 4)
      .fuse.map(x => JsonFuseTest.Pair(x, dead(x)))     // build a product whose `b` calls `dead`
      .foldLeft(0)((acc, p) => acc + p.a)                // fold reads ONLY `a`
    assertEquals(1 + 2 + 3 + 4, sumA)
    assertEquals(0, deadCalls)                           // `b`/`dead` never computed — dead column eliminated

  /** but a WHOLE-record use in the fold op (a method call) must still materialize and run correctly. */
  @Test def inMemory_foldOp_wholeRecord_rebuilds_correctly(): Unit =
    val got = farray.FArray(1, 2, 3)
      .fuse.map(x => JsonFuseTest.Pair(x, x * 10))
      .foldLeft(0)((acc, r) => acc + r.combined)         // .combined is a method → whole record needed
    val ref = List(1, 2, 3).map(x => JsonFuseTest.Pair(x, x * 10)).foldLeft(0)((acc, r) => acc + r.combined)
    assertEquals(ref, got)

  /** discard-DCE extended to nonEmpty/isEmpty (which desugar to exists(_ => true)): `map(f).nonEmpty` and
   *  `map(f).isEmpty` ignore the mapped value, so `f` is dead and must not run. */
  @Test def inMemory_mapNonEmpty_skips_map(): Unit =
    var calls = 0
    val ne = farray.FArray(1, 2, 3, 4).fuse.filter(_ > 2).map(x => { calls += 1; x * 10 }).nonEmpty
    assertTrue(ne)           // 3,4 survive → nonEmpty
    assertEquals(0, calls)   // the map was never run

  @Test def inMemory_mapIsEmpty_skips_map(): Unit =
    var calls = 0
    val empty = farray.FArray(1, 2, 3).fuse.filter(_ > 100).map(x => { calls += 1; x * 10 }).isEmpty
    assertTrue(empty)        // nothing > 100 → isEmpty
    assertEquals(0, calls)

  /** exists with a value-IGNORING predicate discards too; exists with a value-READING predicate does NOT. */
  @Test def inMemory_existsConst_skips_map_but_existsPred_does_not(): Unit =
    var c1 = 0
    val any = farray.FArray(1, 2, 3).fuse.map(x => { c1 += 1; x * 10 }).exists(_ => true)
    assertTrue(any); assertEquals(0, c1) // predicate ignores arg → map dead
    var c2 = 0
    val hit = farray.FArray(1, 2, 3).fuse.map(x => { c2 += 1; x * 10 }).exists(_ > 25)
    assertTrue(hit); assertEquals(3, c2) // predicate reads the mapped value → map MUST run

  /** JSON: `map(_.category).nonEmpty` must not decode any category strings. */
  @Test def fused_mapNonEmpty_matches_jsoniter(): Unit =
    val ref = jsoniterRecs.exists(_.amount > threshold)
    val got = Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(_.category).nonEmpty
    assertEquals(ref, got)

  /** terminal lambdas read fields directly on a JSON source WITHOUT a leading map/filter — those fields must
   *  be scanned, not defaulted (regression: `foldLeft((a,r) => a + r.amount)` over JSON gave `a + 0.0`). */
  @Test def fused_noMapTerminal_readsFields(): Unit =
    val ref = jsoniterRecs.map(_.amount).sum
    // foldLeft directly on the source (no map/filter to pre-mark `amount` live)
    val fold = Json.ndjson[Rec](buf).fuse.foldLeft(0.0)((a, r) => a + r.amount)
    assertEquals(ref, fold, 1e-6)
    // foreach directly on the source
    var fe = 0.0
    Json.ndjson[Rec](buf).fuse.foreach(r => fe += r.amount)
    assertEquals(ref, fe, 1e-6)
    // agg directly on the source
    val (s, n) = Json.ndjson[Rec](buf).fuse.agg(farray.Agg.sum(_.amount), farray.Agg.count)
    assertEquals(ref, s, 1e-6)
    assertEquals(jsoniterRecs.length, n)
    // find directly on the source
    val found = Json.ndjson[Rec](buf).fuse.find(_.amount > 290)
    assertEquals(jsoniterRecs.find(_.amount > 290).map(_.amount), found.map(_.amount))

  /** predicate-fail early-out correctness: a selective filter then a projection of a different field. The
   *  early-out must abandon rejected records without affecting the result (boundary cases: all-reject, all-pass).
   *  Cross-checked vs jsoniter. */
  @Test def fused_predicateEarlyOut_correct(): Unit =
    val ref = jsoniterRecs.filter(_.age > 80).map(_.name).toList
    val got = Json.ndjson[Rec](buf).fuse.filter(_.age > 80).map(_.name).toList
    assertEquals(ref, got)
    val none = Json.ndjson[Rec](buf).fuse.filter(_.age > 100000).map(_.name).toList
    assertEquals(Nil, none) // all rejected → empty, must not crash on the skipped projection
    val allRef = jsoniterRecs.map(_.name).toList
    val all = Json.ndjson[Rec](buf).fuse.filter(_.age > -1).map(_.name).toList
    assertEquals(allRef, all) // all pass → project everything

  /** string predicate early-out (the inline check decodes the predicate string): filter(status==..).map(name). */
  @Test def fused_stringPredicateEarlyOut_correct(): Unit =
    val ref = jsoniterRecs.filter(_.status == "active").map(_.name).toList
    val got = Json.ndjson[Rec](buf).fuse.filter(_.status == "active").map(_.name).toList
    assertEquals(ref, got)

end JsonFuseTest

object JsonFuseTest:
  /** a top-level product for the fold-decomposition tests: `a`/`b` are fields, `combined` is a method that
   *  forces the whole record. (Must be top-level — a case class inside a lambda isn't supported by fuse.) */
  final case class Pair(a: Int, b: Int):
    def combined: Int = a + b
