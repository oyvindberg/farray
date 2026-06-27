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

end JsonFuseTest
