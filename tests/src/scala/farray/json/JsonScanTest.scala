package farray.json

import org.junit.Test
import org.junit.Assert.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

/** Correctness gate for the v0a hand-written projection scanners. The scanner's answers must match (a) the generator's reference and (b) jsoniter-scala's full
  * parse. If this fails, the thesis benchmark is moot.
  */
class JsonScanTest:

  // jsoniter codec for the full Rec — the independent oracle.
  given recCodec: JsonValueCodec[Rec] = JsonCodecMaker.make

  private val N = 5000
  private val threshold = 150.0
  private val buf = JsonGen.ndjson(N)

  /** reference via jsoniter full parse of every line. */
  private def jsoniterRecs: Vector[Rec] =
    val out = Vector.newBuilder[Rec]
    var start = 0
    while start < buf.length do
      var end = start
      while end < buf.length && buf(end) != '\n' do end += 1
      out += readFromSubArray[Rec](buf, start, end)
      start = end + 1
    out.result()

  @Test def sumAmount_matches_reference(): Unit =
    // reference computed directly from the generator
    var ref = 0.0
    var i = 0
    while i < N do { val a = JsonGen.amountOf(i); if a > threshold then ref += a; i += 1 }
    val got = RecScan.sumAmountWhereGt(buf, 0, buf.length, threshold)
    assertEquals(ref, got, 1e-6)

  @Test def sumAmount_matches_jsoniter(): Unit =
    val recs = jsoniterRecs
    val ref = recs.filter(_.amount > threshold).map(_.amount).sum
    val got = RecScan.sumAmountWhereGt(buf, 0, buf.length, threshold)
    assertEquals(ref, got, 1e-6)

  @Test def category_matches_jsoniter(): Unit =
    val recs = jsoniterRecs
    val ref = recs.filter(_.amount > threshold).map(_.category).toList
    val got = RecScan.categoryWhereAmountGt(buf, 0, buf.length, threshold).toList
    assertEquals(ref, got)

  @Test def category_matches_reference(): Unit =
    val ref = scala.collection.mutable.ArrayBuffer.empty[String]
    var i = 0
    while i < N do
      if JsonGen.amountOf(i) > threshold then ref += JsonGen.categoryOf(i)
      i += 1
    val got = RecScan.categoryWhereAmountGt(buf, 0, buf.length, threshold)
    assertEquals(ref.toList, got.toList)

  /** Query C: a projected NUMERIC field stays a raw byte slice (zero decode). Decode the slice here only to check it equals the reference `id` — proving the
    * slice points at the right bytes.
    */
  @Test def idSlices_match_jsoniter(): Unit =
    val recs = jsoniterRecs
    val ref = recs.filter(_.amount > threshold).map(_.id).toList
    val packed = RecScan.idSlicesWhereAmountGt(buf, 0, buf.length, threshold)
    val got = packed.map { sl =>
      val start = (sl >>> 32).toInt
      val len = (sl & 0xffffffffL).toInt
      new String(buf, start, len, java.nio.charset.StandardCharsets.ISO_8859_1).toLong
    }.toList
    assertEquals(ref, got)

  // ---- fat-numeric: read 1-2 of 16 doubles, others never decoded ----
  given fatCodec: JsonValueCodec[FatRec] = JsonCodecMaker.make
  private val fatBuf = FatGen.ndjson(N)
  private val fatThreshold = 75.0

  private def fatRecs: Vector[FatRec] =
    val out = Vector.newBuilder[FatRec]; var start = 0
    while start < fatBuf.length do
      var end = start; while end < fatBuf.length && fatBuf(end) != '\n' do end += 1
      out += readFromSubArray[FatRec](fatBuf, start, end); start = end + 1
    out.result()

  @Test def fatM5_matches_jsoniter(): Unit =
    val ref = fatRecs.filter(_.m0 > fatThreshold).map(_.m5).sum
    val got = FatScan.sumM5WhereM0Gt(fatBuf, 0, fatBuf.length, fatThreshold)
    assertEquals(ref, got, 1e-6)

  @Test def fat3_matches_jsoniter(): Unit =
    val ref = fatRecs.filter(_.m0 > fatThreshold).map(r => r.m1 + r.m2 + r.m3).sum
    val got = FatScan.sumM1M2M3WhereM0Gt(fatBuf, 0, fatBuf.length, fatThreshold)
    assertEquals(ref, got, 1e-6)

  /** verify the strongest jsoniter baseline (hand-written JsonReader projection) is CORRECT before we trust its benchmark — a fast-but-wrong baseline is
    * worthless.
    */
  @Test def jsoniterReaderManual_is_correct(): Unit =
    val codec: JsonValueCodec[Double] = new JsonValueCodec[Double]:
      def decodeValue(in: JsonReader, default: Double): Double =
        var amount = default
        if !in.isNextToken('{') then in.decodeError("expected {")
        if !in.isNextToken('}') then
          in.rollbackToken()
          var continue = true
          while continue do
            val len = in.readKeyAsCharBuf()
            if in.isCharBufEqualsTo(len, "amount") then amount = in.readDouble()
            else in.skip()
            continue = in.isNextToken(',')
          if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        amount
      def encodeValue(x: Double, out: JsonWriter): Unit = out.writeVal(x)
      def nullValue: Double = 0.0
    var acc = 0.0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val a = readFromSubArray[Double](buf, start, end)(using codec)
      if a > threshold then acc += a
      start = end + 1
    val ref = RecScan.sumAmountWhereGt(buf, 0, buf.length, threshold)
    assertEquals(ref, acc, 1e-6)

end JsonScanTest
