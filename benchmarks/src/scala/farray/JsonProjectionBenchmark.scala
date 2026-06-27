package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import org.typelevel.jawn.ast.{JParser, JValue}
import farray.json.{RecScan, Rec, FatScan, FatRec, Json}

/** THE kill-or-prove experiment (docs/json-parser-research.md §5.6).
 *
 *  Query: over NDJSON of 20-field records, `filter(amount > t).map(amount).sum` (aggregate, the zero-alloc
 *  headline) and `filter(amount > t).map(category)` (projection + lazy string).
 *
 *  Contenders:
 *   - fused        : the hand-written projection scanner (the shape the macro will generate) — reads 1-2 of
 *                    20 fields, skips the rest by the byte, lazy string decode, predicate early-out.
 *   - jsoniterFull : jsoniter parses the FULL 20-field Rec, then filter/map in Scala (the common baseline).
 *   - jsoniterNarrow: jsoniter parses a NARROW record (only the read fields), skipping the rest — jsoniter's
 *                    own best projection. The FAIREST baseline (still decodes every key + byte-walks skips).
 *
 *  Success criterion: fused throughput >= jsoniterNarrow (target >=1.3x), and near-zero alloc on the sum
 *  variant (run with -prof gc). Selectivity ~25% here (amount > 150, amounts in [0,298.5]).
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array("-Xms2g", "-Xmx2g", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch")
)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class JsonProjectionBenchmark:

  @Param(Array("10000"))
  var n: Int = 10000

  val threshold = 150.0

  var buf: Array[Byte] = null
  var fatBuf: Array[Byte] = null
  var wideBuf: Array[Byte] = null

  given recCodec: JsonValueCodec[Rec] = JsonCodecMaker.make
  // narrow record: ONLY the fields the query reads; jsoniter skips the other 18.
  given narrowCodec: JsonValueCodec[JsonProjectionBenchmark.Narrow] = JsonCodecMaker.make
  // fat-numeric narrow schemas: jsoniter must DECODE every numeric field it declares.
  given fatN2Codec: JsonValueCodec[JsonProjectionBenchmark.FatN2] = JsonCodecMaker.make
  given fatN4Codec: JsonValueCodec[JsonProjectionBenchmark.FatN4] = JsonCodecMaker.make
  given wideCodec: JsonValueCodec[JsonProjectionBenchmark.Wide] = JsonCodecMaker.make
  given wideNarrowCodec: JsonValueCodec[JsonProjectionBenchmark.WideNarrow] = JsonCodecMaker.make

  @Setup(Level.Trial)
  def setup(): Unit =
    buf = JsonProjectionBenchmark.ndjson(n)
    fatBuf = JsonProjectionBenchmark.fatNdjson(n)
    wideBuf = JsonProjectionBenchmark.wideNdjson(n)

  // ---- PREDICATE-FAIL EARLY-OUT showcase: `key` (predicate Int) is the FIRST field; `payload` (a long
  //      projected string) is the LAST; 10% selectivity. Rejected records (90%) abandon the scan right after
  //      `key` and never touch the ~10 middle fields or the long payload string. ----
  val wideKeyThreshold = 90 // key in [0,100); keep > 90 → 10% pass

  @Benchmark def wide_fuseMacro(): Int =
    Json.ndjson[JsonProjectionBenchmark.Wide](wideBuf).fuse
      .filter(_.key > wideKeyThreshold).map(_.payload).count

  /** jsoniter-narrow {key, payload}: must still byte-walk to find both, and decodes payload for survivors. */
  @Benchmark def wide_jsoniterNarrow(): Int =
    var c = 0; var start = 0
    while start < wideBuf.length do
      var end = start; while end < wideBuf.length && wideBuf(end) != '\n' do end += 1
      val r = readFromSubArray[JsonProjectionBenchmark.WideNarrow](wideBuf, start, end)
      if r.key > wideKeyThreshold then c += 1
      start = end + 1
    c

  // ---- AGGREGATE query: filter(amount > t).map(amount).sum ----

  /** the MACRO-DRIVEN path: the EXISTING fuse optimizer (filter→if, map, sink, DCE, fold) drives the byte
   *  scanner. This is the real product — the hand-written `sum_fused` is just its spec. */
  @Benchmark def sum_fuseMacro(): Double =
    Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(_.amount).sum

  /** same query via foldLeft (avoids Numeric[Double].plus boxing that `.sum` incurs) — the true zero-alloc path. */
  @Benchmark def sum_fuseMacroFold(): Double =
    Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).foldLeft(0.0)((a, r) => a + r.amount)

  @Benchmark def sum_fused(): Double =
    RecScan.sumAmountWhereGt(buf, 0, buf.length, threshold)

  @Benchmark def sum_jsoniterFull(): Double =
    var acc = 0.0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val r = readFromSubArray[Rec](buf, start, end)
      if r.amount > threshold then acc += r.amount
      start = end + 1
    acc

  @Benchmark def sum_jsoniterNarrow(): Double =
    var acc = 0.0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val r = readFromSubArray[JsonProjectionBenchmark.Narrow](buf, start, end)
      if r.amount > threshold then acc += r.amount
      start = end + 1
    acc

  /** jawn (typelevel's high-reputation parser): parse each line DIRECTLY FROM BYTES to a JValue AST, then
   *  extract the field. jawn is a fast tokenizer, but it has no projection — it builds the whole AST (a JObject
   *  of all 20 fields, every value boxed into a JValue) per record before you can read one field. */
  @Benchmark def sum_jawn(): Double =
    var acc = 0.0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val j = JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(buf, start, end - start)).get
      val a = j.get("amount").asDouble
      if a > threshold then acc += a
      start = end + 1
    acc

  /** Jackson databind, tree model: build a JsonNode tree (all 20 fields) per line, then read one. The classic
   *  reflection-free-but-allocation-heavy path everyone reaches for first. (Included for scale.) */
  @Benchmark def sum_jackson(): Double =
    var acc = 0.0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val node = JsonProjectionBenchmark.jackson.readTree(buf, start, end - start)
      val a = node.get("amount").asDouble
      if a > threshold then acc += a
      start = end + 1
    acc

  /** THE strongest jsoniter baseline: a hand-written JsonReader loop (jsoniter's own tuned reader + SWAR),
   *  manual key dispatch, decode ONLY `amount`, skip the other 19, and return the PRIMITIVE — no case-class
   *  object allocated. This is jsoniter at its absolute best for this projection: no codec object, folds into
   *  a var. Beating (or honestly measuring against) this is the real proof the win is fuse's model. */
  @Benchmark def sum_jsoniterReaderManual(): Double =
    var acc = 0.0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val amount = readFromSubArray[Double](buf, start, end)(using JsonProjectionBenchmark.amountCodec)
      if amount > threshold then acc += amount
      start = end + 1
    acc

  // ---- PROJECTION query: filter(amount > t).map(category) ----

  /** macro-driven projection + LAZY string decode (the sink decodes category only for survivors). */
  @Benchmark def cat_fuseMacro(): Int =
    Json.ndjson[Rec](buf).fuse.filter(_.amount > threshold).map(_.category).count

  @Benchmark def cat_fused(): Int =
    RecScan.categoryWhereAmountGt(buf, 0, buf.length, threshold).size

  @Benchmark def cat_jsoniterNarrow(): Int =
    var c = 0; var start = 0
    while start < buf.length do
      var end = start; while end < buf.length && buf(end) != '\n' do end += 1
      val r = readFromSubArray[JsonProjectionBenchmark.Narrow](buf, start, end)
      if r.amount > threshold then c += 1
      start = end + 1
    c

  // ---- FAT-NUMERIC: 16-double records, read 1-2, skip/slice the rest (the user's hypothesis) ----
  // m0/m5 in [0,150); threshold 75 → ~50% selectivity so the compute-for-survivors win shows.
  val fatThreshold = 75.0

  @Benchmark def fatM5_fused(): Double =
    FatScan.sumM5WhereM0Gt(fatBuf, 0, fatBuf.length, fatThreshold)

  /** jsoniter-narrow {m0, m5}: decodes BOTH doubles for every record + allocates the record. */
  @Benchmark def fatM5_jsoniterNarrow(): Double =
    var acc = 0.0; var start = 0
    while start < fatBuf.length do
      var end = start; while end < fatBuf.length && fatBuf(end) != '\n' do end += 1
      val r = readFromSubArray[JsonProjectionBenchmark.FatN2](fatBuf, start, end)
      if r.m0 > fatThreshold then acc += r.m5
      start = end + 1
    acc

  @Benchmark def fat3_fused(): Double =
    FatScan.sumM1M2M3WhereM0Gt(fatBuf, 0, fatBuf.length, fatThreshold)

  /** jsoniter-narrow {m0,m1,m2,m3}: decodes ALL FOUR doubles for EVERY record (no compute-for-survivors). */
  @Benchmark def fat3_jsoniterNarrow(): Double =
    var acc = 0.0; var start = 0
    while start < fatBuf.length do
      var end = start; while end < fatBuf.length && fatBuf(end) != '\n' do end += 1
      val r = readFromSubArray[JsonProjectionBenchmark.FatN4](fatBuf, start, end)
      if r.m0 > fatThreshold then acc += r.m1 + r.m2 + r.m3
      start = end + 1
    acc

end JsonProjectionBenchmark

object JsonProjectionBenchmark:
  /** a single reused Jackson mapper (allocating one per call would be even slower / unfair). */
  val jackson: com.fasterxml.jackson.databind.ObjectMapper = new com.fasterxml.jackson.databind.ObjectMapper()

  /** the narrow projection record jsoniter parses (everything else is skipped). */
  final case class Narrow(amount: Double, category: String)

  /** Wide record for the predicate-fail early-out showcase: `key` FIRST, 10 filler fields, `payload` LAST.
   *  Field declaration order matches the JSON write order (the generator emits them in this order). */
  final case class Wide(
      key: Int, f1: Int, f2: Int, f3: Int, f4: Double, f5: Double,
      f6: String, f7: String, f8: String, f9: String, f10: String, payload: String)
  final case class WideNarrow(key: Int, payload: String)

  def wideRecord(i: Int): String =
    val key = i % 100
    val payload = s"payload_${i}_" + ("x" * 60) // a long string (~70 chars) — expensive to decode
    s"""{"key":$key,"f1":${i % 7},"f2":${i % 13},"f3":${i % 5},"f4":${(i % 100) / 3.0},"f5":${(i % 50) / 7.0},""" +
      s""""f6":"a$i","f7":"b$i","f8":"c$i","f9":"d$i","f10":"e$i","payload":"$payload"}"""

  def wideNdjson(n: Int): Array[Byte] =
    val sb = new java.lang.StringBuilder(n * 200); var i = 0
    while i < n do { sb.append(wideRecord(i)).append('\n'); i += 1 }
    sb.toString.getBytes(UTF_8)

  /** Hand-written jsoniter JsonReader projection: decode ONLY `amount`, skip the rest, return the primitive
   *  (no object). The strongest jsoniter contender — the same shape as our fused scanner, on jsoniter's reader. */
  val amountCodec: JsonValueCodec[Double] = new JsonValueCodec[Double]:
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
  /** fat-numeric narrow schemas — jsoniter DECODES exactly these numeric fields. */
  final case class FatN2(m0: Double, m5: Double)
  final case class FatN4(m0: Double, m1: Double, m2: Double, m3: Double)

  // fat record: 16 double metrics + id, shuffled field order.
  private def fatFields(i: Int): Array[(String, String)] =
    val fs = new Array[(String, String)](17)
    fs(0) = "id" -> (i.toLong * 1000L + 7L).toString
    var k = 0
    while k < 16 do
      // metric value in [0,150), deterministic; m0 and m5 drive the predicate/projection.
      val v = ((i * 31 + k * 7) % 1500) / 10.0
      fs(k + 1) = s"m$k" -> f"$v%.3f"
      k += 1
    fs

  def fatRecord(i: Int): String =
    val fs = fatFields(i)
    val rnd = new java.util.Random(i.toLong * 2654435761L)
    var k = fs.length - 1
    while k > 0 do { val j = rnd.nextInt(k + 1); val t = fs(k); fs(k) = fs(j); fs(j) = t; k -= 1 }
    val sb = new java.lang.StringBuilder(256); sb.append('{'); var first = true
    for (key, v) <- fs do
      if !first then sb.append(','); first = false
      sb.append('"').append(key).append("\":").append(v)
    sb.append('}'); sb.toString

  def fatNdjson(n: Int): Array[Byte] =
    val sb = new java.lang.StringBuilder(n * 256); var i = 0
    while i < n do { sb.append(fatRecord(i)).append('\n'); i += 1 }
    sb.toString.getBytes(UTF_8)

  // generator duplicated here (benchmarks doesn't depend on tests); kept in sync with farray.json.JsonGen.
  private def fields(i: Int): Array[(String, String)] =
    val s = i.toString
    Array(
      "id" -> (i.toLong * 1000L + 7L).toString, "ts" -> (1_700_000_000_000L + i).toString,
      "age" -> (i % 100).toString, "count" -> (i % 50).toString,
      "score" -> f"${(i % 1000) / 10.0}%.2f", "amount" -> f"${(i % 200) * 1.5}%.2f",
      "rank" -> (i % 10).toString, "lat" -> f"${(i % 180) - 90}.${i % 100}",
      "lon" -> f"${(i % 360) - 180}.${i % 100}", "flags" -> (i % 8).toString,
      "name" -> s""""name_$s"""", "category" -> s""""cat_${i % 12}"""",
      "region" -> s""""region_${i % 6}"""", "status" -> s""""${if i % 2 == 0 then "active" else "inactive"}"""",
      "code" -> s""""C${i % 1000}"""", "note" -> s""""note text for record $s with some words"""",
      "tag" -> s""""tag_${i % 20}"""", "kind" -> s""""kind_${i % 4}"""",
      "source" -> s""""src_${i % 7}"""", "label" -> s""""label_${i % 15}""""
    )

  def record(i: Int): String =
    val fs = fields(i)
    val rnd = new java.util.Random(i.toLong * 2654435761L)
    var k = fs.length - 1
    while k > 0 do { val j = rnd.nextInt(k + 1); val t = fs(k); fs(k) = fs(j); fs(j) = t; k -= 1 }
    val sb = new java.lang.StringBuilder(256); sb.append('{'); var first = true
    for (key, v) <- fs do
      if !first then sb.append(','); first = false
      sb.append('"').append(key).append("\":").append(v)
    sb.append('}'); sb.toString

  def ndjson(n: Int): Array[Byte] =
    val sb = new java.lang.StringBuilder(n * 256); var i = 0
    while i < n do { sb.append(record(i)).append('\n'); i += 1 }
    sb.toString.getBytes(UTF_8)
