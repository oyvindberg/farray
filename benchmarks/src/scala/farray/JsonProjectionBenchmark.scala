package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import farray.json.{RecScan, Rec, FatScan, FatRec}

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

  given recCodec: JsonValueCodec[Rec] = JsonCodecMaker.make
  // narrow record: ONLY the fields the query reads; jsoniter skips the other 18.
  given narrowCodec: JsonValueCodec[JsonProjectionBenchmark.Narrow] = JsonCodecMaker.make
  // fat-numeric narrow schemas: jsoniter must DECODE every numeric field it declares.
  given fatN2Codec: JsonValueCodec[JsonProjectionBenchmark.FatN2] = JsonCodecMaker.make
  given fatN4Codec: JsonValueCodec[JsonProjectionBenchmark.FatN4] = JsonCodecMaker.make

  @Setup(Level.Trial)
  def setup(): Unit =
    buf = JsonProjectionBenchmark.ndjson(n)
    fatBuf = JsonProjectionBenchmark.fatNdjson(n)

  // ---- AGGREGATE query: filter(amount > t).map(amount).sum ----

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

  // ---- PROJECTION query: filter(amount > t).map(category) ----

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
  /** the narrow projection record jsoniter parses (everything else is skipped). */
  final case class Narrow(amount: Double, category: String)
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
