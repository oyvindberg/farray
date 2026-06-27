package farray.json

import java.nio.charset.StandardCharsets.UTF_8

/** Deterministic NDJSON generator for the 20-field `Rec`, used by both the correctness test and the
 *  benchmark so they agree on the data. Field WRITE ORDER is shuffled per record (seeded) so the scanner's
 *  any-order dispatch + skip logic is genuinely exercised, and `amount` is sometimes early / sometimes late
 *  in the record (so predicate early-out is tested both ways). */
object JsonGen:

  /** the 20 (key, value-renderer) pairs for record i; value content is deterministic from i. */
  private def fields(i: Int): Array[(String, String)] =
    val s = i.toString
    Array(
      "id"       -> (i.toLong * 1000L + 7L).toString,
      "ts"       -> (1_700_000_000_000L + i).toString,
      "age"      -> (i % 100).toString,
      "count"    -> (i % 50).toString,
      "score"    -> f"${(i % 1000) / 10.0}%.2f",
      "amount"   -> f"${(i % 200) * 1.5}%.2f",
      "rank"     -> (i % 10).toString,
      "lat"      -> f"${(i % 180) - 90}.${i % 100}",
      "lon"      -> f"${(i % 360) - 180}.${i % 100}",
      "flags"    -> (i % 8).toString,
      "name"     -> s""""name_$s"""",
      "category" -> s""""cat_${i % 12}"""",
      "region"   -> s""""region_${i % 6}"""",
      "status"   -> s""""${if i % 2 == 0 then "active" else "inactive"}"""",
      "code"     -> s""""C${i % 1000}"""",
      "note"     -> s""""note text for record $s with some words"""",
      "tag"      -> s""""tag_${i % 20}"""",
      "kind"     -> s""""kind_${i % 4}"""",
      "source"   -> s""""src_${i % 7}"""",
      "label"    -> s""""label_${i % 15}""""
    )

  /** render record i as a single-line JSON object with fields in a shuffled order (seeded by i). */
  def record(i: Int): String =
    val fs = fields(i)
    // Fisher-Yates with a per-record seed
    val rnd = new java.util.Random(i.toLong * 2654435761L)
    var k = fs.length - 1
    while k > 0 do
      val j = rnd.nextInt(k + 1)
      val t = fs(k); fs(k) = fs(j); fs(j) = t
      k -= 1
    val sb = new java.lang.StringBuilder(256)
    sb.append('{')
    var first = true
    for (key, v) <- fs do
      if !first then sb.append(',')
      first = false
      sb.append('"').append(key).append("\":").append(v)
    sb.append('}')
    sb.toString

  /** an NDJSON buffer of `n` records, plus the reference answers for the standard queries. */
  def ndjson(n: Int): Array[Byte] =
    val sb = new java.lang.StringBuilder(n * 256)
    var i = 0
    while i < n do { sb.append(record(i)).append('\n'); i += 1 }
    sb.toString.getBytes(UTF_8)

  /** reference: amount value for record i (must match the generator's "amount" field). */
  def amountOf(i: Int): Double = (i % 200) * 1.5

  /** reference: category string for record i. */
  def categoryOf(i: Int): String = s"cat_${i % 12}"

end JsonGen

/** Fat-numeric NDJSON: 16 double metrics + id, shuffled. Kept in sync with the benchmark's fat generator.
 *  (Makes concrete the observation that a record is "basically all numbers" and reading one while skipping
 *  the rest as bytes is the lazy-decode win.) */
object FatGen:
  private def fields(i: Int): Array[(String, String)] =
    val fs = new Array[(String, String)](17)
    fs(0) = "id" -> (i.toLong * 1000L + 7L).toString
    var k = 0
    while k < 16 do
      val v = ((i * 31 + k * 7) % 1500) / 10.0
      fs(k + 1) = s"m$k" -> f"$v%.3f"
      k += 1
    fs

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

end FatGen
