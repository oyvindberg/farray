package farray.json

import java.nio.charset.StandardCharsets.UTF_8

/** The "many numeric fields, read few" demonstration (the user's hypothesis): when a record has MANY numeric
 *  fields and a query reads only one or two, the lazy-decode win widens — because jsoniter (even with a narrow
 *  schema) must DECODE every numeric field its schema declares, paying the double-parse cost per field per
 *  record, while the fused scanner decodes only the predicate field and skips the rest as raw bytes.
 *
 *  Record: 16 double metrics m0..m15 + an id. ~250 bytes/line. */
final case class FatRec(
    id: Long,
    m0: Double, m1: Double, m2: Double, m3: Double, m4: Double, m5: Double, m6: Double, m7: Double,
    m8: Double, m9: Double, m10: Double, m11: Double, m12: Double, m13: Double, m14: Double, m15: Double
)

object FatKeys:
  // the metric keys, baked as bytes (the macro generates these from the field set).
  val m: Array[Array[Byte]] = Array.tabulate(16)(i => s"m$i".getBytes(UTF_8))
  val m0: Array[Byte] = m(0)
  val m5: Array[Byte] = m(5)

object FatScan:
  import JsonScanner.*

  /** QUERY: parseJson[FatRec].filter(_.m0 > t).map(_.m5).sum
   *  Decode m0 (predicate) and m5 (projection) ONLY; skip the other 14 numeric fields as raw bytes — never
   *  parse them to doubles. jsoniter-narrow with schema {m0, m5} would also skip the other 14, so the FAIR
   *  comparison is against that. The win here is: we decode exactly 2 doubles/record (and m5 only for
   *  survivors — the sink), jsoniter-narrow decodes 2 doubles AND allocates a Narrow case class/record. */
  def sumM5WhereM0Gt(buf: Array[Byte], from: Int, until: Int, threshold: Double): Double =
    val d = new JsonNum.D()
    var acc = 0.0
    var lineStart = from
    while lineStart < until do
      var lineEnd = lineStart
      while lineEnd < until && buf(lineEnd) != '\n' do lineEnd += 1
      var p = lineStart + 1 // past '{'
      var m0 = 0.0; var m0Seen = false
      var m5Start = 0; var m5End = 0; var m5Seen = false // m5 captured as a SLICE — decoded only for survivors
      var open = buf(lineStart) == '{'
      while open do
        if buf(p) == '}' then open = false
        else
          val ks = p + 1
          val ke = scanStringEnd(buf, ks, lineEnd)
          p = ke + 2 // past closing key quote and ':'
          if !m0Seen && keyEquals(buf, ks, ke, FatKeys.m0) then
            p = readDouble(buf, p, lineEnd, d); m0 = d.value; m0Seen = true
          else if !m5Seen && keyEquals(buf, ks, ke, FatKeys.m5) then
            val ve = skipNumber(buf, p, lineEnd) // SLICE only — no decode yet (sink defers past the filter)
            m5Start = p; m5End = ve; m5Seen = true
            p = ve
          else
            p = skipValue(buf, p, lineEnd) // the other 14 numerics: skipped as BYTES, never parsed
          if m0Seen && m5Seen then open = false
          else if buf(p) == ',' then p += 1 else open = false
      // sink: decode m5 to a double ONLY for survivors of the m0 predicate
      if m0Seen && m0 > threshold && m5Seen then
        JsonNum.parseDouble(buf, m5Start, m5End, d)
        acc += d.value
      lineStart = lineEnd + 1
    acc

  /** QUERY (even more extreme): sum SEVERAL metrics for survivors only.
   *  parseJson[FatRec].filter(_.m0 > t).map(r => r.m1 + r.m2 + r.m3).sum
   *  m0 decodes always (predicate); m1/m2/m3 decode ONLY for survivors (compute-for-survivors). The other 12
   *  metrics are never decoded. jsoniter-narrow {m0,m1,m2,m3} decodes all 4 for EVERY record. With low
   *  selectivity this is the multiplier: (#extra decoded) x (1 - selectivity). */
  def sumM1M2M3WhereM0Gt(buf: Array[Byte], from: Int, until: Int, threshold: Double): Double =
    val d = new JsonNum.D()
    var acc = 0.0
    var lineStart = from
    while lineStart < until do
      var lineEnd = lineStart
      while lineEnd < until && buf(lineEnd) != '\n' do lineEnd += 1
      var p = lineStart + 1
      var m0 = 0.0; var m0Seen = false
      var s1 = 0; var e1 = 0; var seen1 = false
      var s2 = 0; var e2 = 0; var seen2 = false
      var s3 = 0; var e3 = 0; var seen3 = false
      val k1 = FatKeys.m(1); val k2 = FatKeys.m(2); val k3 = FatKeys.m(3)
      var open = buf(lineStart) == '{'
      while open do
        if buf(p) == '}' then open = false
        else
          val ks = p + 1
          val ke = scanStringEnd(buf, ks, lineEnd)
          p = ke + 2
          if !m0Seen && keyEquals(buf, ks, ke, FatKeys.m0) then
            p = readDouble(buf, p, lineEnd, d); m0 = d.value; m0Seen = true
          else if !seen1 && keyEquals(buf, ks, ke, k1) then { val ve = skipNumber(buf, p, lineEnd); s1 = p; e1 = ve; seen1 = true; p = ve }
          else if !seen2 && keyEquals(buf, ks, ke, k2) then { val ve = skipNumber(buf, p, lineEnd); s2 = p; e2 = ve; seen2 = true; p = ve }
          else if !seen3 && keyEquals(buf, ks, ke, k3) then { val ve = skipNumber(buf, p, lineEnd); s3 = p; e3 = ve; seen3 = true; p = ve }
          else p = skipValue(buf, p, lineEnd)
          if m0Seen && seen1 && seen2 && seen3 then open = false
          else if buf(p) == ',' then p += 1 else open = false
      // compute-for-survivors: decode m1+m2+m3 ONLY when m0 passes
      if m0Seen && m0 > threshold && seen1 && seen2 && seen3 then
        JsonNum.parseDouble(buf, s1, e1, d); val v1 = d.value
        JsonNum.parseDouble(buf, s2, e2, d); val v2 = d.value
        JsonNum.parseDouble(buf, s3, e3, d); val v3 = d.value
        acc += v1 + v2 + v3
      lineStart = lineEnd + 1
    acc

end FatScan
