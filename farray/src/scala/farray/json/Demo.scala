package farray.json

import java.nio.charset.StandardCharsets.UTF_8

/** The v0a demonstration: a flat 20-field record, and the hand-written projection scanners that are EXACTLY
 *  the shape the `fuse` JSON macro will generate. These prove the thesis before the macro is built — if these
 *  don't beat jsoniter-narrow on a selective query (with near-zero allocation on the aggregate), no macro
 *  elegance saves it (docs/json-parser-research.md §5.6, the kill-or-prove criterion).
 *
 *  Record shape (20 fields): a mix of longs / ints / doubles / strings, names of varied length so the
 *  length-then-first-byte-then-memcmp dispatch is exercised.
 */
final case class Rec(
    id: Long, ts: Long, age: Int, count: Int, score: Double, amount: Double,
    rank: Int, lat: Double, lon: Double, flags: Int,
    name: String, category: String, region: String, status: String, code: String,
    note: String, tag: String, kind: String, source: String, label: String
)

/** The compile-time-baked wanted-key bytes (the macro will generate these from the field set). */
object RecKeys:
  val amount: Array[Byte]   = "amount".getBytes(UTF_8)
  val category: Array[Byte] = "category".getBytes(UTF_8)
  val age: Array[Byte]      = "age".getBytes(UTF_8)
  val name: Array[Byte]     = "name".getBytes(UTF_8)
  val id: Array[Byte]       = "id".getBytes(UTF_8)

/** Hand-written scanners that mirror the macro's intended output for specific queries. */
object RecScan:
  import JsonScanner.*

  /** QUERY A (aggregate, ZERO per-record allocation):
   *    parseJson[Rec].filter(_.amount > t).map(_.amount).sum
   *  Reads ONLY `amount` (1 of 20 fields), skips the other 19 by the byte, decodes no strings, allocates
   *  no Rec, folds into a `var`. This is the constant-memory headline.
   *
   *  Returns the sum of `amount` over records whose amount > threshold. `lineStart`..`lineEnd` is one NDJSON
   *  record (the streaming driver provides these bounds; here we walk an in-memory buffer of many lines). */
  def sumAmountWhereGt(buf: Array[Byte], from: Int, until: Int, threshold: Double): Double =
    val amtRes = new JsonNum.D()
    var acc = 0.0
    var lineStart = from
    while lineStart < until do
      // find end of this NDJSON line
      var lineEnd = lineStart
      while lineEnd < until && buf(lineEnd) != '\n' do lineEnd += 1
      // ---- per-record scanner (generated shape), compact-JSON fast path: no defensive skipWs ----
      var p = lineStart + 1 // past '{'  (compact: structural bytes are adjacent)
      var amount = 0.0
      var amountSeen = false
      var open = buf(lineStart) == '{'
      while open do
        // p is at a key's opening quote (or '}')
        if buf(p) == '}' then open = false
        else
          val ks = p + 1 // after the opening quote
          val ke = scanStringEnd(buf, ks, lineEnd)
          p = ke + 2 // past closing key quote and ':'
          // dispatch by raw bytes — NO key decode
          if !amountSeen && keyEquals(buf, ks, ke, RecKeys.amount) then
            p = readDouble(buf, p, lineEnd, amtRes)
            amount = amtRes.value
            amountSeen = true
            open = false // predicate field found — abandon the rest of the record (early-out)
          else
            p = skipValue(buf, p, lineEnd) // unwanted field — skip the bytes
            if buf(p) == ',' then p += 1 else open = false // ',' → next field; '}' → done
      if amountSeen && amount > threshold then acc += amount
      lineStart = lineEnd + 1
    acc

  /** QUERY B (projection + filter, lazy string decode for survivors only):
   *    parseJson[Rec].filter(_.amount > t).map(_.category).toList
   *  Reads `amount` (predicate) and `category` (projection, decoded LAZILY only for survivors); skips the
   *  other 18. Allocates a String only for the ~selectivity fraction of records. */
  def categoryWhereAmountGt(buf: Array[Byte], from: Int, until: Int, threshold: Double): scala.collection.mutable.ArrayBuffer[String] =
    val amtRes = new JsonNum.D()
    val out = scala.collection.mutable.ArrayBuffer.empty[String]
    var lineStart = from
    while lineStart < until do
      var lineEnd = lineStart
      while lineEnd < until && buf(lineEnd) != '\n' do lineEnd += 1
      var p = lineStart + 1 // past '{'
      var amount = 0.0
      var amountSeen = false
      // category captured as a LAZY slice — not decoded to String yet (the sink defers past the filter)
      var catStart = 0
      var catLen = 0
      var catSeen = false
      var open = buf(lineStart) == '{'
      while open do
        if buf(p) == '}' then open = false
        else
          val ks = p + 1
          val ke = scanStringEnd(buf, ks, lineEnd)
          p = ke + 2 // past closing key quote and ':'
          if !amountSeen && keyEquals(buf, ks, ke, RecKeys.amount) then
            p = readDouble(buf, p, lineEnd, amtRes)
            amount = amtRes.value; amountSeen = true
          else if !catSeen && keyEquals(buf, ks, ke, RecKeys.category) then
            // value is a string: record the slice, DO NOT decode
            val vs = p + 1 // past opening quote
            val ve = scanStringEnd(buf, vs, lineEnd)
            catStart = vs; catLen = ve - vs; catSeen = true
            p = ve + 1
          else
            p = skipValue(buf, p, lineEnd)
          // both wanted fields found? stop early. else expect ',' or '}'
          if amountSeen && catSeen then open = false
          else if buf(p) == ',' then p += 1 else open = false
      // sink: decode category to String ONLY for survivors
      if amountSeen && amount > threshold && catSeen then
        out += decodeLatin1(buf, catStart, catLen)
      lineStart = lineEnd + 1
    out

  /** QUERY C — the user's insight: a PROJECTED numeric field doesn't need to become a `double` at all.
   *  `parseJson[Rec].filter(_.amount > t).map(_.id)` where the result is, say, re-serialized or compared —
   *  `id` is captured as a raw byte SLICE `(start,len)` into the original buffer, exactly like a string.
   *  No double/long decode, no allocation. The predicate field `amount` still decodes (you must compare it),
   *  but the projected `id` stays a slice. Here we just return the survivors' id slices as packed longs
   *  (start<<32 | len) to prove zero decode — a real pipeline would carry these as an FArray column.
   *
   *  This is the general principle: a number is decoded ONLY when an operation forces its numeric value
   *  (sum/compare/arithmetic). Pure projection/pass-through keeps it a slice. */
  def idSlicesWhereAmountGt(buf: Array[Byte], from: Int, until: Int, threshold: Double): scala.collection.mutable.ArrayBuffer[Long] =
    val amtRes = new JsonNum.D()
    val out = scala.collection.mutable.ArrayBuffer.empty[Long]
    var lineStart = from
    while lineStart < until do
      var lineEnd = lineStart
      while lineEnd < until && buf(lineEnd) != '\n' do lineEnd += 1
      var p = lineStart + 1 // past '{'
      var amount = 0.0; var amountSeen = false
      var idStart = 0; var idLen = 0; var idSeen = false // id captured as a SLICE, not decoded
      var open = buf(lineStart) == '{'
      while open do
        if buf(p) == '}' then open = false
        else
          val ks = p + 1
          val ke = scanStringEnd(buf, ks, lineEnd)
          p = ke + 2 // past closing key quote and ':'
          if !amountSeen && keyEquals(buf, ks, ke, RecKeys.amount) then
            p = readDouble(buf, p, lineEnd, amtRes); amount = amtRes.value; amountSeen = true
          else if !idSeen && keyEquals(buf, ks, ke, RecKeys.id) then
            val ve = skipNumber(buf, p, lineEnd) // just find the bounds — NO decode
            idStart = p; idLen = ve - p; idSeen = true
            p = ve
          else
            p = skipValue(buf, p, lineEnd)
          if amountSeen && idSeen then open = false
          else if buf(p) == ',' then p += 1 else open = false
      if amountSeen && amount > threshold && idSeen then
        out += (idStart.toLong << 32) | (idLen.toLong & 0xffffffffL) // the raw slice, zero decode
      lineStart = lineEnd + 1
    out

end RecScan
