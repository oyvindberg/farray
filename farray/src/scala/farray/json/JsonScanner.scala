package farray.json

/** Byte-level JSON scanning primitives — the runtime helpers the `fuse` JSON macro GENERATES calls to
 *  per record (`internKey`/`keyEquals`/`scanStringEnd`/`skipValue`/`readIntAt`/`readLongAt`/`readDoubleAt`/
 *  `decodeLatin1`), and which the hand-written reference scanners in `Demo`/`FatNumeric` also use.
 *
 *  Techniques lifted from jsoniter-scala's `JsonReader` (the JVM king): the position threaded as an explicit
 *  `Int`, the string-aware skip family (string = quote+escape scan, containers = brace counting that routes
 *  strings through the same scan so braces inside them don't miscount), and negated-magnitude integer
 *  accumulation. The hot string-end scan is SWAR (8 bytes/word) in `JsonBytes`; double parsing is the
 *  allocation-free tiered parser in `JsonNum`.
 *
 *  All methods take `(buf, pos, end)` and return the new `pos` (the `…At` decoders stash it in `numEnd`).
 *  Cursor invariant: `pos` points at the next unread byte.
 */
object JsonScanner:

  inline def isWs(b: Byte): Boolean =
    b == ' ' || b == '\n' || b == '\t' || b == '\r'

  /** advance past whitespace; returns the position of the first non-ws byte. */
  def skipWs(buf: Array[Byte], pos0: Int, end: Int): Int =
    var pos = pos0
    while pos < end && isWs(buf(pos)) do pos += 1
    pos

  // ---- string scanning (escape-aware) ----

  /** `pos` points just AFTER the opening quote of a string; scan to the closing unescaped quote (SWAR,
   *  8 bytes at a time, in Java). Returns the position of the closing quote (so [start, ret) is content). */
  inline def scanStringEnd(buf: Array[Byte], start: Int, end: Int): Int =
    JsonBytes.scanStringEnd(buf, start, end)

  // ---- value skipping (THE projection primitive) ----

  /** skip a whole JSON value at `pos` (which must already be AT the value's first byte — callers in compact
   *  NDJSON position the cursor exactly, avoiding a redundant whitespace scan, which the v0a profile showed
   *  was the #1 cost). No decode, no allocation — the heart of projection pushdown. */
  def skipValue(buf: Array[Byte], pos: Int, end: Int): Int =
    val b = buf(pos)
    if b == '"' then scanStringEnd(buf, pos + 1, end) + 1
    else if (b >= '0' && b <= '9') || b == '-' then skipNumber(buf, pos, end)
    else if b == '{' then skipContainer(buf, pos + 1, end, '{', '}')
    else if b == '[' then skipContainer(buf, pos + 1, end, '[', ']')
    else if b == 't' || b == 'n' then pos + 4 // true / null
    else if b == 'f' then pos + 5 // false
    else throw JsonParseException(s"unexpected value byte '${b.toChar}' at $pos")

  /** consume the maximal run of number bytes. */
  def skipNumber(buf: Array[Byte], pos0: Int, end: Int): Int =
    var pos = pos0
    while pos < end do
      val b = buf(pos)
      if (b >= '0' && b <= '9') || b == '-' || b == '+' || b == '.' || b == 'e' || b == 'E' then pos += 1
      else return pos
    pos

  /** `pos` is just past the opening `open`; skip to the matching `close`, string-aware (braces inside
   *  string values don't miscount). Returns the position just past `close`. */
  def skipContainer(buf: Array[Byte], pos0: Int, end: Int, open: Byte, close: Byte): Int =
    var pos = pos0
    var depth = 0
    while pos < end do
      val b = buf(pos)
      if b == '"' then pos = scanStringEnd(buf, pos + 1, end) + 1
      else if b == open then { depth += 1; pos += 1 }
      else if b == close then
        if depth == 0 then return pos + 1
        else { depth -= 1; pos += 1 }
      else pos += 1
    throw JsonParseException(s"unterminated container from $pos0")

  // ---- number decoding (scalar, correct; SWAR later) ----

  /** decode a JSON double via the allocation-free Java byte-level parser (tiers 1-2 allocate nothing;
   *  only genuinely-long/out-of-window inputs fall back to Double.parseDouble). Returns the position past it. */
  def readDouble(buf: Array[Byte], pos0: Int, end: Int, out: JsonNum.D): Int =
    val start = skipWs(buf, pos0, end)
    JsonNum.parseDouble(buf, start, end, out)
    out.pos

  // ---- macro-facing decoders that return the PRIMITIVE value (no tuple, no boxing) and stash the new
  //      position in a thread-local `numEnd`. The generated scanner reads numEnd right after. ----
  private val numEndTL: ThreadLocal[Array[Int]] = ThreadLocal.withInitial(() => new Array[Int](1))
  inline def numEnd: Int = numEndTL.get()(0)

  def readIntAt(buf: Array[Byte], pos0: Int, end: Int): Int =
    var pos = pos0; var neg = false
    if pos < end && buf(pos) == '-' then { neg = true; pos += 1 }
    var x = 0
    while pos < end && { val b = buf(pos); b >= '0' && b <= '9' } do { x = x * 10 - (buf(pos) - '0'); pos += 1 }
    numEndTL.get()(0) = pos
    if neg then x else -x

  def readLongAt(buf: Array[Byte], pos0: Int, end: Int): Long =
    var pos = pos0; var neg = false
    if pos < end && buf(pos) == '-' then { neg = true; pos += 1 }
    var x = 0L
    while pos < end && { val b = buf(pos); b >= '0' && b <= '9' } do { x = x * 10 - (buf(pos) - '0'); pos += 1 }
    numEndTL.get()(0) = pos
    if neg then x else -x

  def readDoubleAt(buf: Array[Byte], pos0: Int, end: Int): Double =
    val d = JsonNumLocal.d.get()
    JsonNum.parseDouble(buf, pos0, end, d)
    numEndTL.get()(0) = d.pos
    d.value

  // ---- key dispatch by raw byte comparison (the edge over jsoniter: NO key decode) ----

  /** intern a wanted field name to a cached UTF-8 byte array (allocated once per distinct name, ever) — the
   *  macro binds the result to a val above the scan loop so per-record key compares allocate nothing. */
  private val keyCache = new java.util.concurrent.ConcurrentHashMap[String, Array[Byte]]()
  def internKey(name: String): Array[Byte] =
    keyCache.computeIfAbsent(name, n => n.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  /** does the key slice [start, stop) equal the wanted name bytes, length-then-memcmp? */
  inline def keyEquals(buf: Array[Byte], start: Int, stop: Int, name: Array[Byte]): Boolean =
    val len = stop - start
    len == name.length && java.util.Arrays.equals(buf, start, stop, name, 0, len)

  /** ASCII/latin-1 fast-path string decode for a slice known to be escape-free. */
  inline def decodeLatin1(buf: Array[Byte], start: Int, len: Int): String =
    new String(buf, start, len, java.nio.charset.StandardCharsets.ISO_8859_1)

end JsonScanner

/** thread-local reusable double holder so `readDoubleAt` allocates nothing per call. */
object JsonNumLocal:
  val d: ThreadLocal[JsonNum.D] = ThreadLocal.withInitial(() => new JsonNum.D())
