package farray.json

import org.junit.Test
import org.junit.Assert.*
import java.io.{ByteArrayInputStream, InputStream}

/** STREAMING NDJSON — `Json.ndjsonStream[Rec](in, blockSize)` must produce byte-identical answers to the in-memory `Json.ndjson[Rec](buf)` path, across
  * pathological block sizes (records straddle block boundaries, blocks split mid-key / mid-number / mid-string, a single record larger than the block). Same
  * decomposed scan-pass, only the framing differs — so parity proves the [[Framer]] stitches "unfinished record" correctly. Also: constant-memory over a
  * far-larger-than-heap-budget stream, short-circuit stops reading, and `close()` on all three exit paths.
  */
class StreamingJsonTest:

  private val N = 2000
  private val threshold = 150.0
  private val buf: Array[Byte] = JsonGen.ndjson(N)

  private def inMem[A](f: NdjsonSource[Rec] => A): A = f(Json.ndjson[Rec](buf))
  private def streamed[A](blockSize: Int)(f: StreamingNdjsonSource[Rec] => A): A =
    f(Json.ndjsonStream[Rec](new ByteArrayInputStream(buf), blockSize))

  // block sizes chosen to be hostile: 1 byte (every record straddles many blocks), tiny, a typical record-ish
  // size (splits land mid-field constantly), one near the whole buffer, and a couple of primes to desync.
  private val blockSizes = List(1, 2, 3, 7, 13, 64, 100, 257, 1000, 4096, buf.length - 1, buf.length, buf.length + 1)

  // ── parity: streaming == in-memory across every hostile block size ─────────────────────────────────────────
  @Test def parity_sum(): Unit =
    val ref = inMem(_.fuse.filter(_.amount > threshold).map(_.amount).sum)
    for bs <- blockSizes do
      val got = streamed(bs)(_.fuse.filter(_.amount > threshold).map(_.amount).sum)
      assertEquals(s"blockSize=$bs", ref, got, 1e-6)

  @Test def parity_count(): Unit =
    val ref = inMem(_.fuse.filter(_.amount > threshold).count)
    for bs <- blockSizes do assertEquals(s"blockSize=$bs", ref, streamed(bs)(_.fuse.filter(_.amount > threshold).count))

  @Test def parity_categoryList(): Unit =
    // a projected lazy STRING — a block split mid-string would corrupt the slice if framing were wrong.
    val ref = inMem(_.fuse.filter(_.amount > threshold).map(_.category).toList)
    for bs <- blockSizes do assertEquals(s"blockSize=$bs", ref, streamed(bs)(_.fuse.filter(_.amount > threshold).map(_.category).toList))

  @Test def parity_stringPredicate(): Unit =
    // predicate on a STRING field — block splits land mid-key and mid-value; framing + key compare must hold.
    val ref = inMem(_.fuse.filter(_.status == "active").map(_.amount).sum)
    for bs <- blockSizes do assertEquals(s"blockSize=$bs", ref, streamed(bs)(_.fuse.filter(_.status == "active").map(_.amount).sum), 1e-6)

  @Test def parity_multifield(): Unit =
    val ref = inMem(_.fuse.filter(_.amount > threshold).map(r => r.age + r.count).sum)
    for bs <- blockSizes do assertEquals(s"blockSize=$bs", ref, streamed(bs)(_.fuse.filter(_.amount > threshold).map(r => r.age + r.count).sum))

  @Test def parity_toList_whole(): Unit =
    // materialize the WHOLE record (forces every field) — the strongest framing check.
    val ref = inMem(_.fuse.toList)
    for bs <- blockSizes do assertEquals(s"blockSize=$bs", ref, streamed(bs)(_.fuse.toList))

  // ── a record LARGER than the block → the framer must grow its working buffer ───────────────────────────────
  @Test def recordLargerThanBlock(): Unit =
    // each record here is ~hundreds of bytes; block size 8 means a record spans dozens of reads + a buffer grow.
    val ref = inMem(_.fuse.map(_.id).sum)
    assertEquals(ref, streamed(8)(_.fuse.map(_.id).sum))
    assertEquals(ref, streamed(1)(_.fuse.map(_.id).sum)) // 1 byte at a time — maximal straddling

  // ── trailing record with NO final newline still emitted ────────────────────────────────────────────────────
  @Test def noTrailingNewline(): Unit =
    val trimmed =
      if buf.nonEmpty && buf.last == '\n' then java.util.Arrays.copyOf(buf, buf.length - 1) else buf
    val refSrc = Json.ndjson[Rec](trimmed, 0, trimmed.length)
    val ref = refSrc.fuse.count
    for bs <- List(1, 7, 64, trimmed.length) do
      val got = Json.ndjsonStream[Rec](new ByteArrayInputStream(trimmed), bs).fuse.count
      assertEquals(s"blockSize=$bs", ref, got)

  // ── short-circuit: a downstream take stops pulling (and stops reading) ─────────────────────────────────────
  @Test def shortCircuit_find_stopsReading(): Unit =
    // a recording stream: count bytes actually read. `find` should stop long before the whole stream is consumed.
    val src = new CountingInputStream(new ByteArrayInputStream(buf))
    val got = Json.ndjsonStream[Rec](src, 64).fuse.find(_.amount > threshold)
    assertTrue("find should locate a survivor", got.isDefined)
    assertTrue(s"read ${src.bytesRead} of ${buf.length} — should stop early", src.bytesRead < buf.length)

  // ── close() on all three exit paths ────────────────────────────────────────────────────────────────────────
  @Test def close_onExhaustion(): Unit =
    val src = new ClosingInputStream(new ByteArrayInputStream(buf))
    Json.ndjsonStream[Rec](src, 256).fuse.filter(_.amount > threshold).map(_.amount).sum
    assertTrue("closed after full consumption", src.closed)

  @Test def close_onShortCircuit(): Unit =
    val src = new ClosingInputStream(new ByteArrayInputStream(buf))
    Json.ndjsonStream[Rec](src, 64).fuse.find(_.amount > threshold)
    assertTrue("closed after short-circuit", src.closed)

  @Test def close_onException(): Unit =
    val src = new ClosingInputStream(new ByteArrayInputStream(buf))
    try
      Json
        .ndjsonStream[Rec](src, 256)
        .fuse
        .map { r => if r.amount > threshold then throw new RuntimeException("boom") else r.amount }
        .sum
      fail("expected exception")
    catch case _: RuntimeException => ()
    assertTrue("closed after exception", src.closed)

  // ── constant memory: a stream FAR larger than any sane buffer folds in O(block) ────────────────────────────
  @Test def constantMemory_hugeStream(): Unit =
    // 200k records generated lazily from one repeating line — never materialized as a whole.
    val line = "{\"amount\":42.0,\"category\":\"x\",\"status\":\"active\"}\n".getBytes("UTF-8")
    val total = 200000
    val src = new RepeatingLineStream(line, total)
    val got = Json.ndjsonStream[Rec](src, 4096).fuse.filter(_.amount > 0.0).count
    assertEquals(total, got)

  // ── helpers ────────────────────────────────────────────────────────────────────────────────────────────────
  final class CountingInputStream(in: InputStream) extends InputStream:
    var bytesRead = 0
    def read(): Int = { val b = in.read(); if b >= 0 then bytesRead += 1; b }
    override def read(b: Array[Byte], off: Int, len: Int): Int =
      val n = in.read(b, off, len); if n > 0 then bytesRead += n; n
    override def close(): Unit = in.close()

  final class ClosingInputStream(in: InputStream) extends InputStream:
    var closed = false
    def read(): Int = in.read()
    override def read(b: Array[Byte], off: Int, len: Int): Int = in.read(b, off, len)
    override def close(): Unit = { closed = true; in.close() }

  /** emits `line` `count` times, then EOF — never holds the whole stream (constant-memory generator). */
  final class RepeatingLineStream(line: Array[Byte], count: Int) extends InputStream:
    private var remaining = count
    private var pos = line.length // forces a refill on first read
    def read(): Int =
      if pos >= line.length then
        if remaining <= 0 then return -1
        remaining -= 1; pos = 0
      val b = line(pos) & 0xff; pos += 1; b
