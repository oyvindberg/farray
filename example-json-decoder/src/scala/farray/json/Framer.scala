package farray.json

import farray.ByteRecordSource

/** The result of scanning the working buffer for the next record — `Framer`'s internal verdict. Making "unfinished record" a FIRST-CLASS value (`Partial`)
  * instead of an offset to remember is what keeps the boundary logic in exactly one place (`Framer.nextRecord`): the byte reader below knows nothing about
  * records, the engine above never sees a `Partial`. Private to this file — it is not part of any public contract.
  */
private enum Framed:
  /** a complete `\n`-terminated record occupies `[start, end)` in the working buffer (end = the `\n`, exclusive). */
  case Record(start: Int, end: Int)

  /** the working buffer ends mid-record: `[start, len)` is an UNFINISHED record — the framer must read more bytes and stitch before it can be framed. (The
    * "send 'unfinished record'" signal.)
    */
  case Partial(start: Int)

  /** the working buffer is fully consumed with no trailing bytes — read another block. */
  case NeedMore

  /** the underlying byte stream is exhausted AND nothing is pending. */
  case End

/** Streams `\n`-framed records out of a raw byte reader in constant memory, owning the carry/working buffer and the boundary stitch — the SOLE place
  * `Framed.Partial` ("unfinished record") is handled. Presents the [[ByteRecordSource]] contract upward (the macro sees only complete, contiguous frames);
  * consumes a JDK-shaped block reader downward (`read(buf, off, max): Int`, `-1` at EOF), which knows nothing about records.
  *
  * Working set = O(read block + the largest single record): the carry buffer grows only to hold one record that straddles reads, never the whole stream. A
  * record longer than `blockSize` triggers a one-time buffer growth.
  */
private[json] class Framer(read: (Array[Byte], Int, Int) => Int, blockSize: Int, doClose: () => Unit) extends ByteRecordSource:
  private val bs: Int = math.max(64, blockSize)
  private var work: Array[Byte] = new Array[Byte](bs * 2) // carry tail + one fresh block
  private var dataEnd: Int = 0 // bytes valid in `work` are [0, dataEnd)
  private var pos: Int = 0 // next unframed byte
  private var recStart: Int = 0
  private var recEnd: Int = 0
  private var eof: Boolean = false // the reader returned -1
  private var closed: Boolean = false

  // ── ByteRecordSource contract (the only public surface) ──
  def buf: Array[Byte] = work
  def recordStart: Int = recStart
  def recordEnd: Int = recEnd

  /** scan `[pos, dataEnd)` for the next record terminator → a `Framed` verdict (no buffer mutation). */
  private def scan(): Framed =
    if pos >= dataEnd then (if eof then Framed.End else Framed.NeedMore)
    else
      var e = pos
      while e < dataEnd && work(e) != '\n' do e += 1
      if e < dataEnd then Framed.Record(pos, e) // found '\n' → complete record
      else if eof then Framed.Record(pos, dataEnd) // last record, no trailing '\n' → still complete at stream end
      else Framed.Partial(pos) // ran off the end with bytes pending → unfinished record

  /** read one more block, FIRST compacting any pending tail (`[pos, dataEnd)`) to the front so an unfinished record becomes contiguous with the new bytes —
    * growing `work` if a single record is larger than the buffer.
    */
  private def refill(): Unit =
    val tail = dataEnd - pos
    if pos > 0 then { System.arraycopy(work, pos, work, 0, tail); pos = 0; dataEnd = tail }
    if dataEnd + bs > work.length then work = java.util.Arrays.copyOf(work, math.max(work.length * 2, dataEnd + bs))
    var got = 0
    // one read attempt per refill; a short read just means the next scan may still be Partial → refill again.
    got = read(work, dataEnd, work.length - dataEnd)
    if got <= 0 then eof = true else dataEnd += got

  def nextChunk(): Boolean =
    // The streaming framer treats the WHOLE stream as one logical "chunk" of records — nextRecord drives refills
    // internally. So nextChunk is true once (there is data to try), false only once fully drained. (The two-level
    // contract still holds: when nextRecord returns false at end-of-data, nextChunk returns false too.)
    !(eof && pos >= dataEnd)

  def nextRecord(): Boolean =
    var verdict = scan()
    // keep reading blocks while the buffer holds only an unfinished record or is empty-but-not-EOF.
    while (verdict match { case Framed.Partial(_) | Framed.NeedMore => true; case _ => false }) do
      refill()
      verdict = scan()
    verdict match
      case Framed.Record(s, e) => recStart = s; recEnd = e; pos = e + 1; true // advance past the '\n'
      case Framed.End          => false
      case _                   => false // unreachable (Partial/NeedMore loop above until Record or End)

  override def close(): Unit =
    if !closed then { closed = true; doClose() }
