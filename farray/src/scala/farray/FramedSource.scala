package farray

// start:framed-adt
/** A framer's verdict when asked for the next record in the working buffer. Chunks (units of memory) and records (units of meaning) don't line up: a record can
  * straddle two reads. Making that misalignment a first-class VALUE — instead of an offset somebody has to remember — is what keeps the boundary logic in
  * exactly one place ([[FramedByteSource]]): the byte reader below knows nothing about records, and the engine above only ever sees complete frames.
  */
enum Framed:
  /** a complete record occupies `[start, end)` in the working buffer. */
  case Record(start: Int, end: Int)

  /** the buffer ends mid-record: `[start, limit)` is unfinished — read more bytes and re-frame. */
  case Partial(start: Int)

  /** the buffer is fully consumed with nothing pending — read another block. */
  case NeedMore

  /** the underlying byte stream is exhausted and nothing is pending. */
  case End
// stop:framed-adt

/** The reusable half of a streaming byte source: owns the working buffer, the block reads, and the carry/stitch for records that straddle reads — the one
  * genuinely fiddly job in implementing [[ByteRecordSource]], written once. A format plugs in by implementing [[frame]]: look at `[from, limit)` of `buf` and
  * say what's there ([[Framed]]). NDJSON's implementation is a newline scan; a length-prefixed format would read a header.
  *
  * Working set = O(read block + the largest single record): the buffer grows only to hold one straddling record, never the stream.
  */
abstract class FramedByteSource(read: (Array[Byte], Int, Int) => Int, blockSize: Int, doClose: () => Unit) extends ByteRecordSource:
  private val bs: Int = math.max(64, blockSize)
  private var work: Array[Byte] = new Array[Byte](bs * 2) // carry tail + one fresh block
  private var dataEnd: Int = 0 // bytes valid in `work` are [0, dataEnd)
  private var pos: Int = 0 // next unframed byte
  private var recStart: Int = 0
  private var recEnd: Int = 0
  private var eof: Boolean = false // the reader returned -1
  private var closed: Boolean = false

  /** the format's whole job: frame the next record in `buf(from until limit)`, or say why not. Must not mutate anything; `atEof` is true once the underlying
    * stream is exhausted (so a trailing record with no terminator can still be [[Framed.Record]]).
    */
  protected def frame(buf: Array[Byte], from: Int, limit: Int, atEof: Boolean): Framed

  // ── ByteRecordSource contract ──
  def buf: Array[Byte] = work
  def recordStart: Int = recStart
  def recordEnd: Int = recEnd

  /** read one more block, FIRST compacting any pending tail (`[pos, dataEnd)`) to the front so an unfinished record becomes contiguous with the new bytes —
    * growing `work` if a single record is larger than the buffer.
    */
  private def refill(): Unit =
    val tail = dataEnd - pos
    if pos > 0 then { System.arraycopy(work, pos, work, 0, tail); pos = 0; dataEnd = tail }
    if dataEnd + bs > work.length then work = java.util.Arrays.copyOf(work, math.max(work.length * 2, dataEnd + bs))
    var got = 0
    // one read attempt per refill; a short read just means the next frame may still be Partial → refill again.
    got = read(work, dataEnd, work.length - dataEnd)
    if got <= 0 then eof = true else dataEnd += got

  def nextChunk(): Boolean =
    // A streaming framer treats the WHOLE stream as one logical "chunk" of records — nextRecord
    // drives refills internally. So nextChunk is true once (there is data to try), false only once
    // fully drained. (The two-level contract still holds: when nextRecord returns false at
    // end-of-data, nextChunk returns false too.)
    !(eof && pos >= dataEnd)

  def nextRecord(): Boolean =
    var verdict = frame(work, pos, dataEnd, eof)
    // keep reading blocks while the buffer holds only an unfinished record or is empty-but-not-EOF.
    while (verdict match { case Framed.Partial(_) | Framed.NeedMore => true; case _ => false }) do
      refill()
      verdict = frame(work, pos, dataEnd, eof)
    verdict match
      case Framed.Record(s, e) => recStart = s; recEnd = e; pos = e + 1; true // advance past the terminator
      case Framed.End          => false
      case _                   => false // unreachable (Partial/NeedMore loop above until Record or End)

  override def close(): Unit =
    if !closed then { closed = true; doClose() }
