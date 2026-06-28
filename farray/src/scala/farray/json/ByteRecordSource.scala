package farray.json

/** The decomposed-source contract the JSON macro drives — a **record-framed byte cursor**.
  *
  * This is the seam between the byte-level source and the macro's projection scan-pass (`loopOverJson`/`jsonRecord` in `FuseMacro`). The macro asks for one
  * thing: "hand me the byte range of the next COMPLETE record, in constant memory, and tell me when records run out." Everything column-specific — which keys
  * are live, skip-vs-decode, predicate early-out — stays in the macro (it owns the backwards live-set); the source owns only framing + I/O.
  *
  * Driving protocol (a two-level loop the macro emits):
  * {{{
  *   try
  *     while src.nextChunk() do                 // advance to the next backing buffer (false at EOF)
  *       while src.nextRecord() do              // frame the next record within it (false when the buffer is drained)
  *         scan(src.buf, src.recordStart, src.recordEnd)   // jsonRecord — untouched, already (buf,start,end)-shaped
  *   finally src.close()
  * }}}
  * (The real emitted loops are `done`-gated at BOTH levels, so a downstream `take`/`find` stops pulling.)
  *
  * '''Lifetime / constant memory.''' `buf` may be a reused/recycled array; a `(recordStart, recordEnd)` range is valid only until the next `nextChunk()`. The
  * macro's string columns are lazy `(start,len)` slices that `decodeLatin1` forces to a real `String` ON READ, before any chunk swap — so a survivor's
  * projected string never dangles. Folding terminals never escape a record. Working set is therefore O(one chunk + the largest single record), independent of
  * stream length.
  */
trait ByteRecordSource:
  /** Advance to the next backing buffer. `false` once the stream is exhausted (idempotent after that). The in-memory implementor returns `true` exactly once; a
    * streaming implementor reads the next block. After this returns true, `buf` and the `recordStart`/`recordEnd` cursor describe the new chunk and
    * `nextRecord()` is ready to frame it.
    */
  def nextChunk(): Boolean

  /** The current chunk's backing bytes. Valid only until the next `nextChunk()`. */
  def buf: Array[Byte]

  /** Frame the next COMPLETE record within the current chunk: sets `recordStart`/`recordEnd` and returns `true`, or returns `false` when the chunk is drained
    * (the driver then tries `nextChunk()`). A record that straddles a chunk boundary is the framer's problem, NOT the macro's — the macro only ever sees
    * complete, contiguous frames.
    */
  def nextRecord(): Boolean

  /** Start (inclusive) of the current framed record in `buf`. */
  def recordStart: Int

  /** End (exclusive) of the current framed record in `buf` — the `\n` terminator or chunk end. */
  def recordEnd: Int

  /** Release any held resource (file handle, socket). Default no-op; the driver always wraps the loop in `try/finally close()`, so it fires on exhaustion,
    * short-circuit, AND exception. Idempotent.
    */
  def close(): Unit = ()

/** The result of scanning the working buffer for the next record — the typed source↔framer seam (idea 1). Making "unfinished record" a FIRST-CLASS value
  * (`Partial`) instead of an offset to remember is what lets the boundary logic live in exactly one place (`Framer`): the byte reader below it knows nothing
  * about records, the macro above it never sees a `Partial`.
  */
private[farray] enum Framed:
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
private[farray] class Framer(read: (Array[Byte], Int, Int) => Int, blockSize: Int, doClose: () => Unit) extends ByteRecordSource:
  private val bs: Int = math.max(64, blockSize)
  private var work: Array[Byte] = new Array[Byte](bs * 2) // carry tail + one fresh block
  private var dataEnd: Int = 0 // bytes valid in `work` are [0, dataEnd)
  private var pos: Int = 0 // next unframed byte
  private var recStart: Int = 0
  private var recEnd: Int = 0
  private var eof: Boolean = false // the reader returned -1
  private var closed: Boolean = false

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
