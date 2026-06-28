package farray

/** The decomposed-source contract the fusion engine drives — a **record-framed byte cursor**. Engine-neutral: any
  * byte source that can hand back complete records (NDJSON today, columnar/protobuf/etc. in future) implements it,
  * and the engine lowers it to a per-record projection scanner via the decomposed-source bridge (see
  * `farray.RecordDecoder`). The engine asks for one thing: "hand me the byte range of the next COMPLETE record, in
  * constant memory, and tell me when records run out." Everything column-specific — which keys are live, skip-vs-
  * decode, predicate early-out — is the engine's/decoder's job; the source owns only framing + I/O.
  *
  * Driving protocol (a two-level loop the engine emits):
  * {{{
  *   try
  *     while src.nextChunk() do                 // advance to the next backing buffer (false at EOF)
  *       while src.nextRecord() do              // frame the next record within it (false when the buffer is drained)
  *         decode(src.buf, src.recordStart, src.recordEnd)
  *   finally src.close()
  * }}}
  * (The real emitted loops are `done`-gated at BOTH levels, so a downstream `take`/`find` stops pulling.)
  *
  * '''Lifetime / constant memory.''' `buf` may be a reused/recycled array; a `(recordStart, recordEnd)` range is
  * valid only until the next `nextChunk()`. A decoder's String columns are lazy `(start,len)` slices forced to a real
  * `String` ON READ, before any chunk swap — so a survivor's projected string never dangles. Folding terminals never
  * escape a record. Working set is therefore O(one chunk + the largest single record), independent of stream length.
  */
trait ByteRecordSource:
  /** Advance to the next backing buffer. `false` once the stream is exhausted (idempotent after that). An in-memory
    * implementor returns `true` exactly once; a streaming implementor reads the next block. After this returns true,
    * `buf` and the `recordStart`/`recordEnd` cursor describe the new chunk and `nextRecord()` is ready to frame it.
    */
  def nextChunk(): Boolean

  /** The current chunk's backing bytes. Valid only until the next `nextChunk()`. */
  def buf: Array[Byte]

  /** Frame the next COMPLETE record within the current chunk: sets `recordStart`/`recordEnd` and returns `true`, or
    * returns `false` when the chunk is drained (the driver then tries `nextChunk()`). A record that straddles a chunk
    * boundary is the framer's problem, NOT the engine's — the engine only ever sees complete, contiguous frames.
    */
  def nextRecord(): Boolean

  /** Start (inclusive) of the current framed record in `buf`. */
  def recordStart: Int

  /** End (exclusive) of the current framed record in `buf` — the record terminator or chunk end. */
  def recordEnd: Int

  /** Release any held resource (file handle, socket). Default no-op; the driver always wraps the loop in
    * `try/finally close()`, so it fires on exhaustion, short-circuit, AND exception. Idempotent.
    */
  def close(): Unit = ()
