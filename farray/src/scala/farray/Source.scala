package farray

/** A pull-based, **chunk-granular** source the fused pipeline can stream over in constant memory.
  *
  * The unit of transfer is an [[FArray]] chunk, not an element: one virtual `pullChunk` call amortizes over a whole `int[]`/`long[]`/`double[]`/`Object[]`
  * leaf, so the hot path stays the same tight unboxed element loop the in-memory `fuse` already emits — no per-element virtual call, no boxed sentinel. The
  * macro drives a `Source` by wrapping its existing element loop in an outer chunk loop (see the streaming spec, §3).
  *
  * '''Chunk lifetime is intra-chunk.''' A source MAY hand back the same backing array on the next pull (e.g. an `InputStream` source reusing one read buffer).
  * So a chunk returned by `pullChunk` is only valid until the NEXT `pullChunk`. Primitive elements are safe (copied by value into stage slots / the output); a
  * reference element or byte-slice that escapes per-chunk scope must be materialized before the next pull — the same force-before-release rule the JSON path
  * already applies. Folding terminals (sum/count/fold/agg) never escape a chunk, so they are unconditionally constant-memory.
  *
  * This is a SEPARATE source shape alongside the in-memory `FBase` and the byte-sourced `NdjsonSource`; the downstream optimizer (stages, DCE/sink/CSE,
  * terminals, `done`) is identical across all three — only the source read differs.
  */
trait Source[+A]:
  /** The next chunk of elements, or [[Source.End]] once exhausted. After end is returned, `pullChunk` must keep returning end (idempotent). The returned chunk
    * is valid only until the next `pullChunk` call.
    */
  def pullChunk(): FArray[A] | Source.End

object Source:
  /** The end-of-stream sentinel. A union member (`FArray[A] | End`) so no `Option`/boxing on the hot pull. */
  sealed trait End
  case object End extends End

  /** the default chunk size for the iterator/generator constructors — large enough that the per-chunk virtual `pullChunk` call and leaf allocation amortize,
    * small enough to stay well within constant memory.
    */
  inline val DefaultChunkSize = 4096

  /** An already-realized `FArray` as a one-chunk source — the degenerate (in-memory) case. Lets the in-memory path be a `Source` too, so the chunk driver
    * subsumes it; we keep the dedicated array loop for now (it is hot and proven) but this makes the collapse possible later without changing callers.
    */
  def fromFArray[A](xs: FArray[A]): Source[A] = new Source[A]:
    private var pending: FArray[A] | End = if xs.isEmpty then End else xs
    def pullChunk(): FArray[A] | End =
      val r = pending; pending = End; r

  // A `Source[A]` is driven over CONCRETE element kinds (the macro reads each chunk via `${kind}At`), so a chunk
  // must be a kind-correct leaf (`LongArr`, not a boxed `RefArr` of `java.lang.Long`s). But these constructors are
  // generic in `A`, and FArray's builders are `inline`/specialize-or-fail. The bridge: each `inline` constructor
  // passes the kind-dispatched `FArray.fromBoxedArray[A]` (which the inline expands, specialized at the concrete-`A`
  // call site) plus a `fill` closure into `FromChunkBuilder`. The source fills an `Object[]` and unboxes once per
  // chunk (amortized over the whole chunk), zero per-element virtual dispatch downstream.

  /** Public because the inline constructors below reference it: a `Source` that, per pull, fills an `Object[cs]` via `fill` (returns the count, ≤ 0 to end) and
    * wraps it to a kind-correct leaf via `mk`.
    */
  final class FromChunkBuilder[A](cs: Int, mk: Array[Object] => FArray[A], fill: Array[Object] => Int) extends Source[A]:
    def pullChunk(): FArray[A] | End =
      val buf = new Array[Object](cs)
      val n = fill(buf)
      if n <= 0 then End
      else mk(if n == cs then buf else java.util.Arrays.copyOf(buf, n))

  /** Chunk an `Iterator` into leaves of `chunkSize`. Constant memory: one chunk live at a time, the iterator paced by the consumer (pull = back-pressure). The
    * classic adapter from any `Iterable`/`Iterator` source.
    */
  inline def fromIterator[A](it: Iterator[A], chunkSize: Int = DefaultChunkSize): Source[A] =
    val cs = math.max(1, chunkSize)
    new FromChunkBuilder[A](
      cs,
      buf => FArray.fromBoxedArray[A](buf),
      buf => {
        var n = 0; while n < cs && it.hasNext do { buf(n) = it.next().asInstanceOf[Object]; n += 1 }; n
      }
    )
  inline def fromIterable[A](xs: Iterable[A], chunkSize: Int = DefaultChunkSize): Source[A] = fromIterator(xs.iterator, chunkSize)

  /** The general generator: produce a chunk at a time from an evolving state. `step(s)` returns either the next `(chunk, nextState)` or `None` to end. This is
    * the real generator primitive — `unfold`, `iterate`, `repeat`, and `range` are sugar over it (a chunk-of-one generator wastes a leaf per element, so prefer
    * producing whole chunks). The chunk `step` hands back is already an `FArray[A]`, so this one needs no inline bridge — the caller built the leaf.
    */
  def unfoldChunk[S, A](init: S)(step: S => Option[(FArray[A], S)]): Source[A] = new Source[A]:
    private var s: S = init
    private var ended = false
    def pullChunk(): FArray[A] | End =
      if ended then End
      else
        step(s) match
          case Some((chunk, next)) => s = next; chunk
          case None                => ended = true; End

  /** Element-at-a-time unfold (chunked internally to `chunkSize`): `step(s)` yields the next `(elem, nextState)` or `None`. Builds the leaf via
    * `FArray.fromBoxedArray` so a primitive element stays unboxed.
    */
  inline def unfold[S, A](init: S, chunkSize: Int = DefaultChunkSize)(step: S => Option[(A, S)]): Source[A] =
    val cs = math.max(1, chunkSize)
    var s: S = init; var alive = true
    new FromChunkBuilder[A](
      cs,
      buf => FArray.fromBoxedArray[A](buf),
      buf => {
        var n = 0
        while n < cs && alive do
          step(s) match
            case Some((a, ns)) => buf(n) = a.asInstanceOf[Object]; s = ns; n += 1
            case None          => alive = false
        n
      }
    )

  /** `start, f(start), f(f(start)), …` — an UNBOUNDED source. Pair with `take`/`takeWhile` to bound it. */
  inline def iterate[A](start: A, chunkSize: Int = DefaultChunkSize)(inline f: A => A): Source[A] =
    unfold[A, A](start, chunkSize)(a => Some((a, f(a))))

  /** `value` repeated forever — UNBOUNDED; bound with `take`. */
  inline def repeat[A](value: A, chunkSize: Int = DefaultChunkSize): Source[A] =
    unfold[Unit, A]((), chunkSize)(_ => Some((value, ())))

  /** an int range as a streaming source (chunked), [start, end) by step. */
  inline def range(start: Int, end: Int, step: Int = 1, chunkSize: Int = DefaultChunkSize): Source[Int] =
    require(step != 0, "range step cannot be 0")
    unfold[Int, Int](start, chunkSize)(i => if (if step > 0 then i < end else i > end) then Some((i, i + step)) else None)

  /** Enter the fused pipeline over a streaming source: `src.fuse.filter(_ > 0).map(_ * 2).sum` runs in constant memory (one chunk live at a time). Identical
    * surface to `FArray#fuse` — the macro wraps the element loop in an outer chunk-pull loop.
    */
  extension [A](src: Source[A]) inline def fuse: Fuse[A] = new Fuse[A](src)
