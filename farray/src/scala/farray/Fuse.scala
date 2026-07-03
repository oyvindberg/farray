package farray

/** Fused-pipeline builder. `xs.fuse.map(f).filter(p).take(k).run` compiles to ONE unboxed traversal of `xs` producing ONE output collection — no intermediate
  * `FArray` per stage, no per-element boxing or virtual calls. See `docs/fused-pipeline-design.md`.
  *
  * The combinator methods below are MARKERS: their bodies never run. The terminal methods (`run`, …) are `inline` macros that read the whole `xs.fuse.…` chain
  * off the AST, peel the stage list + the (inlined) lambdas, and emit the fused loop. The `Fuse` wrapper itself is elided by the macro — only `xs` and the
  * lambda bodies survive into the generated code.
  *
  * `S` is the pipeline's SOURCE SHAPE — a phantom type naming where elements come from, e.g. [[Chunks]] (an in-memory `FArray` or a chunked [[Source]]) or
  * a decoder module's shape (`farray.json.Ndjson`). The stage markers are shape-generic; the TERMINALS are not members of this class at all. They are inline
  * extension methods provided by the shape's [[FuseLowering]] given (found in the shape companion's implicit scope), so each source shape brings exactly the
  * terminals it supports, lowered by its own macro: an unsupported terminal is "not a member", at compile time, and a decoder module plugs in as an ordinary
  * typeclass instance — no registry, no reflection.
  *
  * Semantics — assume PURE stage functions. A fused pipeline is lazy/short-circuiting: `take`, `find`, `head`, `exists`/`forall` stop the traversal as soon as
  * the answer is known, so a stage function (incl. a `flatMap`'s) may run FEWER times than in the equivalent strict `List` pipeline (e.g.
  * `xs.fuse.flatMap(f).take(3)` invokes `f` only until 3 elements are produced). Likewise, when a `map` produces a tuple of independent columns and a later
  * `filter` uses only some of them, the OTHER columns are computed only for elements that pass the filter — "compute-for-survivors" (e.g.
  * `xs.fuse.map(x => (cheap(x), expensive(x))).filter(_._1 > 0).map(_._2)` runs `expensive` only on survivors, and never allocates the tuple). For a pure `f`
  * the result is identical; with a side-effecting or throwing `f` the observable behavior (call count, whether it throws) can differ. The element type must be
  * Int/Long/Double or a reference type (`<: AnyRef`) — a primitive-backed FArray widened to `Any`/`AnyVal` is a compile error, not a silent miscompile.
  */
// `base` is `AnyRef` (not `FBase`) so a non-array source (e.g. a byte-backed JSON NDJSON source) can flow
// through the same `Fuse` surface; the terminal macro reads the source's static type at the base case and
// lowers accordingly (an `FBase`/`Source` → the loops; a byte source → the shape's decoder).
final class Fuse[+A, S](private[farray] val base: AnyRef):
  // ---- stage markers (bodies irrelevant; the macro reads these calls off the AST) ----
  def map[B](f: A => B): Fuse[B, S] = this.asInstanceOf[Fuse[B, S]]
  def flatMap[B](f: A => FArray[B]): Fuse[B, S] = this.asInstanceOf[Fuse[B, S]]
  def filter(p: A => Boolean): Fuse[A, S] = this

  /** like `filter`; also lets a for-comprehension guard (`for (x <- xs.fuse if p(x)) yield …`) fuse. */
  def withFilter(p: A => Boolean): Fuse[A, S] = this
  def filterNot(p: A => Boolean): Fuse[A, S] = this

  /** keep elements that match `pf`, mapping each through it — filter + map + pattern-match fused in one pass. */
  def collect[B](pf: PartialFunction[A, B]): Fuse[B, S] = this.asInstanceOf[Fuse[B, S]]
  def take(n: Int): Fuse[A, S] = this
  def drop(n: Int): Fuse[A, S] = this

  /** the LAST `n` elements, in order — a ring buffer (O(n) memory), one pass, no `reverse`. Nothing is emitted until the stream ends, so unlike `take` this
    * doesn't short-circuit the source; a downstream `take` does.
    */
  def takeRight(n: Int): Fuse[A, S] = this

  /** emit elements until `p` first fails, then stop the whole traversal (short-circuits like `take`). */
  def takeWhile(p: A => Boolean): Fuse[A, S] = this

  /** skip the leading run of elements matching `p`, then emit all the rest. */
  def dropWhile(p: A => Boolean): Fuse[A, S] = this

  /** keep only the first occurrence of each element (by `==`/hashCode). */
  def distinct: Fuse[A, S] = this

  /** keep only the first element for each distinct key `f(a)`. */
  def distinctBy[K](f: A => K): Fuse[A, S] = this

  /** running fold: emit `z`, then each successive `op(acc, a)` — yields one more element than the input. */
  def scanLeft[B](z: B)(op: (B, A) => B): Fuse[B, S] = this.asInstanceOf[Fuse[B, S]]

  /** Streaming group-aggregate for input ALREADY CLUSTERED by `key`: emit `(k, acc)` once per maximal run of equal keys, where `acc` starts at
    * `combine(seed, firstOfRun)` and folds the run with `combine`. O(1) memory (state = curKey, acc, started), no buffer, no hashmap, short-circuits under
    * `take` — the ordered-input counterpart of `groupMapReduce`. The "already clustered by key" precondition is the USER'S declaration; on unordered input the
    * result is per-run, not per-key (documented, not detected).
    */
  def foldAdjacentBy[K, B](key: A => K)(seed: B)(combine: (B, A) => B): Fuse[(K, B), S] = this.asInstanceOf[Fuse[(K, B), S]]

  /** Streaming GROUP stage for input ALREADY CLUSTERED by `key`: emit each maximal run of equal keys as its own `FArray[A]` (the rows, in order). O(largest
    * run) memory — buffers one run at a time, not all N — no hashmap, short-circuits under `take`. The materializing counterpart of `foldAdjacentBy` (use that
    * when you only need a per-run aggregate and never the rows). Same "clustered by key" user precondition.
    */
  def groupAdjacentBy[K](key: A => K): Fuse[FArray[A], S] = this.asInstanceOf[Fuse[FArray[A], S]]

  /** NESTED FUSION — the spec's headline: for input ALREADY CLUSTERED by `key`, reduce each run with a FUSED sub-pipeline and emit `(k, result)` per run.
    * `prep` is the per-group stages (map/filter/collect/take/…) over the run's rows; `agg` is the per-group aggregate (`Agg.sum`/`count`/`min`/`fold`/…). When
    * the aggregate is a fold, the run's rows are NEVER materialized — the inner fold runs inline as rows stream past: O(1) memory per group, zero per-group
    * allocation. Same "clustered by key" precondition. Example: {{{src.fuse.groupAdjacentReduceBy(_.day)(_.map(_.amount))(Agg.sum(identity))}}}
    *
    * (Spelled `(prep)(agg)` rather than `(reduce: Fuse[A] => B)` because an inner inline terminal like `.sum` expands before this macro reads the lambda;
    * `prep` is plain stage markers and `agg` is a macro-read value.)
    */
  inline def groupAdjacentReduceBy[K, B, R](inline key: A => K)(inline prep: Fuse[A, S] => Fuse[B, S])(inline agg: Agg[B, R]): Fuse[(K, R), S] =
    ${ FuseMacro.groupReduceStageImpl[A, S, K, B, R]('this, 'key, 'prep, 'agg) }

  /** Internal non-inline marker the `.run`/agg macro reads off the AST for nested fusion. `agg` here is an `AggRaw.*` (the non-compileTimeOnly twin) —
    * `groupReduceStageImpl` already consumed the user's `Agg.*`. Not for direct use.
    */
  def groupAdjacentReduceByMarker[K, B, R](key: A => K)(prep: Fuse[A, S] => Fuse[B, S])(agg: Agg[B, R]): Fuse[(K, R), S] =
    this.asInstanceOf[Fuse[(K, R), S]]

  /** run `f` for its side effect on each surviving element, passing the element through unchanged. */
  def tapEach(f: A => Unit): Fuse[A, S] = this

  /** pair each element with its position in the stream at this point (post-upstream-filtering). */
  def zipWithIndex: Fuse[(A, Int), S] = this.asInstanceOf[Fuse[(A, Int), S]]

  /** lock-step with another source: pair element k of this pipeline with `that(k)`; stops at the shorter. */
  def zip[B](that: FArray[B]): Fuse[(A, B), S] = this.asInstanceOf[Fuse[(A, B), S]]

  /** lock-step combine with another source via `f` (like `zip(that).map(f)` but never builds the pair). */
  def map2[B, C](that: FArray[B])(f: (A, B) => C): Fuse[C, S] = this.asInstanceOf[Fuse[C, S]]

  // ---- derived stages (inline sugar over the markers above; still one fused pass) ----
  /** elements in index range `[from, until)` — `drop(from).take(until - from)`. */
  inline def slice(from: Int, until: Int): Fuse[A, S] = drop(from).take(until - from)

  /** concatenate a pipeline of `FArray`s — `flatMap(identity)`. The evidence only constrains `A`; the lambda casts rather than applying it, so the parsed stage
    * carries no reference to the evidence proxy binding.
    */
  inline def flatten[B](using A <:< FArray[B]): Fuse[B, S] = flatMap(a => a.asInstanceOf[FArray[B]])

  /** Fixed-size chunks (the last may be shorter) — a composable STAGE: `grouped(n)` emits an `FArray[A]` every `n` elements, so it fuses into the one pass AND
    * short-circuits under a downstream `take`. `xs.fuse.grouped(n).run` gives the same `FArray[FArray[A]]` the old terminal did, but you can now keep
    * transforming the chunks. O(n) memory.
    */
  def grouped(n: Int): Fuse[FArray[A], S] = this.asInstanceOf[Fuse[FArray[A], S]]
