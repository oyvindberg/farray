# Fuse → Streaming: roadmap & design notes

*Status: design captured after the grouping/sorting/constant-memory discussions. This is the north star for
turning the `fuse` compile-time pipeline into a constant-memory streaming engine — "fs2's guarantees, but
statically fused, with structured concurrency instead of an effect monad."*

---

## 0. The thesis

The headline we're chasing is **constant memory**, not just speed. `fuse` already gives unbeaten throughput
for in-memory `FArray` work (one loop, no closures, no boxing — see `docs/inside-the-fusion-macro.md`). The
next frontier is the property fs2 is known for: process arbitrarily large / unbounded input in **O(1)
working set**, with proper short-circuiting and resource safety. Effect composition (`IO`) is explicitly *not*
the draw here — after virtual threads / structured concurrency (ox), the interesting part of fs2 is the
**streaming + constant-memory + concurrency** core, not the monadic effect plumbing.

So the target identity:

> **A compile-time-fused streaming library: constant-memory by construction, one fused loop over chunks,
> UNBOXED end to end, with ox-style structured concurrency at the boundaries — not an `IO`-monad runtime.**

**Unboxed is a categorical differentiator vs fs2.** fs2 boxes every element: `Chunk[A]`, the `Pull`/`Stream`
functional machinery, and the `IO` interpreter are all generic, so a stream of `Int`/`Long`/`Double` carries
`java.lang.Integer`s and pays allocation + cache + GC for each. A fuse-based stream keeps primitives in
`int[]`/`long[]`/`double[]` chunks with `iadd` in the body and zero `valueOf` (already proven for the in-memory
case — see the boxing javap proofs). For numeric streaming — telemetry, time-series, sensor/market data, ETL
over primitive columns — that's not a constant factor; it's the difference between a GC-bound and a
memory-bandwidth-bound program. **Constant memory + unboxed** together is the headline no streaming library in
the Scala ecosystem currently offers.

---

## 1. Where fuse is today (the foundation)

- **Model:** a Scala 3 macro reads the whole `xs.fuse.…` chain off the typed AST and emits ONE specialized
  `while` loop; lambdas beta-reduced in (no `Function1`), unboxed over Int/Long/Double + reference kinds.
- **Stages:** map, flatMap, filter/filterNot/withFilter, collect (PF match inlined), take, drop, takeWhile,
  dropWhile, distinct/distinctBy, slice, flatten, zipWithIndex, zip, map2, scanLeft, tapEach.
- **Terminals (~45):** run, foreach, foldLeft, count(/p), find, exists, forall, head(Option), reductions
  (sum/product/fold/reduce/min/max/minBy/maxBy + Option), last(Option), to{List,Vector,Set,Map,Array,Seq},
  mkString, to(factory), indexWhere/indexOf, collectFirst, contains, groupBy, groupMapReduce, partition,
  span, unzip, grouped, sliding.
- **Optimizer (Layer B):** product decomposition (tuples / case classes / nested / generic) → DCE, sink
  (compute-for-survivors), deep CSE; lazy `zip` reads; allocation-free reductions/indexWhere/minBy; unboxed
  `Tuple2$mcXY$sp` materialization; collect-match extraction.
- **Validated:** fuzz vs `List`, no-blowup (HugeMethodLimit), benchmarks, golden snapshots, complex-lambda
  boundary, javap boxing proofs.
- **Constraints:** element kind ∈ {Int, Long, Double, <:AnyRef} (specialize-or-fail, anti-boxing); a local
  `case class` defined inside a lambda body is rejected with a clear message.

**Key structural fact for everything below:** every terminal is already an `(init-state-above-loop, step-per-
element, finalize)` shape. That is exactly the shape a *streaming fold* needs. The whole streaming plan is
"expose and compose that shape."

---

## 2. Streaming vs barriers (the conceptual core)

Operators split into two classes:

- **Streaming** — O(1) latency per element, fuse into one loop, short-circuit: map, filter, collect, take,
  drop, takeWhile, dropWhile, flatMap, zip, zipWithIndex, scanLeft, tapEach, distinct (seen-set, streaming),
  grouped (bounded buffer).
- **Barriers** — must consume all N before emitting: sorted/sortBy, groupBy, reverse; and the reductions
  (see all, emit O(1)).

Constant memory = stay in the streaming class as much as possible, and when a barrier is unavoidable, make it
*bounded* (heap of size n) rather than *full* (buffer of N).

The three high-leverage **user declarations** that collapse a group/sort problem (from the discussion):
1. **"aggregate only, not the rows"** → fold into per-key accumulators; O(G) memory, no row buffering, no sort.
2. **"input is already ordered by the key"** → grouping becomes streaming (emit on key change); O(largest run).
3. **"only the top-k"** → bounded heap; O(N log k) time, O(k) memory, no full sort.

The library's job is to *offer combinators that let the user declare these*, so the better plan is reachable.
The macro must never guess/reorder (that would be a miscompile for impure code — same reason guard-reorder
was dropped).

---

## 3. Near-term build queue (designed, ready)

### 3a. The buffer-slot + epilogue mechanism (enabling infra)
Mirror of `scanLeft`'s prologue: a `withGroupEpilogue` that runs after `loopOver` to flush trailing state,
guarded by `!done` so a satisfied downstream `take` doesn't get a spurious final chunk. Needed by everything
below.

### 3b. `groupAdjacentBy[K](key: A => K): Fuse[FArray[A]]`  — streaming group stage
- Assumes input clustered by key; emits a group each time the key changes; epilogue flushes the last run.
- State = current run buffer + current key (above loop). Memory **O(largest run)**, not O(N). Reference rows
  ⇒ no boxing. Composes (it's a stage, like flatMap but contracting), and **short-circuits** under `take`
  (`groupAdjacentBy(k).take(5)` reads only enough rows to form 5 groups).
- Contrast: `groupBy`/`groupMapReduce` are barriers (read all); `groupAdjacentBy` is the streaming counterpart
  unlocked by the "ordered by key" declaration.

### 3c. `foldAdjacentBy[K,B](key)(seed)(combine): Fuse[(K,B)]`  — O(1) streaming aggregate
- The "ordered + aggregate" sweet spot: no buffer, state = (curKey, acc, started); emit `(key, acc)` per run.
- O(1) memory, no hashmap, no boxing of primitive keys, streaming, short-circuits. Strictly dominates
  `groupMapReduce` *when the input is ordered*.

### 3d. **Nested fusion** — `groupAdjacentBy(key)(reduce: Fuse[A] => B): Fuse[(K, B)]`  ← the big one
- Per-group work supplied as a **fused sub-pipeline** `Fuse[A] => B` over the run's rows. When `reduce` is a
  reduction (`_.map(f).sum`, `_.count`, `_.min`, `_.take(3).run`), the group's rows are **never materialized**
  — the inner fold runs inline as rows stream past. **O(1) memory per group, zero per-group allocation.**
  `_.run`/`_.toList` recovers the materializing behavior only when you actually want the rows.
- Implementation = **nested staging**: the inner `Fuse[A]` has no concrete `FArray` source; its source is
  "the current run's elements," delivered one at a time by the outer loop. So the inner pipeline must compile
  to `(init, step, finalize)` — a push-driven fold — with its "above the loop" state becoming *per-run* state
  reset at each key change. This reuses the existing terminal shape (§1) recursively, in **push** mode.
- **This is the bridge to streaming sources (§4): the same push-consumer compilation needed for nested fusion
  is what lets the outer source itself be an incremental stream rather than an array.**

### 3e. Bounded-heap terminals — `topN(n)`, `smallest(n)`, `largest(n)`, `takeRight(n)`
- The algorithmic prize for "sort then take a few": O(N log n) time, **O(n) memory**, streaming, short-circuit
  — vs `sorted.take(n)` = O(N log N) + O(N) memory. Dedicated terminals (the macro won't auto-derive a heap
  from `sorted.take(n)` — that's an operator rewrite, out of scope).
- `takeRight(n)` = ring buffer, O(n) memory, vs `reverse.take(n)` (a full barrier).

### 3f. Promote `grouped`/`sliding` from terminals to composable stages
- Currently terminals (materialize `FArray[FArray[A]]`). As stages they fuse chunk-processing into the same
  loop. Reuse the §3a buffer+epilogue infra. `grouped` is a *bounded* barrier (buffers k), so it still
  short-circuits under `take`.

---

## 4. The streaming leap: incremental sources & constant memory over unbounded input

Today the source is a concrete `FArray` (an array the loop indexes). Real streaming needs **incremental
sources** — iterator / `InputStream` / file / socket / generator — consumed in **constant memory**.

Two consumption models, and fuse already leans one way:
- **Pull** (consumer asks for next): fuse's indexed array loop is already pull-shaped. Generalize "read
  `a(i)`" to "pull next element / next chunk."
- **Push** (source drives): for I/O, the source pushes chunks; the fused pipeline is a push-consumer
  `(init, step, finalize)` — *exactly the shape from §3d*.

Design directions to evaluate (fs2 comparison feeds this):
- **Chunked streaming.** fs2's `Chunk` ≈ our `FArray`. Process **chunk-at-a-time**: pull a chunk, run the
  fused per-element loop over it, carry fold/scan state across chunks. This keeps the inner loop tight
  (fused, unboxed) while the outer driver advances chunks in constant memory. The HugeMethodLimit constraint
  is naturally respected (the per-chunk body is the same bounded fused loop).
- **A `Source[A]` abstraction** the macro can stage over: minimally `pullChunk(): FArray[A] | end`. The fused
  pipeline becomes a chunk consumer; terminals fold across chunks. Sources: `fromIterator`, `fromInputStream`
  / lines, `unfold`, `repeat`, `iterate`, plus the existing in-memory `FArray`.
- **Resource safety** (bracket/acquire-release) for file/socket sources — open on first pull, close on
  termination/short-circuit/exception. fs2's `Resource`/`bracket` is the reference; ox's structured scopes are
  the post-IO equivalent.
- **Back-pressure** is implicit in pull (the consumer paces the source). Worth confirming against fs2.

The unifying observation: **§3d (nested fusion, push-fold) and §4 (streaming sources, chunk-fold) are the same
machinery** — "compile a fused pipeline to `(init, step, finalize)` and drive it from an external element
source." Building one builds the other. That is the single most important architectural investment.

---

## 5. Structured concurrency (ox-style), not an effect monad

fs2's concurrency (`merge`, `parJoin`, `concurrently`, `parEvalMap`, `broadcast`, `balance`) is built on `IO`
+ a runtime. Post-virtual-threads, the more interesting substrate is **ox / structured concurrency** over
direct-style code:
- **`parEvalMap(n)` / `mapAsync`** — bounded-concurrency stage: run `f: A => B` on up to n elements in
  parallel (a scoped pool of virtual threads), preserve (or not) order. Constant memory = bounded in-flight.
- **`merge` / `parJoin`** — interleave several streams; structured scope owns the producers.
- **`broadcast` / `balance`** — fan-out / work-steal across consumers.
- **Boundaries, not pervasiveness:** keep the *pure spans* statically fused (zero-alloc tight loop); introduce
  a concurrency/effect boundary only at `parEvalMap`-style stages, where a scope + queue is unavoidable. (This
  is the same "fuse the pure segments, suspend at effects" conclusion from the effects discussion — but with
  ox scopes instead of `IO` binds, so no monadic runtime.)

Research question: what's the minimal concurrency vocabulary that (a) preserves constant memory (bounded
in-flight + chunking), (b) is expressible with ox scopes + virtual threads, (c) composes with the fused
pure stages without forcing the whole pipeline into a runtime?

---

## 6. Open tensions / risks

- **Static vs dynamic.** Fuse knows the whole pipeline at compile time; fs2 composes streams at runtime. To
  stream we keep the pipeline static but let the *source* and *concurrency boundaries* be dynamic. Need to
  confirm nothing in the combinator set requires runtime pipeline composition we can't stage.
- **HugeMethodLimit.** Long fused chains over chunks — the per-chunk body must stay under ~8000 bytecodes.
  Chunking + the existing no-blowup regression test cover this; very long pipelines may need a planned split.
- **Cross-chunk state.** scan/fold/distinct/groupAdjacent state must persist across chunk boundaries — the
  §3a above-loop-state machinery generalizes to "above the chunk-driver loop."
- **Purity contract** still holds: streaming stages may run fewer times under short-circuit; effectful stages
  go through the concurrency boundary, not the fused span.

---

## 7. Sequenced plan

1. **Buffer-slot + epilogue infra** (§3a) — small, unblocks the rest.
2. **`foldAdjacentBy`** (§3c) — O(1) streaming aggregate, low risk, immediately useful.
3. **`groupAdjacentBy` (materializing form)** (§3b) — streaming group stage.
4. **Nested fusion / push-fold** (§3d) — the key architectural piece; `groupAdjacentBy(key)(Fuse[A]=>B)`.
   *This is the same machinery as §4, so prototype it with §4 in mind.*
5. **Bounded-heap terminals** (§3e) — topN / takeRight.
6. **Incremental `Source[A]` + chunk-driver** (§4) — the streaming leap; constant memory over unbounded input.
7. **Resource safety** (§4) — bracket/scope for file/socket sources.
8. **Structured-concurrency stages** (§5) — `parEvalMap`, `merge`, informed by the fs2 research.

`grouped`/`sliding`-as-stages (§3f) slot in opportunistically once §3a exists.

---

## 8. Inputs to this plan
- `docs/inside-the-fusion-macro.md` — how the current fusion works (hygiene, predicates, applicative core,
  zip, nesting, dynamism, collect).
- `docs/fused-pipeline-design.md` — original design.
- `docs/fs2-research.md` — **(to be produced by the fs2 research pass)**: fs2 combinator inventory categorized
  by memory behavior, Chunk/Pull/Stream architecture, concurrency model, and the ox-based reimagining. Feeds
  §4 and §5 concretely.
