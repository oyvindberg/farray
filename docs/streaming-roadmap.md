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

## 0.5 Research synthesis (fs2 + ox — see `docs/fs2-research.md`, `docs/ox-research.md`)

Two independent research passes (fs2 verified against a v3.13 clone; ox against v1.0.5 source + JDK 25)
**converged on the same architecture**. The load-bearing conclusions:

1. **The keystone is one abstraction + one loop change.** A minimal pull source —
   `trait Source[+A] { def pullChunk(): FArray[A] | Source.End }` (the unboxed analogue of fs2's
   `Pull.uncons`, chunk-granular so the one virtual call amortizes over a whole `int[]`) — plus a **chunk
   driver** that wraps fuse's existing element loop in an outer chunk loop and hoists stage/terminal state one
   level further out. **This converts the entire existing fused surface (every CONST + SINK-O(1) terminal)
   into constant-memory streaming in a single move** — the existing `(init, step, finalize)` terminals need
   *no change*. fs2's `Chunk` ≈ our `FArray` exactly (`ArraySlice`≈`IntArr`, `Chunk.Queue`≈`Concat`).

2. **Constant memory = pull + chunk + discard.** Live data at any instant = `acc` + stage slots + **one
   chunk's backing array**, independent of stream length. A 100 GB file folds in O(chunkSize). `scanLeft`'s
   "run the prologue once before the loop" already generalizes to "before the chunk loop."

3. **Unboxed is our moat, and it survives concurrency** — *if* we pass **`FArray` chunks across thread seams,
   not elements**. fs2 (`Chunk.map` → `new Array[Any]`, `apply(i): Object`) and ox (`Channel[Int]`/`Flow[Int]`
   both box) box at the element level; a `BlockingQueue[FArray[Int]]` moves one pointer per chunk (leaf stays
   `int[]`), so zero per-element boxing even at a `parMap` seam.

4. **Concurrency: build on the JDK, do NOT depend on ox.** The structured-concurrency core we need is ~250
   lines on **final** Loom primitives (`Thread.ofVirtual` / `newVirtualThreadPerTaskExecutor` +
   `ArrayBlockingQueue` + `Semaphore`). Reasons: `StructuredTaskScope` is **still preview in JDK 25** (5th
   preview, JEP 505 — shipping a preview API to library users is hostile); ox itself doesn't even use it (its
   own `ThreadHerd`), so "depend on ox to dodge JEP churn" is a non-reason; and ox's `Flow` is **push/callback
   + boxed**, the opposite shape from our pull/fused/unboxed engine — adopting it re-introduces the `Function1`
   indirection fuse exists to kill. Borrow two ox *designs* (not the dep): ordered/unordered `parEvalMap(n)`
   (`Semaphore` + in-order collector) and deterministic cancel-on-first-error (sibling interrupt-and-await).
   Reconsider à-la-carte `ox-core` later only for off-critical-path extras (adaptive retry, rate limiting,
   fair `select`). `ScopedValue` is final (JEP 506) and usable.

5. **The clean boundary (what won't statically fuse).** Runtime-decided *topology* (dynamic
   `flatMap`-of-runtime-`Source`, `parJoin`/stream-of-streams) and independently-paced concurrent producers
   (`merge`/`parEvalMap`) cannot be one static loop. The rule both reports reached: **a concurrency or
   dynamic-topology operator is a scope-opening SEAM** — fuse up to it, open a `supervised` scope, bounded
   chunk-channel across, fuse again on the other side. Producer and consumer are each fused loops; only the
   queue+threads at the seam are not. The `IO`/effect-monad layer is dropped entirely: `evalMap(f)` is just
   `map` doing I/O on a virtual thread.

The phased sequence in §7 below is updated to the **S1–S4** plan both reports recommend.

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

### 3d. **Nested fusion** — `groupAdjacentReduceBy(key)(prep)(agg): Fuse[(K, R)]`  ← the big one — DONE (v1)
- Per-group work supplied as a **fused sub-pipeline** over the run's rows. When the aggregate is a fold
  (`Agg.sum`/`count`/`min`/`max`/`avg`/`fold`/`reduce`/`minBy`/`maxBy`), the group's rows are **never
  materialized** — the inner fold runs inline as rows stream past. **O(1) memory per group, zero per-group
  allocation.** (Proven by the `neverMaterialized_eachRowVisitedOnce` test: each row touched exactly once.)
- **Spelling — `(prep: Fuse[A] => Fuse[B])(agg: Agg[B, R])`, NOT `(reduce: Fuse[A] => B)`.** The original
  `Fuse[A] => B` spelling is *not implementable*: a `reduce` body ending in an inline terminal (`_.map(f).sum`)
  has its `.sum` macro-expanded BEFORE the outer macro reads the lambda (Scala inlining is inside-out), and the
  inner `.sum`'s `parse` then fails on the lambda param as source (`Ident("g")`). Empirically confirmed. So the
  inner pipeline is split: `prep` = the stateless inner STAGES (plain non-inline markers — `map`/`filter`/
  `filterNot`/`collect` — that survive to the AST), and `agg` = the per-group aggregate as an `Agg.*` value.
- **The `@compileTimeOnly` hurdle + fix.** `Agg.*` is `@compileTimeOnly`, so passing it to a *non-inline* stage
  marker fires before the `.run` macro can consume it. Fix: `groupAdjacentReduceBy` is `inline`, splicing
  `groupReduceStageImpl`, which CONSUMES the `Agg.*` (rewriting it to the non-compileTimeOnly `AggRaw.*` twin,
  same method name → same `parseAgg1` shape) and re-emits a plain non-inline `groupAdjacentReduceByMarker` call
  that the `.run`/agg macro parses normally.
- Implementation = **nested staging**: the inner pipeline has no concrete `FArray` source; its source is "the
  current run's elements," delivered one at a time by the outer loop. The inner agg compiles to a per-run
  `(reset, step, finish)` (`innerAggState`, mirroring the folding `stateFor` cases) declared ABOVE the loop
  (persists across chunks), `reset` at each key change, `step` per row (threading the inner stages via
  `buildBody`), `finish` at the run boundary + epilogue. Emits `(k, R)` through the SAME decomposed-`Tup` path
  as `foldAdjacentBy` (so `.map(_._2)` never builds the tuple; all-primitive `(K,R)` stays unboxed).
- **v1 scope / follow-ups:** ONE folding aggregate per group (multi-agg = follow-up); inner stages are stateless
  only (`take`/`drop`/`takeWhile` inside a group need resettable per-run counters = follow-up); `topN`/`exists`/
  `find` as a per-group reduce (element-retaining / per-group short-circuit) error clearly. The `_.run`/`_.toList`
  materialize escape hatch stays as the existing `groupAdjacentBy(key)` + `.map`.
- **This was the bridge to streaming sources (§4): the same push-consumer compilation is what lets the outer
  source itself be an incremental stream rather than an array.** (Works across chunk boundaries — tested.)

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

### 4.5 The decomposed-source contract & the BACKWARDS live-set flow (the real abstraction)

`Source` is richer than `pullChunk(): FArray[A]` for sources whose fields are *expensive, order-dependent, and
lazily decodable* — JSON bytes, columnar (Parquet/Arrow), protobuf, a DB cursor. The JSON research pinned the
gap precisely: an in-memory column (`Sc(read)`) is a **pure, random-access, position-free** thunk the
optimizer reorders / hoists / skips freely — that's *why* DCE/sink/CSE work. A byte-sourced field violates
four of those assumptions, and the fix defines the abstraction:

1. **Not random-access, not independent.** `col_name.read` = "scan forward from the *shared* cursor to the
   `name` field" — stateful, order-dependent; its *position* in the emitted code is load-bearing.
2. **Two-phase shape.** Resolve by splitting one in-memory step into a **scan-pass** (one left-to-right walk
   filling per-field `var` *slots* — a `(start,len)` slice for strings, a decoded primitive for numbers) **then
   column-reads from the slots**. Random-access over the slots is restored ⇒ DCE/sink/CSE downstream run
   unchanged; a column's `Sc(read)` becomes "read slot K," and a separate generated block (the scan) fills slots.
3. **Cost is bisected.** A byte field has a *scan* half (eager + in-order — you must pass a field to reach the
   next; deferrable only as a whole-record **early-out** once the predicate fields are filled) and a
   *decode/alloc* half (defers past the filter exactly like the in-memory sink). The array model has only the
   second half. ⇒ the **position of predicate fields in the record matters** (predicate field last = scanned
   the whole record before rejecting; first = reject after one field).
4. **Lifetime.** A slice points into the recycled chunk `byte[]`; a reused un-decoded string slice dangles
   once the chunk advances. Rule: records stay intra-chunk, and any slice escaping per-record scope
   (`Tup.rebuild`, `==`, an opaque call) is **forced to a real `String` before the chunk is released**. `a(i)`
   was always valid; a slice is not.

**The one genuinely new mechanism: the live-column set flows BACKWARDS to the source.** Today DCE flows
*forward* — a dead column's thunk simply never fires. For bytes, a dead column must make the scan-pass emit
`skipValue` (not "decode into a slot nobody reads"), so the source's codegen is **parameterized by the
projection/predicate analysis the optimizer already computes**. The core hook is: *"here is the live-column
set + predicate fields; emit your scan/read accordingly."*

Crucially, none of this touches the **downstream** optimizer (`readShape`, DCE/sink/CSE, `Ctx.consume`, `done`,
`loopOver`) — only the **source end** generalizes from "indexable `FBase`" to "**emit a scan-pass + a `Tup` of
slot-backed columns, consuming the live-set**." That backwards live-set flow is what makes the JSON decoder a
*demonstration that the optimizer is general* rather than a bespoke parser: any future expensive /
order-dependent / lazily-decodable source plugs into the same hook. **So S1's `Source` should be designed as
this decomposed-source contract from the start — not merely a chunk-puller.** (Caveat: the `FuseMacro` is
welded to `package farray` internals, so the decoder lives downstream as a *consumer* of this public contract;
build it in-repo first, hoist to `farray-json` once the hook is clean.)

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

## 7. Sequenced plan (S1–S4, per the research synthesis)

The research reordered priorities: the **`Source` + chunk-driver is the keystone** (it makes the whole
existing surface constant-memory in one move), so it comes first — not after the in-memory grouping niceties.

- **S0 — buffer-slot + epilogue infra** (§3a): mirror of `scanLeft`'s prologue. Small; unblocks the
  cross-chunk state hoisting AND `groupAdjacentBy`/`grouped`-as-stages. Do this first as plumbing.
- **S1 — incremental `Source[A]` + chunk-driver** (§4) ← **the keystone.** `trait Source[A] { pullChunk():
  FArray[A] | End }`, specialized per kind; teach the macro to stage over a `Source` by wrapping the element
  loop in a chunk loop with state hoisted above it. Ship sources: `fromIterator`, `fromInputStream`/`lines`
  (the O(1) file-reader demo), `unfold`/`unfoldChunk`, `iterate`, `repeat`, `range`. Turns every existing
  CONST/SINK-O(1) terminal into constant-memory streaming for free. Subsumes fs2's source constructors,
  unboxed.
- **S2 — resource safety via an ox-style scope** (§4.4): emit a `supervised` scope (built on JDK Loom, not
  the ox dep — see §0.5.4) ONLY when a source/stage needs it; `useInScope(acquire)(release)`; guaranteed
  release on completion / short-circuit / exception. Validate with "`take(1)` over a file still closes it."
- **S3 — cross-chunk stateful + bounded-buffer stages** (§3b/§3c/§3f): hoist `scan`/`distinct`/
  `zipWithIndex`/`mapAccumulate` slots above the chunk loop; add `groupAdjacentBy`, `foldAdjacentBy`,
  `grouped`/`sliding`-as-stages, `chunkN`, `takeRight`/`dropRight` (ring buffers), bounded-heap `topN` (§3e).
  This is also where **nested fusion** (§3d, `groupAdjacentBy(key)(Fuse[A]=>B)`) lands — it's the same
  push-fold shape as the chunk-driver, so it's cheap once S1 exists.
- **S4 — the structured-concurrency seam** (§5) ← **the differentiator.** `parMap(n)`/`mapAsync(n)`,
  `merge`, `prefetch(n)`, `concurrently`, and the runtime drivers for dynamic `flatMap`-of-`Source` /
  `parJoin`. Each is a scope-opening seam: fused-producer loop → bounded `ArrayBlockingQueue[FArray[A]]` →
  fused-consumer loop. ~250 lines of JDK-Loom structured concurrency (borrow ox's `parEvalMap` ordering +
  cancel-on-first-error designs). Keep chunks-across-the-seam so it stays unboxed.

Note the resequencing vs the original list: nested fusion (§3d) and the in-memory adjacency stages move to S3
because S1's push-fold machinery is their prerequisite and delivers far more leverage first.

---

## 8. Inputs to this plan
- `docs/inside-the-fusion-macro.md` — how the current fusion works (hygiene, predicates, applicative core,
  zip, nesting, dynamism, collect).
- `docs/fused-pipeline-design.md` — original design.
- `docs/fs2-research.md` — fs2 combinator inventory categorized by memory behavior, Chunk/Pull/Stream/Scope
  architecture (verified vs a v3.13 clone), where fs2 boxes, concurrency reimagined in ox terms, the concrete
  `Source`/chunk-driver proposal (§4 there), the honest "won't port" boundary, and S1–S4 recommendations.
- `docs/ox-research.md` — ox inventory (incl. its `Flow`/`Channel` model), side-by-side ox-vs-raw-JDK-25 for
  each primitive we need, the `StructuredTaskScope`-still-preview finding, and the **build-on-JDK (don't
  depend on ox)** recommendation with the ~250-line minimal core.
