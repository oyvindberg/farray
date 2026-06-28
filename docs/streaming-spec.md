# Streaming spec — constant-memory fusion over incremental sources

*Branch: `streaming`. Predecessor: the fusion PR (`fused-json`), which delivered compile-time fusion over
in-memory `FArray` + a byte-sourced JSON projection demo. This document specs the NEXT capability: turning the
fused pipeline into a **constant-memory streaming engine** over incremental sources, with the **smallest
possible set of new primitives**.*

*Status: design. Nothing implemented on this branch yet. This is the contract we build against.*

---

## 0. Goal, non-goals, and the two hard constraints

**Goal.** Process arbitrarily large / unbounded input in **O(1) working set** (independent of stream length),
keeping the existing fused-loop guarantees: one loop, no `Function1`, no per-element boxing of primitives,
correct short-circuiting, parity with the eager semantics.

**The two constraints that govern every decision in this spec:**

1. **Constant memory must be *achievable and provable*.** Not "usually small" — *bounded by construction*. The
   live set at any instant must be a closed-form function of (stage state + ONE chunk), never of stream length
   `N`. Every primitive we add is judged first against: *can this run in O(1) extra memory, and can we point at
   the exact bytecode that bounds it?* Operators that cannot (a full sort, a `groupBy` that buffers all rows)
   are **barriers** and are explicitly out of the streaming core — they may exist as eager terminals, but they
   are not streaming primitives and must not silently appear inside a streaming pipeline.

2. **Smallest-possible primitive set.** Every new concept costs forever: codegen paths, tests, the mental model,
   the HugeMethodLimit budget. The discipline: a feature ships as a *primitive* only if it cannot be expressed
   as sugar over an existing one. We are ruthless about collapsing — the fusion PR's own history (≈45 terminals
   collapsed to 8 `TTag` + 8 `AggSpec`; reduce/min/max/count/fold/foreach all routed through one
   `(AState.step, AState.finish)` shape) is the precedent. **The target for this whole effort is ~3 new
   load-bearing primitives, not a combinator zoo.**

**Non-goals (this PR).**
- No `IO`/effect monad. Effects are "a `map` doing I/O on a virtual thread," nothing more.
- No structured concurrency yet (that is the *following* PR — see §9, kept out of scope here deliberately so the
  pure constant-memory core lands first and clean).
- No operator rewriting (the macro will not turn `sorted.take(n)` into a heap; the user asks for `topN`).
- No new element kinds (still Int/Long/Double/<:AnyRef, specialize-or-fail).

---

## 1. The one structural fact this all rests on (verified in code)

The macro's outermost assembly today is (FuseMacro.scala, `core` epilogue):

```
val src0 = <source>; val n0 = src0.length
withDone        { done =>                       // the done flag        — declared ABOVE the loop
  declareSlots(counterStages) { counters =>     // every stage's state  — declared ABOVE the loop
    assemble(src0, n0, done, counters)          // → drive(ctx) → loopOver(src0, …)  ← the ELEMENT loop
  }
}
```

Two facts make streaming a *parameterization*, not a rewrite:

- **All cross-element state is already hoisted above `loopOver`.** `withDone` declares `done`; `declareSlots`
  declares every stage's vars — take/drop counters (`LimSpec`), `zipWithIndex` counter (`IdxSpec`), `zip`'s
  `that`+counter (`ZipSpec`), `dropWhile`'s `var dropping` (`DropWhileSpec`), `distinct`'s `seen` HashSet
  (`DistinctSpec`), `scanLeft`'s `var acc` (`ScanSpec`). These are *exactly* the vars that must persist across
  chunk boundaries — and they already live one level above the element loop. **Cross-chunk state needs no new
  hoisting machinery; it needs the chunk loop placed between `declareSlots` and `loopOver`.**

- **The "outer loop wrapping inner fused work, `done` at the outer level" shape already runs in production** —
  it is the JSON path (`loopOverJson`, FuseMacro.scala:1042):

  ```
  var lineStart = src.from
  while lineStart < until && !done do          // OUTER loop (records)
     … scan one record into slots; run the fused inner body (buildBody) …
     lineStart = lineEnd + 1
  ```

  The *entire downstream optimizer* — `buildBody`, `Shape`/`Sc`/`Tup`, DCE/sink/CSE, `Ctx.consume`, the `done`
  break — is **identical** between the array path (`loopOver`) and the JSON path (`loopOverJson`). Only the
  *source read* differs (`a(i)` vs "scan forward to the next record"). The chunk driver is a third member of
  that family: "the source read is *advance to the next chunk, then index it*."

> **Consequence.** S1 (the chunk driver, below) converts **every existing terminal and stage** to
> constant-memory streaming with **zero changes to terminals, stages, or the optimizer**. That is the whole
> reason this is tractable and why it is the keystone.

---

## 2. The primitive set (the entire surface this PR adds)

We add **exactly three** load-bearing primitives. Everything else in this spec is either (a) sugar over these,
(b) existing machinery reused, or (c) explicitly deferred.

| # | Primitive | What it is | Why it must be a primitive |
|---|-----------|-----------|----------------------------|
| **P1** | **`Source[+A]`** + the **chunk driver** | A pull-based, chunk-granular, kind-specialized source the macro can stage over, plus the outer-loop codegen that drives it. | The single abstraction that makes the existing fused surface constant-memory. Cannot be sugar — it is a new *source shape* alongside `FBase` and `NdjsonSource`. |
| **P2** | **`PushConsumer` compilation** (compile a `Fuse[A]` to `(init, step, finalize)`) | The ability to lower a fused sub-pipeline to a push-fold whose "above-loop" state is *externally owned* and fed elements one at a time. | This is what nested fusion (`groupAdjacentBy(key)(reduce)`) and any push-driven source need. It is *the same shape* as P1's driver (see §5) — building P1 is 80% of building P2. |
| **P3** | **The epilogue hook** (`withEpilogue`, mirror of the existing `withScanPrologue`) | Run a fixed block of downstream-emitting code *after* the loop, guarded by `!done`, to flush trailing stage state. | Required by any stage that holds a pending run/window across the boundary (`groupAdjacentBy`, `grouped`-as-stage, ring buffers). Tiny, but genuinely new plumbing. |

Everything user-facing below (`groupAdjacentBy`, `foldAdjacentBy`, nested fusion, `grouped`/`sliding`-as-stages,
`takeRight`, the source constructors) is **expressed in terms of P1/P2/P3 plus the existing stage/slot
machinery.** If a proposed feature needs a *fourth* primitive, that is a signal to stop and re-derive.

---

## 3. P1 — `Source[+A]` + the chunk driver (the keystone)

### 3.1 The trait (minimal)

```scala
// kind-specialized at the call site by the macro, exactly like the FArray opaque type.
trait Source[+A]:
  /** the next chunk of elements, or the end sentinel. The chunk is an FArray (so the leaf stays int[]/long[]/…
   *  — zero per-element boxing) and is CONSIDERED INVALID after the next pull (it may be recycled). */
  def pullChunk(): FArray[A] | Source.End
object Source:
  sealed trait End
  case object End extends End
```

Design notes, each tied to a constraint:

- **Chunk-granular, not element-granular.** One virtual call (`pullChunk`) amortizes over a whole `int[]`. An
  element-granular `pull(): A | End` would re-introduce a per-element virtual call and box the sentinel — the
  exact indirection fusion exists to kill. *(Constraint 2: keeps the hot path identical to today's array loop.)*
- **The chunk is an `FArray`, reused.** The leaf is `int[]`/`long[]`/`double[]`/`Object[]`; moving a chunk is
  moving one pointer. The driver indexes it with the *existing* `loopOver` body. *(Constraint 1: live data =
  one chunk's backing array.)*
- **Chunk lifetime is intra-chunk.** A `Source` may hand back the SAME backing array on the next pull
  (`fromInputStream` reuses its read buffer). Rule, inherited verbatim from the JSON slice-lifetime rule
  (streaming-roadmap §4.5.4): **anything that escapes per-chunk scope is forced to a real value before the next
  pull.** For primitive kinds this is automatic (values are copied into stage slots / the output by value). For
  reference kinds and byte-slice sources, an escaping element (into `Tup.rebuild`, `==`, an opaque call, or the
  output buffer of `run`/`toList`) must be materialized. The `run`/`toList`/`groupAdjacentBy`-materialize paths
  already copy into a fresh leaf; the at-risk cases are exactly the ones the JSON path already guards.

### 3.2 The driver (codegen)

A new `loopOverSource` sits beside `loopOver`/`loopOverJson` and is selected the same way (`jsonSrc match …`
becomes a 3-way `sourceKind match`). Shape:

```
val src = <the Source>
var chunk = src.pullChunk()
while (chunk ne End) && !done do          // OUTER chunk loop ; `done` already hoisted by withDone
   val leaf = chunk.asFBase ; val len = leaf.length
   var i = 0
   while i < len && !done do              // INNER element loop = the EXISTING loopOver body, verbatim
      <perElem(read a(i))>                //   ← buildBody, Ctx, Shape, DCE/sink/CSE — UNCHANGED
      i += 1
   chunk = src.pullChunk()
```

- The **inner loop is literally the current `loopOver` body** (the kind-specialized `while i<len` with the
  `${kind}At`/leaf read). We extract it so both `loopOver` (single chunk = the whole `FArray`) and
  `loopOverSource` (many chunks) share it. *(Constraint 2: no duplicated element-loop logic.)*
- `done` is checked at **both** levels so a satisfied `take`/short-circuit stops mid-chunk AND prevents the next
  pull (and thus the next I/O / resource use). *(This is what makes `take(1)` over a 100 GB file read one
  chunk.)*
- `capExpr` (the static output-capacity bound used by `run`) has **no static `n0`** for a source — the output
  path falls back to the existing **growable** assembly (`needsGrow`), the same one `flatMap`/`scan`/JSON
  already use. So `run`/`toList` over a stream grows like today; a *folding* terminal (sum/count/fold/agg) needs
  no buffer at all and is the true O(1) case.

### 3.3 What this unlocks for free

The instant `loopOverSource` exists, **every CONST stage and every O(1)-finalize terminal becomes
constant-memory streaming with no further work**: `map/filter/collect/take/drop/takeWhile/dropWhile/scanLeft/
zipWithIndex/tapEach` × `foreach/fold/count/sum/min/max/reduce/find/exists/forall/agg/…`. This is the leverage
that justifies P1 going first.

**Stages that are NOT automatically O(1) over a source, and the honest rule:**
- `distinct`/`distinctBy` — `seen` HashSet grows to the number of *distinct* values: **O(distinct), unbounded**.
  Allowed to compile (it streams, it short-circuits), but documented as not-constant-memory. No silent buffering
  of *rows*, just keys.
- `zip(that)` where `that` is an in-memory `FArray` — fine (the in-memory side is already realized). `zip` of
  two *sources* is a §9 concurrency seam, out of scope.
- `flatMap(a => FArray)` — O(1) (inner is a finite materialized array per element). `flatMap` of a *runtime
  Source* is dynamic topology, out of scope.
- `sorted`/`groupBy`/`reverse` — **barriers**, never streaming. See §6.

### 3.4 Source constructors (sugar over P1, ship a minimal set)

These are *not* primitives — each is a small `Source` implementation. Ship the spanning set:

- `Source.fromIterator[A](it)` / `fromIterable` — chunk the iterator into leaves of size `chunkSize`.
- `Source.fromInputStream(is)` / `lines(reader)` — the O(1) file-reader; reuses one read buffer. **The headline
  demo.**
- `Source.unfold(s0)(f: S => Option[(A, S)])` / `unfoldChunk` — the general generator (covers `iterate`,
  `repeat`, `range`, `tabulate` as one-liners over it). Prefer `unfoldChunk` as the real primitive and `unfold`
  as sugar (chunk-of-one is wasteful).
- `FArray` itself is trivially a one-chunk `Source` — so the in-memory path is a *degenerate* source, and we may
  later collapse `loopOver` into `loopOverSource` entirely (one driver). **Do not do that collapse in this PR**
  (risk vs. reward; the array path is hot and proven) — but design `loopOverSource` so the collapse is possible
  later. *(Constraint 2: keep the door open to fewer primitives, don't force it now.)*

---

## 4. P3 — the epilogue hook (tiny, do first as plumbing)

`withScanPrologue` already exists (FuseMacro.scala:1260): it runs a downstream-emitting block ONCE *before* the
loop (so `scanLeft` emits its seed even for empty input). The mirror:

```scala
def withEpilogue(c: Ctx, body: Expr[Unit]): Expr[Unit] = epilogueInfo match
  case Some(emit) => '{ $body ; if !${doneCond} then ${ emit(c) } }   // flush trailing state, unless satisfied
  case None       => body
```

- Guarded by `!done` so a satisfied downstream `take` does not get a spurious trailing chunk.
- It is the *symmetric counterpart* to the prologue; placing both around `loopOver`/`loopOverSource` gives every
  stateful stage a "before-all" and "after-all" hook around the (possibly chunked) loop.
- This is the only genuinely-new *plumbing* primitive. ~20 lines. Land it first (call it **S0**) because P2's
  nested fold and the adjacency stages all need it.

---

## 5. P2 — `PushConsumer` compilation (compile a `Fuse[A]` to `(init, step, finalize)`)

This is the deepest primitive and the bridge between "streaming sources" and "nested fusion." They are the same
mechanism viewed from two ends.

### 5.1 The shape

A fused pipeline today is *pulled*: the driver reads `a(i)` and calls `perElem`. A **push consumer** is the
inversion: an external loop owns the iteration and feeds elements in. Compiling `Fuse[A] ⇒ B` to a push consumer
yields three emitted fragments, parameterized by where its state lives:

```
init     :  declare the pipeline's above-loop state (its slots + terminal accumulator) — but at a
            CALLER-CHOSEN scope (per-chunk for a driver; per-group for nested fusion).
step(a)  :  the per-element body — exactly buildBody(stages, a, ctx) with the terminal's step as `consume`.
finalize :  the terminal's finish term (read the accumulator → B), run when the caller declares "no more".
```

**Every terminal already is `(init, step, finalize)`** — after the fusion PR's consolidation this is literally
the `AState(step, finish)` for the agg family, and the same shape for the short-circuit terminals (with their
`done`). P2 is "expose that triple as a callable unit and let the *caller* own the init scope," instead of the
macro always wrapping it in its own `withDone`/`declareSlots`/`loopOver`.

### 5.2 Why P1 ⇒ P2 is nearly free

The chunk driver (§3.2) already *is* a push loop: the outer chunk loop pushes a chunk's elements through the
inner body. Generalize "the elements come from `src.pullChunk()`" to "the elements come from whatever the caller
feeds `step`," and you have P2. The macro work is: factor `buildBody(stages, a, ctx)` + the terminal's
`(init, step, finalize)` out of the driver so it can be (a) driven by the chunk loop (P1) or (b) embedded as the
inner fold of a group (nested fusion) or (c) handed to a push source.

### 5.3 What P2 buys (all as sugar, no further primitives)

- **Nested fusion** — `groupAdjacentBy(key)(reduce: Fuse[A] => B): Fuse[(K, B)]`. Per-group work is a fused
  sub-pipeline whose `init` runs at each key change, `step` runs per row as rows stream past, `finalize` emits
  `(key, B)`. When `reduce` is a fold (`_.map(f).sum`, `_.count`, `_.min`), **the group's rows are never
  materialized** — O(1) memory per group, zero per-group allocation. `_.run`/`_.toList` recovers materialization
  only when the rows are actually wanted. This is the analytics prize.
- **Push sources** — a source that *drives* (an I/O callback) feeds `step`; same triple.

---

## 6. The streaming/barrier boundary (the discipline that protects constant memory)

Operators are partitioned, and the partition is enforced, not hoped for:

- **Streaming (O(1) latency, fuse into the loop, short-circuit):** map, filter, collect, take, drop, takeWhile,
  dropWhile, flatMap(→FArray), zip(in-memory), zipWithIndex, scanLeft, tapEach, **groupAdjacentBy/foldAdjacentBy
  (ordered-key), grouped/sliding-as-stages (bounded buffer), takeRight (ring buffer)**.
- **Barriers (must see all N before emitting):** sorted/sortBy, groupBy/groupMapReduce (hash, all rows),
  reverse. And the reductions are "see all, emit O(1)" — fine to *end* a stream, not to sit mid-stream.

**The three user declarations that turn a barrier into a streaming op** (the only sanctioned escapes from a
barrier, each O(1)-or-bounded):
1. *"aggregate only, not the rows"* → fold into per-key accumulators. (`foldAdjacentBy` when ordered;
   `groupMapReduce` stays a barrier when unordered.)
2. *"the input is already ordered by the key"* → `groupAdjacentBy`/`foldAdjacentBy`: emit on key change.
   O(largest run), not O(N).
3. *"only the top-k"* → bounded heap (`topN`/`largest`/`smallest`, already shipped in the fusion PR). O(k).

**The macro must never guess or reorder to reach these** — that would be a miscompile for impure code (same
reason guard-reordering was rejected). The user *declares* the precondition by choosing the combinator; if they
use `groupAdjacentBy` on unordered input, that is their bug, documented, not ours to detect. A streaming
pipeline that contains a non-declared barrier should be a **compile error** ("`sorted` is a barrier; it cannot
appear in a streaming pipeline — collect to an `FArray` first, or use `topN`"), not a silent O(N) buffer.

---

## 7. Worked memory accounting (the proof obligation for every primitive)

For each primitive we commit to a closed-form live-set bound, to be asserted by a test (the fusion PR's
"50 passes, assert <N MB" pattern, plus a post-GC live-memory assertion like `topNTest`):

| Pipeline | Live set | Bound |
|----------|----------|-------|
| `src.fuse.filter(p).map(f).sum` | acc + 1 chunk | O(chunk) |
| `src.fuse.filter(p).foldLeft(z)(op)` | acc + 1 chunk | O(chunk) |
| `src.fuse.scanLeft(z)(op).foreach(sink)` | scan acc + 1 chunk | O(chunk) |
| `src.fuse.take(k).run` | output(≤k) + 1 chunk | O(k) |
| `src.fuse.foldAdjacentBy(key)(z)(op)` (ordered) | (curKey, acc) + 1 chunk | O(chunk) |
| `src.fuse.groupAdjacentBy(key)(_.sum)` (ordered, nested) | (curKey, inner acc) + 1 chunk | O(chunk) |
| `src.fuse.groupAdjacentBy(key)(_.run)` (ordered, materialize) | current run rows + 1 chunk | O(largest run) |
| `src.fuse.takeRight(n).run` | ring(n) + 1 chunk | O(n) |
| `src.fuse.distinct.run` | seen-keys set + output | **O(distinct)** — flagged, not constant |
| `src.fuse.sorted` | — | **barrier — rejected in streaming** |

If a proposed combinator cannot fill a row here with an `N`-independent bound (or a user-declared bound like
`k`/`run`/`distinct`), it does not enter the streaming core.

---

## 8. Sequenced plan (this PR)

- **S0 — epilogue hook (P3).** ~20 lines mirroring `withScanPrologue`. Unblocks everything stateful-across-the-
  boundary. Land first, with a test that `scanLeft`-style "flush after loop" works and is `!done`-guarded.
- **S1 — `Source` + chunk driver (P1).** The keystone. `trait Source`, `loopOverSource` (extracted from the
  shared inner element loop), the `sourceKind` dispatch, and the spanning constructor set (`fromIterator`,
  `fromInputStream`/`lines`, `unfoldChunk`). Acceptance: every existing terminal/stage works over a `Source`;
  the file-reader folds a file larger than heap; `take(1)` over a file pulls one chunk (assert pull count).
- **S2 — resource safety.** Open-on-first-pull / close-on-termination-or-short-circuit-or-exception for file/
  socket sources. Acceptance: `take(1)` over a file *closes* it. (A small `try/finally` around the driver keyed
  on the `Source` declaring a `close()`; the structured-scope version is the *next* PR.)
- **S3 — cross-chunk stateful + bounded stages, built on P2+P3.** `groupAdjacentBy`/`foldAdjacentBy`, nested
  fusion (`groupAdjacentBy(key)(Fuse[A]⇒B)`), `grouped`/`sliding`-as-stages, `takeRight`/`dropRight` (ring
  buffers). Each is sugar over P1/P2/P3 + existing slots; each gets a §7 memory test.

Each step is independently testable against the existing parity harness (`FListTest`-style: streaming result ==
the eager `Iterator`/`List` result), the snapshot golden, and a memory assertion.

---

## 9. Explicitly out of scope (the NEXT PR — named here so we don't drift)

- **Structured concurrency** — `parMap(n)`/`mapAsync`, `merge`, `prefetch`, `concurrently`, dynamic
  `flatMap`-of-`Source`, `parJoin`. Each is a **scope-opening seam**: fused-producer loop → bounded
  `ArrayBlockingQueue[FArray[A]]` → fused-consumer loop, chunks across the seam to stay unboxed. Built on raw
  JDK Loom (~250 lines), not a dependency on ox (its `Flow` is push/boxed — wrong shape). Deliberately deferred:
  the pure constant-memory core (S0–S3) must land first and clean, with no thread/queue machinery muddying the
  fusion proofs.
- **The decomposed-source contract generalized beyond JSON** (Parquet/Arrow/protobuf/DB-cursor): the "backwards
  live-set flows to the source" hook (streaming-roadmap §4.5) is real and the JSON path already implements it,
  but generalizing the contract is its own effort. P1's `Source` should be *designed not to preclude* it (leave
  room for a `pullChunk` variant that consumes the live-set), but this PR ships the value-chunk source.

---

## 10. Risk register (cautious by request)

| Risk | Mitigation |
|------|-----------|
| Chunk lifetime bugs (a recycled buffer's slice/ref escapes) | Inherit the JSON force-before-release rule; the at-risk escapes are exactly the ones the JSON path already forces. Primitive kinds are copy-by-value, immune. Test: a reusing `Source` + `run`/`==`/`groupAdjacent-materialize`. |
| HugeMethodLimit | *Improves* under chunking — the per-chunk body is the same bounded loop the no-blowup test covers; the outer chunk loop adds ~nothing. Keep the regression test. |
| Two element-loop copies drift (`loopOver` vs `loopOverSource`) | Extract the inner element loop into ONE shared emitter both call. (And keep the door open to collapsing the array path into the source path later.) |
| Silent O(N) from a barrier in a streaming pipeline | Compile error, not a buffer. §6. |
| Primitive creep | The §2 table is the budget. A 4th primitive is a stop-and-rederive signal. |
| `done` not checked at the chunk level → reads one chunk too many | `done` guards BOTH loops and gates the next `pullChunk`. Test: pull-count on `take`. |

---

## 11. The bottom line

- **~3 new primitives** (`Source`+driver, push-consumer compilation, epilogue hook) — the rest is sugar.
- **Constant memory is structural**, not aspirational: live set = stage state + one chunk, with a per-primitive
  bound asserted by a test, and barriers fenced out by a compile error.
- **The optimizer, every stage, and every terminal are untouched** — S1 carries the entire existing surface into
  streaming, because the macro already emits "state above the loop, loop wrapped in an outer loop, `done` at the
  outer level" (proven by the JSON path).
- **Concurrency is the next PR**, behind a clean seam, so the pure core lands first.
