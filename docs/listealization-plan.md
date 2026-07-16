# Listealization: closing FArray's List-tax on tiny-sequence workloads

Status: PLAN (nothing implemented). Driven by the Scala 3 compiler migration's closing
benchmarks and JFR profiles (scala3 repo, `farray-migration/BENCH-RESULTS.md` +
`out/bench/profiles/`).

## 0. The problem, with numbers

The FArray-migrated compiler (round 8) vs List-based main, compiling identical sources:

| corpus | farray vs main |
|---|--:|
| stdlib (709 sources, byte-identical) | +6.03% |
| compiler-src (576 files) | +5.66% (beats Vector's +6.70%) |
| mill libs.javalib (98 files, inline-heavy) | +3.41% (Vector: +1.86%) |

JFR attribution: the vector-vs-main delta is dominated by *collection machinery*
(`Vector.foreach` +62pp inclusive), not dotc logic — and farray inherits the same cost
class. The workload: **millions of 0–3 element ref sequences, built once, traversed one
or a few times, frequently destructured** (`case h +: t`), rarely random-indexed.

What List does for that shape, per element: one perfectly-predicted `instanceof ::` +
two field reads. Construction: one 24B cons cell, no length bookkeeping, no array.
`tail`: a field read, zero alloc.

What FArray pays today for the same shapes (ref kind):

1. **Construction** — `FArray(a, b)` = `RefArr` = **two** allocations (node + `Object[2]`)
   plus a length field; the macro unrolls stores but can't remove the array indirection.
2. **Op entry** — leaf-peel instanceof cascade + loop setup, amortized over n≤3 elements
   (fine at n=32, dominant at n=2). The tag-dispatch research (`node-tag-dispatch-research.md`)
   proved this is *not* dispatch-depth: chains are already leaf-first and beat tableswitch
   on the hot arm. The floor is entry/setup overhead relative to tiny n.
3. **Destructuring** — `case h +: t` on a non-Prepend scrutinee: `_2` = `tail` = `drop(1)`
   = **a SliceNode allocation per match**, and nested matches build SliceNode towers.
   List's equivalent is free. With 603 `+:` pattern sites in dotc, this is a real,
   currently-unprofiled tax (SliceNode wasn't a top JFR line, but alloc-sampling
   undercounts small short-lived objects; needs targeted measurement).
4. **Single-element churn** — `RefOne` per changed element in conserve-maps is inherent
   and already optimal; but `Repr$refRepr` given instantiation (172 MB, 1.2% of alloc)
   is not (parameterized given → fresh instance per call).

## 1. Design principles (hard-won; do not relitigate without new data)

- **Measure, don't assume.** Every phase has a JMH gate BEFORE it merges; end-to-end
  arbitration via the scala3 bench rig (`bench.sh` trio + isolation A/B for wash detection).
- **No tag dispatch** — measured slower on the hot arm (`node-tag-dispatch-research.md`).
- **Fast paths live inside farray**, not at consumer call sites (round-7 experiments:
  hand-rolled dotc loops lost to the native traversers).
- **Respect the inline budget** — HugeMethodLimit and anon-class duplication are real
  (see `jvm-inlining-boxing-pitfalls.md`); every inline-surface growth needs a bytecode
  size check on a representative consumer.
- **Ref kind first.** The compiler's tiny sequences are refs. Primitive kinds keep their
  current leaves unless a phase is free for them.
- `FBase.length` stays — it is load-bearing for O(1) structural ops. A cons family
  without length (a true FList inside FBase) is REJECTED: it breaks the core invariant
  every engine relies on. `Prepend` already *is* a length-carrying cons cell.

## 2. Phase 0 — ground truth (do this before any design commitment)

1. **Mine the remaining research**: `benchmark-loss-fastpath-plan.md`,
   `traversal-fastpath-analysis.md` (engine map), `jvm-inlining-boxing-pitfalls.md`,
   git log, project memory. Update this plan where they contradict it.
2. **Instrumented shape histogram.** Diagnostic farray build with counters at op entries:
   (op × node-class × length bucket 0/1/2/3/4–8/9–32/33+). Compile the mill corpus and
   stdlib with the instrumented compiler; dump the histogram. This replaces guesswork
   about which (op, shape, size) pairs matter and how much `+:`-match SliceNode churn
   really costs.
3. **The compiler-shaped JMH suite** (`CompilerShapeBenchmark`): a weighted mix from the
   histogram — construction, foreach/foldLeft, conserve ops, `+:` destructuring, with
   megamorphic pollution (multiple shapes per site) — measured for FArray vs List vs
   Vector. **This suite is the acceptance metric for every phase below.** Target: parity
   with List on the weighted mix.

### 2.0 Research-mining findings (deliverable 1 — what the four research docs and rounds 7–8 change)

Mined `benchmark-loss-fastpath-plan.md`, `traversal-fastpath-analysis.md`,
`jvm-inlining-boxing-pitfalls.md`, `node-tag-dispatch-research.md`, and `git log` (rounds 7–8:
`5cfe598`, `5cd2656`, `5e4b583`, `d2b3db6`, `13f226c`). All four round-7/8 code changes are
present in current `GenCores.scala` (verified). Corrections/sharpenings to this plan:

- **The traversal-engine map is PARTIALLY SUPERSEDED by rounds 7–8, but its asymptotics were
  never the listealization bottleneck.** `traversal-fastpath-analysis.md` centred on the
  `kindAt` fallback going O(n·d)/O(n²) on **trees** for the short-circuit / backward / two-operand
  families. Rounds 7–8 closed the two-operand half outright: `d2b3db6` makes
  corresponds/startsWith/endsWith/sameElements/indexOfSlice/zip/unzip **walk both operands via one
  dfs, never materialize** (its Options 1–3 for that family are effectively shipped), and `5cd2656`
  **peels right-leaning Concat spines with no DFS-stack allocation** (the O(n²) append/concat-spine
  case). **What still holds:** the Engine-B short-circuit family (`exists`/`forall`/`find`/
  `indexWhere`/`indexOf`/`collectFirst`/`prefixLength`/`count`/`foreachWhile`/`contains`) and
  Engine-C `foldRight` still use the two-arm `scan`/`foldRightV` (leaf-or-`kindAt`) shape — no
  breakable/reverse dfs was added. **BUT this is orthogonal to listealization:** those O(n·d) taxes
  only bite on deep *trees*; the tiny 0–3-element ref leaves / `One` / `Prepend` this plan targets
  pay `kindAt` at O(1) depth, so what remains is a per-element **node-match constant factor**, not an
  asymptotic problem. The durable breakable-dfs (that doc's Option 1) is therefore **not a
  prerequisite for any phase here** — drop it from listealization's critical path.

- **The size-1 read floor lever is already spent; representation is the remaining one.**
  `node-tag-dispatch-research.md` (keep "no tag dispatch" — confirmed, −17-20% on the mono leaf)
  identified the floor as call-boundary + pointer-chase and pointed at "a direct `${K}Arr`
  fast-path in `applyAt`" as the only lever. That was implemented in round 7 (`5cfe598`,
  inline-leaf `applyAtImpl`) and measured **end-to-end WASH** (BENCH-RESULTS isolation A/B). So the
  reduce-family "intAt read floor" from `benchmark-loss-fastpath-plan.md` is now as-good-as-it-gets
  at the call-boundary level; the only remaining way down is **field-leaf representation** (D1/D2b),
  which removes the pointer-chase entirely for tiny n. This *strengthens* D1/D2b's rationale and is
  why phase 1's D3 audit is expected to be low-yield on its own.

- **`benchmark-loss-fastpath-plan.md` is the concrete spec for D3, but its 127-loss list is
  mostly OUT of the listealization workload.** Its invariant — *peel `Empty` and `{K}One` at the
  very front, before `summonFrom`/`materialize`/DFS-consumer alloc* — is exactly D3's audit rule.
  Many hot ops already comply (`zipWithIndex`, `zip`, `unzip`, `mapConserve`, `filterConserve`,
  `updated`, `append`, `prepend` — verified). The still-unpeeled ops it flags (sort/groupBy/
  partition/diff/intersect/toSet/distinct/toArray/scan/mkString/lastIndex*) are **rare in dotc's
  tiny-seq traversal** — the histogram (deliverable 2) must confirm which ops actually fire at
  n≤3 before D3 spends effort. Expect D3 to reduce to a handful of ops, not 18.

- **`jvm-inlining-boxing-pitfalls.md` sharpens two gates and adds a NEW risk to D1.**
  (1) The inline-budget gate is numeric: `HugeMethodLimit`/`DesiredMethodLimit` = **8000
  bytecodes**; the abandoned inlined-walk hit it (~640 bytecodes/op → interpreted → 0.27–0.37×
  IArray). Every D1/D2b inline-surface growth needs a **bytecode-size check on a real dotc
  consumer**, not a micro. (2) **NEW risk for D1, beyond "pyramid growth":** adding a `Ref2`/`Ref3`
  leading arm doesn't only add bytecode — it adds a *receiver type* at every shared `match` site.
  With `TypeProfileWidth = 2` / bimorphic inlining, a site already seeing `RefArr`+`RefOne`+
  `Prepend` is at/over the 3-type megamorphic cliff; a new leaf class pushes more sites past it,
  **losing inlining/EA/unboxing** (the 3.3× dispatch cost is the small part). D1 can thus be
  *net-negative* on megamorphic sites even where it wins the monomorphic micro — the plan's
  "megamorphic control" gate must be the deciding measurement, and D2b (which adds **no new leaf
  class**, only a field to `RefArr`) is inherently safer on this axis. This is a concrete reason to
  prefer the phase-2 D2b-first ordering already in the plan.
  (3) This same mechanism *is* the spec for the CompilerShapeBenchmark's pollution: feed each hot
  site **≥3 distinct node shapes** so dispatch is genuinely megamorphic — that is what dotc's
  millions of mixed 0–3-element sequences produce, and it is where FArray's List-tax actually lives.

## 3. Candidate designs

### D1 — Small-arity field leaves: `Ref2` (evaluate `Ref3`)
Elements as fields, one allocation, no array indirection, no bounds math.
- Construction at n=2: 2 allocs → 1; reads: field loads.
- Codegen: per-op leading arms for the ref kind only; the `FArray(...)` macro emits
  `Ref2(a, b)` directly.
- **Risk**: pyramid growth at every dispatch site. The tag research says chain order
  is what matters — measure arm-order variants (Ref2 before/after RefArr) under
  realistic pollution. If megamorphic controls regress, cap at `Ref2` (skip `Ref3`).
- Gate: alloc/op halves at n=2; foreach/fold at n≤3 within 1.2× of List; zero regression
  at n≥8, on primitives, and on the megamorphic control.

### D2 — Allocation-free-ish destructuring (the `+:`-match tax)
Two sub-options, decided by the Phase-0 histogram:
- **D2a (with D1)**: `Ref3.tail = Ref2`, `Ref2.tail = RefOne` — bounded small allocs,
  flat results, no wrapper towers.
- **D2b (structural)**: fold slicing into the leaf — `RefArr` gains an `offset` field;
  `tail`/`drop`/`take` of a leaf return a new *leaf* (one alloc, still flat) instead of
  a `SliceNode` wrapper. Kills slice towers everywhere and simplifies every engine's
  SliceNode arms. **Risk**: +4 bytes on the hottest class; offset-aware indexing in every
  leaf loop (measure: likely free — `arr(off + i)` — but verify vectorization survives).
  This is the biggest structural simplification on the table; if it measures clean it
  likely subsumes half of D1's motivation.
- Gate: `case a +: b +: rest` over a 3-element flat FArray: ≤1 small allocation total,
  time within 1.5× of List's two-level `::` match.

### D3 — Tiny-n entry audit
Leading `length` checks so n=0/1 (and with D1, n=2) resolve before any engine setup;
verify arm order per op against the histogram. Mostly an audit of what rounds 7–8
already left in good shape; cheap, do it alongside D1.

### D4 — `refRepr` singleton (round-8 leftover, independent)
Replace the parameterized given's per-call instantiation with a shared instance. The
naive object-with-concrete-inline-bodies compiles but warns of per-inline-site anon-class
duplication — needs a bytecode-size study on dotc before shipping. Independent of D1–D3;
worth its own small phase. Evidence: 172 MB / 1.2% of allocation.

### D5 — Worklist/cons-accumulate parity check (verification, not a change)
dotc retains some cons-style accumulation. `Prepend` chains are cons cells; round-8
spine peeling made their traversal allocation-free. Add a worklist-shaped JMH
(build by `+:`, drain by `case h +: t`) to *prove* parity with List or expose the gap —
feeds back into D2.

## 4. Sequencing

| phase | content | exit gate |
|---|---|---|
| 0 | docs mine + histogram + CompilerShapeBenchmark | histogram published; suite committed |
| 1 | D3 audit + D5 verification | compiler-shape suite baseline recorded |
| 2 | **D2b offset-leaf** (if histogram confirms destructure/slice weight) else D2a | JMH gate + full suite (613+) green |
| 3 | D1 Ref2 (re-evaluate necessity after phase 2) | JMH gate incl. megamorphic control |
| 4 | D4 refRepr singleton | bytecode-size study + JMH |
| each | publish → re-pin scala3 → clean rebuild → `bench.sh` trio → isolation A/B if within noise | keep if ≥0.3pp end-to-end or neutral-with-simplification; revert otherwise |

## 4.1 Phase 2 (D2b offset-leaf) — design decisions (IMPLEMENTED, branch `dotty-gaps`)

Slicing is folded into the flat leaf: `${K}Arr`/`RefArr` gain a `final int offset` — logical element
`i` lives at `data(offset + i)`. `tail`/`drop`/`take`/`slice`/`init` of a LEAF return a NEW same-class
leaf (one small alloc, still flat), never a `SliceNode`. `Empty`/`One` results are unchanged.

Decisions made (evaluated against implementation cost + the gates):

- **Kind scope: UNIFORM across all kinds (not ref-only).** The engines are kind-generic — every leaf
  arm is emitted from one `${k.name}Arr` template that already threads a *start index* (the SliceNode
  arms passed `so`). Adding the offset to the shared template was free (offset just replaces the `0`
  start); a ref-only path would have forced per-kind branches into every engine. So Int/Long/Double/
  Float/Short/Byte/Char/Boolean/Ref all carry the offset. No per-kind special-casing anywhere.
- **Backing-field RENAME (`arr` → `data`) as the blast-radius net.** Every stale `leaf.arr` (assumes
  offset 0) now fails to compile; each of the ~70 generated + hand-written sites was made offset-aware
  deliberately (`grep '\.arr'` over `farray/` + `codegen/` is zero unaudited hits). Leaf constructors
  kept a 2-arg `(data, length)` (offset 0) so every construction site (map/fromValues/builders/the apply
  macro) is untouched and free; a 3-arg `(data, offset, length)` is the slice path only.
- **Two site classes.** (a) Sites that iterate through a parameterized start (traversal engines, dfs
  walkers, scan/hash runners, foldLoop/collectLoop) simply pass `leaf.offset` as the start — the loop
  read `a(i)`/`a[i]` is unchanged, so the offset-0 hot path is byte-identical. (b) Sites that hand the
  raw backing array out for 0-based `sa(i)` iteration or in-place `Arrays.sort`/arraycopy-from-0 (zip/
  unzip/matchAll2 `flatOf`, sort/sortBy, distinct, mapConserve/filterConserve, iterator cursor, flatMap
  segments, the FuseMacro fused-loop leaf arms) take a `if leaf.offset == 0` guard: offset-0 hands
  `leaf.data` directly (fast path preserved), a sliced leaf materializes a fresh 0-based copy (correct,
  and rare). `materialize` became `copyOfRange(data, offset, offset+length)`.
- **SliceNode kept for non-leaf bases only.** Post-D2b nothing constructs a `SliceNode` over a leaf
  (leaf `take/drop/slice` return leaves), so the `SliceNode`-over-leaf engine arms are dead but were
  still made offset-correct (base offset composes: `slf.offset + s.offset`). SliceNode itself is
  retained for the (currently-unreachable) non-leaf-base case per the plan.
- **Slack composes.** A builder leaf has `data.length > offset + length`; slicing only moves
  `offset`/`length` within the shared array, and Phase-1's OOB check validates against the LOGICAL
  length — verified under offset (`offsetOverSlackBackedLeaf` test).
- **Memory retention: unchanged tradeoff, documented not fixed.** A tiny slice pins the whole backing
  array — exactly what `SliceNode` already did. `data` is shared (asserted in the test).

Audited sites: **~62 generated (GenCores.scala) + ~9 hand-written (FuseMacro.scala 9 leaf arms; plus
OpaquePrimAllocTest white-box)**. No site class could NOT be made offset-aware.

## 5. Targets

- **Realistic**: close half of the ~6pp stdlib/compiler-src gap (→ ~+3% vs main), keep
  the compiler-src lead over Vector, pull mill under +2.5%.
- **Stretch**: List parity on the compiler-shape suite → FArray strictly dominates every
  candidate (same speed, plus O(1) structural ops, unboxing, conserve ops, fuse).
- **Orthogonal stack**: the deferred `.fuse` pass over dotc's hot files (migration plan
  Phase 6) needs no farray changes and adds on top of whatever this plan achieves.
