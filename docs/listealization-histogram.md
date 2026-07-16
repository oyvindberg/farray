# Listealization histogram: the measured (op × shape × size) ground truth

Status: DATA (Phase 0, deliverable 2 + 3 of `listealization-plan.md`). This replaces the plan's
guesswork about which `(op, shape, size)` pairs matter, how much the `+:`-match SliceNode churn
really costs, and whether Phase 2 should be **D2b** (offset-leaf) or **D1** (Ref2 field leaves).

## Method

- A diagnostic FArray build (`diag-histogram` side branch, version `1.0.0-M1+36-c8ec88dc`, **not
  merged, never pushed**) adds racy `long[]` counters in the **non-inline shared layer**: every node
  **constructor** (`S_CTOR`), the `dfsC` traversal engine (`S_DFS`), the `kindAt`/`refAt` random
  reader (`S_AT`, recorded once per external call — a recording entry delegating to a recursive
  `At0`, so internal recursion does not re-count), and the shared `fold`/`map`/`filter`/`foreach`
  leaf methods (`S_FOLD`/`S_MAP`/`S_FILTER`/`S_FOREACH`). Dimensions: `site × node-class × length
  bucket {0,1,2,3,4-8,9-32,33+}`. No-op unless `-Dfarray.histogram=<path>` is set; dumped via a JVM
  shutdown hook.
- Pinned that build into the scala3 `farray-collections-rebased` worktree, compiled
  `scala3-compiler-nonbootstrapped`, and ran that compiler directly (JDK 26, `dotty.tools.dotc.Main`)
  over both benchmark corpora — **compiler-src** (576 dotc sources) and **mill `libs.javalib`** (98
  files) — with the histogram enabled. Both compiled clean (exit 0). Counts are **per full compile**.
- Raw dumps checked in beside this doc: `listealization-histogram-compiler-src.csv`,
  `listealization-histogram-mill.csv`.
- **Caveat:** `applyAtImpl`'s inline leaf fast path (round 7, `5cfe598`) reads a top `${K}Arr`
  WITHOUT entering `kindAt`, so `S_AT` counts **non-leaf** random access only (RefOne / RefPrepend /
  Concat / …). Leaf random-access is inlined and invisible here — which is the point: the `S_AT`
  rows are exactly the reads that still pay a call boundary + pointer-chase.

## 1. The (op × shape × size) distribution

**Site totals** (share of all instrumented events):

| site | compiler-src | mill |
|---|--:|--:|
| `at` (non-leaf random read) | 38.0M (47.5%) | 34.2M (65.7%) |
| `ctor` (node construction) | 15.8M (19.7%) | 9.0M (17.3%) |
| `fold` (foldLeft/reduce/sum/…) | 12.3M (15.3%) | 4.5M (8.7%) |
| `foreach` | 8.0M (10.0%) | 2.2M (4.3%) |
| `map` | 4.6M (5.7%) | 1.5M (2.8%) |
| `dfs` (tree walk) | 0.8M (1.0%) | 0.4M (0.8%) |
| `filter` | 0.7M (0.9%) | 0.2M (0.4%) |

`dfs` is a rounding error: **almost nothing is a genuine tree** — the shared leaf/One/Slice arms in
`fold`/`map`/`foreach`/`filter` handle the workload without ever entering the `dfsC` stack walk. This
retires the traversal-engine map's asymptotic concerns for this workload (already noted in the plan's
research-mining section).

**Construction by node-class** (the shapes the compiler actually builds):

| node-class | compiler-src | mill |
|---|--:|--:|
| **RefOne** (1 elem, field) | 9.98M (**63.1%**) | 5.66M (**62.8%**) |
| **RefArr** (flat 2+) | 2.66M (16.8%) | 2.06M (22.9%) |
| **SliceNode** | **1.22M (7.7%)** | 0.27M (3.0%) |
| RefPrepend | 0.81M (5.1%) | 0.79M (8.8%) |
| Concat | 0.46M (2.9%) | 0.04M (0.4%) |
| RefAppend | 0.40M (2.5%) | 0.06M (0.6%) |
| ReverseNode | 0.18M (1.1%) | 0.04M (0.5%) |
| all primitive nodes | <0.6% | <0.9% |

**Ref-first is confirmed with numbers**: primitive leaves are <1% of construction on both corpora.
`RefOne` — the single-element sequence — is the overwhelming mode (63%), and it is *already* a
field leaf (no array). The plan's D1 targets `Ref2`/`Ref3`, i.e. the **RefArr-2/3** slice only
(≈13% of ctors on compiler-src), never the 63% RefOne mode.

**Size-bucket weights** (share of events in each length bucket):

| site | 0 | 1 | 2 | 3 | 4-8 | 9-32 | 33+ |
|---|--:|--:|--:|--:|--:|--:|--:|
| ctor | 0.0% | 63.5% | 18.5% | 6.6% | 7.3% | 3.1% | 1.1% |
| fold | 25.9% | 59.2% | 9.4% | 3.3% | 1.9% | 0.3% | 0.0% |
| map | 24.2% | 55.0% | 12.7% | 4.0% | 3.5% | 0.6% | 0.0% |
| foreach | 43.5% | 39.6% | 8.5% | 2.9% | 3.6% | 1.5% | 0.4% |
| filter | 45.5% | 31.2% | 9.5% | 2.9% | 6.0% | 4.5% | 0.4% |
| at | 0.0% | 81.2% | 8.7% | 4.4% | 3.6% | 1.3% | 0.9% |

(compiler-src; mill is within a few points except `filter`/`dfs`.) **~89% of constructions and
~85%+ of every traversal op are length ≤ 3**; traversal is additionally dominated by the **empty**
(24–46%) and **size-1** cases. This is the size mix the JMH suite weights to.

**Top individual `(op × node-class × bucket)` cells** (compiler-src):

| cell | count |
|---|--:|
| `at` · RefOne · 1 | **30.9M** |
| `fold` · RefOne · 1 | 7.2M |
| `foreach` · Empty · 0 | 3.5M |
| `fold` · Empty · 0 | 3.2M |
| `foreach` · RefOne · 1 | 3.2M |
| `map` · RefOne · 1 | 2.5M |
| `at` · RefPrepend · 2 | 2.0M |
| `at` · Concat · 2 | 0.8M |

**The single largest cell across the whole compiler is `at` · RefOne · 1 = 30.9M (compiler-src),
30.4M (mill)**: reading the one element of a `RefOne` (`head`/`apply(0)`/`_1`) through the `kindAt`
call boundary. `applyAtImpl`'s leaf fast path peels `${K}Arr` but **not `RefOne`**, so these 30M+
reads/compile still cross the non-inlined call. Peeling `RefOne` (and the length-1 case generally)
at the `applyAt` inline surface — a Phase-1 / D3 item — is the cheapest, highest-count win on the
board, independent of the D1/D2b choice.

## 2. The SliceNode-per-`+:`-match question, answered

`case h +: t` on a non-`Prepend` scrutinee sets `t = tail = drop(1)`, which on a flat `RefArr`
allocates a **`SliceNode`**; nested matches build SliceNode towers.

- **SliceNode constructions per compile: 1,222,299 (compiler-src) / 268,899 (mill)** — 7.7% / 3.0%
  of all node allocations. By length: `2`→481k, `3`→202k, `4-8`→292k, `9-32`→153k, `33+`→93k
  (compiler-src). **56% of SliceNodes are length 2-3** — the small-tail shape a `+:` match produces.
- SliceNode is the **3rd-largest allocation class** on compiler-src (behind RefOne and RefArr) and
  the **only structural allocation that a representation change can remove**: RefOne is already
  optimal, RefArr is inherent to a 2+-element literal.
- The provenance is not *only* `+:`: `SliceNode` also comes from `take`/`drop`/`slice`/`init`. But
  the length distribution (peaked at 2-3) and the JMH destructuring numbers below (7.7–10× slower
  than List) confirm the `+:`-tail is a first-order contributor. Destructuring on **Prepend**-built
  sequences is *free* (`tail = base`, no alloc) — and `at`·RefPrepend is 3.7M/3.3M, so a large slice
  of destructuring already pays nothing; the tax is concentrated on `+:`-of-flat-array.

## 3. CompilerShapeBenchmark baseline (the acceptance metric)

`benchmarks/.../CompilerShapeBenchmark.scala`, weighted from §1: construction at the measured arity
mix; foreach/foldLeft/map/mapConserve/filterConserve over a **megamorphic** input array holding
RefOne + RefArr(2/3) + RefPrepend + Concat + SliceNode at the histogram size mix (so each op site
sees ≥5 receiver types — the histogram shows 4-6 at the hot sites, past the 3-type cliff); and
`case h +: t` destructuring 1-/2-/3-deep. Candidates run the **same logical data** (List/Vector built
from each FArray's elements). Throughput ops/s, higher is better; `-f 1 -wi 3 -i 5`, JDK 26,
macOS aarch64. `mapConserve`/`filterConserve` competitor columns use `map(id)`/`filter` (List/Vector
have no conserve op — the nearest tiny-seq analogue).

| op | FArray | List | Vector | FArray/List | FArray/Vector |
|---|--:|--:|--:|--:|--:|
| construct | 29.7M | 22.2M | 6.9M | **1.34×** | 4.28× |
| foreach | 16.9M | 26.3M | 20.7M | 0.65× | 0.82× |
| foldLeft | 21.2M | 26.8M | 12.8M | 0.79× | 1.66× |
| map | 13.5M | 11.1M | 26.8M | 1.21× | 0.50× |
| mapConserve | 29.4M | 11.2M | 32.0M | **2.61×** | 0.92× |
| filterConserve | 26.7M | 37.1M | 21.5M | 0.72× | 1.24× |
| **destructure1** (`h +: t`) | 27.8M | 213.4M | 45.1M | **0.13×** | 0.62× |
| **destructure2** | 15.0M | 110.2M | 45.2M | **0.14×** | 0.33× |
| **destructure3** | 9.7M | 98.0M | 46.0M | **0.10×** | 0.21× |

Reading:

- **Construction: FArray already wins** (1.34× List, 4.3× Vector). The RefOne / small-RefArr build
  path beats cons and Vector. D1's construction motivation is therefore *weak on the mix* — the
  place FArray builds tiny sequences is already ahead of List.
- **Traversal: the List-tax is real but modest** — foreach 0.65×, foldLeft 0.79× of List under the
  megamorphic mix (map/mapConserve/filterConserve are at-or-above List). This is the ~2–7pp
  end-to-end gap's collection-machinery component.
- **Destructuring is the catastrophe: 7.7–10× slower than List, and it degrades with depth**
  (27.8M → 15.0M → 9.7M as List holds 213M → 110M → 98M). List's `::` tail is two field reads and
  zero allocation; FArray pays a `SliceNode` alloc **and** a `kindAt` head-read per level, and the
  tower compounds. **This single op family is where FArray loses the workload**, and it is exactly
  the SliceNode tax §2 quantified.

## 4. The D2b-vs-D1 recommendation (Phase 2)

The plan deferred the D2b/D1 ordering to this data. The data says **D2b (offset-leaf) first, and
treat D1 (Ref2) as a re-evaluation in Phase 3, if at all** — with a cheap D3 `RefOne`-peel landing
first in Phase 1. Reasoning, all evidence-based:

1. **The gap is destructuring, not construction.** Construction is already a *win* (1.34× List). The
   ≥7× loss is `case h +: t`, whose cost is `SliceNode` alloc + tower growth + head-read. **D2b
   attacks exactly this**: folding the offset into `RefArr` makes `tail`/`drop`/`take` of a leaf
   return a *flat leaf* (one alloc, no wrapper), which every engine already treats at full leaf
   speed (the `SliceNode`-over-leaf fast arms exist), and — crucially — **kills the tower**, so
   2-/3-deep stop degrading super-linearly. D1's field leaves do nothing for the tower and little for
   construction (which already wins).
2. **D2b is megamorphism-safe; D1 is not.** The `jvm-inlining` finding: `TypeProfileWidth = 2`, so a
   site seeing ≥3 receiver types goes megamorphic and loses inlining/EA/unboxing. The histogram shows
   the hot `at` site already sees **RefOne + RefPrepend + Concat + RefArr + SliceNode** — 4-6 types,
   already past the cliff. **D2b adds no new leaf class** (RefArr gains a field) → no new receiver
   type. **D1 adds `Ref2`/`Ref3`** → 2 more receiver types at every match/`at`/leaf site, pushing
   more sites megamorphic — a net negative at the busiest site (38M events/compile) that could erase
   its own monomorphic-micro win. The plan's "megamorphic control" gate would be the arbiter, but the
   histogram already shows the site is saturated.
3. **The 63% RefOne mode is untouched by D1.** D1 is `Ref2`/`Ref3`; the dominant single shape
   (`RefOne`, 63% of ctors, 81% of `at`) is already a field leaf. The biggest *cheap* win is not D1
   at all — it is peeling `RefOne` at the `applyAt` inline surface (kills 30.9M/30.4M `kindAt` calls
   per compile). That is a Phase-1 D3 item; do it first and re-measure destructure1 (a chunk of its
   cost is the RefPrepend/RefOne head-read via `kindAt`).
4. **Honest limit:** even D2b leaves **one alloc per tail** (the offset leaf) vs List's zero — so
   D2b alone will not reach List parity on destructuring, only flatten the tower and remove the
   wrapper indirection. Closing the last gap to List may require the `+:`-match to avoid
   re-wrapping entirely (e.g. an extractor that returns an offset-view without allocating, or
   accepting FArray stays ~1.5-2× on pure destructure while winning everything else). The D2 gate
   ("≤1 small alloc, within 1.5× of List") is the right target; the baseline is ~7-10× off, so this
   is the load-bearing phase.

**Sequencing confirmed:** Phase 1 = D3 `RefOne`/length-1 `applyAt` peel + the D5 worklist check
(cheap, high count). Phase 2 = **D2b offset-leaf** (the destructuring/SliceNode tax — the measured
#1 gap). Phase 3 = re-evaluate D1 `Ref2` *only if* the suite still shows a construction/traversal
gap after D2b, gated hard on the megamorphic `at`-site control. Re-run this suite after each phase;
it is the acceptance metric.

## 5. Phase 1 results (implemented; this section supersedes §3 as the Phase-2 baseline)

Phase 1 landed three changes (branch `dotty-gaps`):

1. **Item 1 — RefOne peeled in the `applyAtImpl` inline fast path.** The §1 top cell
   (`at`·RefOne·1, 30.9M/compile) now resolves inline: a second arm
   `else if (xs.isInstanceOf[${K}One]) xs.asInstanceOf[${K}One].elem` before the out-of-line
   megamorphic `${k}At`. One edit covers every surface (`apply`/`head`/`last`/`_1` all funnel
   through `applyAtImpl`). JMH gate (`IndexedApplyPollutedBenchmark`, extended with a `refOneHot`
   case — RefOne read under full 5-shape refAt pollution; `-f 2 -wi 3 -i 5`, size=3):
   **refOneHot 27.5M → 44.3M ops/s (1.61×)**; refArrHot 18.96M → 18.66M (parity, within noise);
   mixedShapes 50.8M → 45.7M (−8%: the structural-node minority pays one extra failed instanceof —
   RefOne is 63% of reads vs ~10% structural, strongly net-positive). Bytecode growth: +17–19
   bytecodes per `applyAtImpl` splice (vs the 8000 HugeMethodLimit; a consumer would need ~450
   apply sites to be pushed over).
2. **D3 audit (evidence-scoped to ops ≥1% of histogram events).** The hot sites are `at` (47.5%),
   `ctor` (19.7%), `fold` (15.3%), `foreach` (10.0%), `map` (5.7%), `dfs` (1.0%) — `filter` (0.9%)
   was included as borderline. Verified in the generated code: `reduceLeaf*`/`foreachLeaf*`/
   `mapLeaf*`/`filterLeaf*` all peel `Empty`/`${K}One`/top-leaf/Slice-over-leaf INLINE at entry
   before any engine setup — **no misses; no changes needed**. The plan's flagged unpeeled ops
   (sort/groupBy/partition/diff/intersect/toSet/distinct/toArray/scan/mkString/lastIndex*) were
   re-checked and SKIPPED: all are <1% of events in both corpora, and in fact all of them already
   carry length-0/1 (or length<2) guards at their inline surfaces (verified: `mkStringImpl`,
   `toArrayImpl`, `distinctImpl`, `partitionImpl`, `sortWithImpl`/`sortedImpl`/`sortByImpl`,
   `scanLeft/RightImpl`, `groupByImpl`, `lastIndexWhere/OfImpl`, `diff`, `intersect`, `toSet`) —
   the research doc's 127-loss list predates rounds 7–8.
   The audit did surface one real bug, fixed and tested: **`apply(i)` out of range silently
   returned a value instead of throwing** on every shape whose read arm ignores the index
   (RefOne/Prepend/Append/Updated/Pad/structural recursion) and on slack-backed leaves —
   `FArray("x")(5)` returned `"x"` where `List("x")(5)` throws. Fixed with a logical-length bounds
   check at the two public surfaces: the specialized `apply` extension, and `FBase.applyBoxed`
   (now a final checked entry delegating to `applyBoxedUnchecked`, so the check re-runs at every
   recursion level and validates against the LEAF's logical length when the walk lands on one).
   `head`/`last`/internal engines keep the unchecked `applyAtImpl` path. Cost on the item-1 gate:
   none (refOneHot 44.9M unchecked vs 44.3M checked, within noise).
3. **D5 worklist-parity verification (measurement only).** New `worklist_*` cases in
   `CompilerShapeBenchmark`: build a 10-item accumulator by `+:` cons, drain it fully by
   `case h +: t`. Tightened run (`-f 2 -wi 5 -i 8`):

   | | FArray | List | Vector |
   |---|--:|--:|--:|
   | worklist (cons-build + uncons-drain) | 36.65M ± 2.5M | 36.78M ± 0.5M | 6.93M ± 1.0M |

   **Verdict: exact List parity (1.00×; 5.3× Vector).** Prepend-chain build+destructure is free
   post-round-8 (`tail = base`, no alloc). This CONFIRMS the §3 destructure catastrophe is
   EXCLUSIVELY the flat-scrutinee/SliceNode case — D2b's scope is exactly right, and no extractor/
   isEmpty/length-maintenance gap exists on the cons-shaped path.

### Post-Phase-1 CompilerShapeBenchmark baseline (the table Phase 2 is measured against)

Same suite/method as §3 but `-f 2` (two forks; §3 was `-f 1`), JDK 26, macOS aarch64, quiet box.
Absolute numbers are not directly comparable to §3 (different day/forks); ratios are.

| op | FArray | List | Vector | FArray/List | FArray/Vector |
|---|--:|--:|--:|--:|--:|
| construct | 29.1M | 21.6M | 7.1M | **1.34×** | 4.08× |
| foreach | 14.2M | 25.2M | 20.1M | 0.56× | 0.71× |
| foldLeft | 19.9M | 25.7M | 11.4M | 0.77× | 1.76× |
| map | 11.8M | 10.6M | 23.6M | 1.11× | 0.50× |
| mapConserve | 30.3M | 10.1M | 28.0M | **3.01×** | 1.08× |
| filterConserve | 29.5M | 32.7M | 20.8M | 0.90× | 1.42× |
| **destructure1** (`h +: t`) | 24.4M | 180.7M | 44.9M | **0.14×** | 0.54× |
| **destructure2** | 12.7M | 101.4M | 44.7M | **0.13×** | 0.28× |
| **destructure3** | 8.1M | 76.7M | 37.6M | **0.10×** | 0.21× |
| worklist (D5) | 34.6M | 31.5M | 7.8M | **1.10×** (tightened: 1.00×) | 4.43× |

Reading, vs §3: construct ratio unchanged (1.34×), conserve ops improved (mapConserve 2.61→3.01×,
filterConserve 0.72→0.90×), fold ~unchanged (0.77×), foreach 0.65→0.56× (no mechanism links Phase-1
changes to foreach — it never touches `applyAtImpl` — treat as environment variance between the two
sessions). **Destructuring is still the catastrophe (0.10–0.14×), as predicted**: the `faD`
scrutinees are ≥3-element flat/structural shapes with no RefOne among them, so the item-1 peel
cannot move this op family — its cost is the SliceNode alloc + tower, which is exactly D2b's target.
The worklist row proves the tax is confined to flat scrutinees. **Phase 2 (D2b) is measured against
THIS table.**
