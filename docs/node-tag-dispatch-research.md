# Node dispatch: `instanceof`-chain vs `final int tag` + `tableswitch`

Research question: the `FBase` node hierarchy (~11 concrete types — `Concat`,
`Append`, `Prepend`, `SliceNode`, `ReverseNode`, `Pad`, `Updated`, `${K}One`/
`Empty`, `${K}Arr`, `RangeNode`) is dispatched everywhere via Scala `match` on
subtype, which the compiler lowers to a chain of `instanceof` checks
(`atDef`/`${k.lc}At` and `dfsBody` in `codegen/src/scala/farray/GenCores.scala`).
Would giving every node a `final int tag` constant and dispatching via an integer
`match` (a `tableswitch`) be cheaper, and would it close the size-1 read floor?

Verdict: **No, do not convert.** Int-tag dispatch is *slower* on the exact site
that motivated the question (the monomorphic size-1 `applyAt` leaf read), and only
wins on the megamorphic dfs/op walk by ~15%, which is not enough to justify the
codegen complexity and an extra field. The size-1 floor is **not** a dispatch-depth
problem and a `tableswitch` does not close it.

## What the current code actually does

- `FBase` is **already** `sealed abstract` (Java `sealed ... permits`, GenCores
  line 564) and **already** has `final int length`. So the JIT is working with a
  closed type set and a known-final field, not an open hierarchy.
- `${k.lc}At` (line 178) order: **leaf first** (`${K}Arr`), then `Concat`,
  `Append`, `Prepend`, `ReverseNode`, `Pad`, `Updated`, `SliceNode`, `RangeNode`.
  So a single-element read on a flat leaf hits **case 1** — already best-case for an
  instanceof chain.
- `dfsBody` (line 124) order: leaf, `Prepend`, `Append`, `Concat`, `Reverse`,
  `Pad`, `Updated`, `Slice`, `Range`. This is the megamorphic op/traversal walk.
- `applyAtImpl` is `inline` and calls `${k.lc}At`, a non-inlined self-recursive
  method.

## Method

Throwaway JMH benchmark `benchmarks/.../NodeTagDispatchBenchmark.scala` (delete
after). An ~11-case hierarchy (`TNode` + `NArr/NConcat/.../NEmpty`), each node
carrying both its subtype *and* a `final int tag`, so the **same objects** are
dispatched both ways:

- `viaInstanceOf` — Scala `match` on subtype, case order mirroring the real
  `${k.lc}At`. Confirmed via `javap`: **11 `instanceof` ops** (a chain).
- `viaTag` — `match` on `node.tag`. Confirmed via `javap`: a **`tableswitch` 0..9**
  + default (so yes, Scala 3 lowers a dense int match to a real `tableswitch`).

Sites measured:
- **monomorphic** single dispatch (the size-1 `applyAt` case), with the hit landing
  at chain depth 1 (`leaf`, tag 0), depth 8 (`slice`, tag 7), depth 11 (`empty`,
  tag 10) — to see whether instanceof cost grows with depth;
- **megamorphic** tight loop over 1023 shuffled nodes covering all 11 types (the
  dfs/op walk; the call site is truly megamorphic, no devirtualization possible).

JDK 25 (GraalVM CE), `-f 2`, 5×1s warmup + 5×1s measurement, AverageTime ns/op,
`-Xms2g -Xmx2g -XX:+UseParallelGC`. macOS aarch64 dev box (non-server-tuned).

## Results (ns/op, lower is better)

| Benchmark                  | instanceof | int-tag | delta            |
|----------------------------|-----------:|--------:|------------------|
| mono leaf  (depth 1, tag 0)  | **0.463** | 0.556 | tag **+20% slower** |
| mono slice (depth 8, tag 7)  | **0.706** | 0.825 | tag **+17% slower** |
| mono empty (depth 11, tag 10)| 0.408 | **0.389** | tie (≈ noise) |
| mega (1023 mixed, 11 types)  | 1275 | **1084** | tag **−15% faster** (~0.19 ns/elem) |

Errors were tight (mono ±0.004–0.024, mega ±~20 on ~1100), so the orderings are
real, not noise — though the mono numbers are sub-nanosecond and near the
measurement floor, so treat the *magnitude* there as indicative, not exact.

## Analysis

1. **HotSpot already collapses the monomorphic sealed-type match.** When the call
   site sees one concrete type, the JIT proves the type and the instanceof chain
   evaporates — depth is irrelevant. Evidence: `mono_empty` (the *last*, 11th case)
   is the *fastest* of the three, and `mono_leaf` (1st case) is not meaningfully
   cheaper than `mono_slice` (8th case) in a way that tracks depth — the ~0.24ns
   spread between leaf and slice is the extra **field load** (`SliceNode.base ->
   payload`), not chain traversal. So "the hit lands deep in the chain" is **not**
   what costs `applyAt` on a tree; chasing the indirection is.

2. **Int-tag is slightly slower when monomorphic**, because it forces an *extra
   memory load* (read `node.tag`) plus the `tableswitch` indirection, where the
   instanceof version was already folded to nothing. The tag field is pure overhead
   at a site the JIT already optimized.

3. **Int-tag wins ~15% only at the megamorphic site**, because there the instanceof
   chain genuinely runs N `instanceof` tests per element with unpredictable branch
   outcomes, whereas `tableswitch` is one indirect jump on a (still well-predicted)
   tag value. ~0.19 ns/element saved across the mixed walk.

4. **The size-1 read floor is not a dispatch problem.** Reading one element through
   the node match costs more than a flat `arr(0)` because of (a) the non-inlined
   `${k.lc}At` call boundary and (b) for tree nodes, the pointer-chase through
   `base`/`left` to reach the backing array — **not** because of instanceof depth.
   A `tableswitch` changes neither; it *adds* a field load. The benchmark shows
   tag is *worse* on exactly the monomorphic-leaf case the floor is about.

## Recommendation

**Don't** convert `FBase` to int-tag dispatch.

- **`applyAt` / size-1 single read (the floor): DON'T.** Monomorphic; the JIT
  already removes the chain; tag is measurably *slower* here and would not close the
  floor. To attack the floor, look elsewhere: a direct fast-path on `${K}Arr`
  (read `leaf.arr(i)` without entering `${k.lc}At` — the leaf already lands first,
  but it still pays the non-inlined call), or `@inline`/specialization of the leaf
  read at hot call sites. Dispatch shape is not the lever.
- **dfs / op-match (megamorphic): HOT-SITES-ONLY, and probably still not worth it.**
  The ~15% dispatch win is real but it is 15% *of the dispatch*, and the dfs body
  already does most of its work in per-leaf/per-run inner loops (`onLeaf(arr, len)`),
  so the per-node match is a small fraction of total dfs time. Real-world impact on
  end-to-end op benchmarks would be a small single-digit % at best, against the cost
  of: an extra `int tag` field on every node (8 bytes with alignment, on objects
  that are often tiny like `${K}One`), codegen complexity (every `match` in GenCores
  must be rewritten to `tag match` + `asInstanceOf` downcasts, losing exhaustivity
  checking and readability), and the risk of regressing the many monomorphic sites
  (leaf-fast-path matches all over the ops) that the JIT currently folds for free.

**Risks if pursued anyway:** added field memory (worst on the smallest nodes);
loss of Scala's sealed-match exhaustivity safety net (tag matches need a manual
`case _`); every downcast is now unchecked `asInstanceOf`; and net regression on the
monomorphic majority of call sites. The only defensible target would be a *single*
genuinely-megamorphic, dispatch-bound hot loop — and the dfs isn't dispatch-bound.

## Bottom line

Int-tag `tableswitch` is **not** cheaper where it matters. It is ~15% faster on a
pure megamorphic dispatch loop, ~17–20% *slower* on the monomorphic size-1 read, and
it does **not** close the size-1 floor (that floor is call-boundary + pointer-chase,
not instanceof depth). HotSpot already optimizes the sealed-type match on
monomorphic sites, which are the majority. Keep the `match`-on-subtype dispatch.
