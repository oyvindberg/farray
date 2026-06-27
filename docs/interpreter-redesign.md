# Traversal interpreter redesign — one node-arm spec, two lowerings

Status: **design, settled.** Behavior-preserving. Correctness is already *proven* by `FExhaustiveTest`
(46,773 shapes × 2 kinds = 6.8M op-comparisons, 0 wrong, 0 hang), so the hard part — the windowed
deep-base sub-traverse — is **written and certified**. The work is de-duplicating the *easy* (iterative)
half without disturbing the *hard* (recursive) half, with the harness as the contract at every step.

## Priorities (in order)
1. **Correct** — `FExhaustiveTest` green at every step; bump it to depth 4 once for headroom.
2. **Fast** — no regression on the structural ops or the leaderboard (FArray 1.12 geomean).
3. **Understandable** — the walk fits on a page; one source of truth.
4. **Non-duplicative** — collapse the **8 emitter functions** (`dfsBody` + 7 traversers, the same iterative
   walk re-written 8×, ~280 lines each) into **one** parameterized emitter.

## Settled structural decisions
- **No full recursion of the main walk** — off the table, for perf. The explicit stack stays.
- **Main walk = iterative explicit stack.** Most of any tree is descended trivially; the stack only holds
  deferred `Concat` rights and `Append`/`Prepend` elements.
- **`ReverseNode` = call the opposite-direction method** (JVM stack). The *only* recursion in the main
  walk; bounded (reverse-of-reverse collapses); gives near-optimal code.
- **`Append`/`Prepend` deferral = wrap-in-`One`** on the single `FBase[]` stack — `tail[]`, `isTail[]`,
  `ensureTail`, `clearTail`, and the pop-loop discriminator are deleted. One `${K}One` alloc per `Append`
  per traversal (cheaper than two arrays for the common 1–2 appends; only a deep `:+` spine pays).
- **Windowed deep-base sub-traverse = recursive** (the proven core, kept as-is). Recursion is *why* it is
  tractable: each composite node recurses into its children with a **child-local sub-window computed from
  `child.length`**, so `skip`/`take` never thread back across a call boundary. Depth = tree depth, hit only
  on a deep-base `Slice`/`Updated` (rare), bounded for any balanced tree. Going iterative there would
  re-open skip/take-threading-through-the-stack — the bug-prone path we are *not* taking.

## The node algebra (unchanged)
`Empty`, `${K}One`, `${K}Arr` (leaf), `Concat`, `${K}Append`, `${K}Prepend`, `SliceNode`, `${K}Pad`,
`${K}Updated`, `ReverseNode`, `RangeNode`. Invariants: leaf `arr.length == length`; no length≤1 structural
nodes; a `Slice`/`Pad`/`Updated` whose base is a leaf is a *direct run*, never windowed.

## One node-arm ORDER, two lowerings
The single spec is the **visit order per node** (the segment decomposition):
```
fwd:  leaf run↑ · Concat l→r · Append base→elem · Prepend elem→base · Pad base→fillers · Updated pre→elem→suf
bwd:  the exact mirror (children swap, runs descend, Append↔Prepend swap which side defers)
```
Lowered two ways from that one description:

**(1) Iterative main walk** — the full, un-windowed traversal. One `FBase[] stack`, `cur` cursor:
```
leaf          → run the backing array (asc fwd / desc bwd)
Concat        → push right; cur = left            (bwd: push left; cur = right)
Append (fwd)  → push One(elem); cur = base         Prepend (fwd) → act elem; cur = base
Append (bwd)  → act elem; cur = base               Prepend (bwd) → push One(elem); cur = base
Reverse       → cur-state = <opposite-direction method>(rev.base, …)   ← the only recursion
Slice/Updated/Pad over a NON-leaf base → winCall into the windowed lowering (below)
Range         → computed loop
advance       → cur = next, else pop the stack (a popped One acts as an ordinary node)
```

**(2) Recursive windowed walk** — the deep-base sub-traverse over a `(skip, take)` window:
```
each composite node → ordered SEGMENTS, each segment one of:
  recurSeg(child, child.length)  → recurse with a child-local sub-window; skip/take collapse after the
                                    first partially-covered segment; early-return the instant take==0
  elemSeg(e)                     → skip-- or act(e); take--
  fillerSeg / leaf run           → clipped run over the window
Slice    → sliceDeep(base, off, len)                         (one window)
Updated  → sliceDeep(base, 0, ui) ; elem ; sliceDeep(base, ui+1, …)   (prefix · elem · suffix)
Reverse  → call the OPPOSITE-direction window with the SAME skip/take (window lands in the reversed frame)
```

## Method signatures
```
<ret> <shape>{Fwd,Bwd}${kinds}(FBase root, <state…>, <fn>)                       // iterative main walk
<ret> <shape>Window{Fwd,Bwd}${kinds}(FBase node, int skip, int take, <state…>, <fn>)  // recursive windowed
```
- `skip`/`take` are a window **in the method's own direction frame** — fwd from the start, **bwd from the
  end**. Not absolute indices: that is precisely why direction flips with no re-indexing.
- `Fwd`/`Bwd` are **mutual**; `ReverseNode` calls the other with the *same* `skip`/`take`.
- `state` threads through the **return value**, so each parent recomposes its child sub-window from
  `child.length`. No skip/take threaded across the stack.

## The one genuinely-evil line — and where it lives
A slice's `off`/`len` are *forward* semantics. Entering a **backward** traverse, the base-window skip is:
```
bwd base skip = base.length - off - len  (+ local skip)
```
e.g. `take(4)` = `Slice(base, 0, 4)`; in a bwd traverse the deep base is `windowBwd(base, baseLen-4, 4)` →
`base[3],base[2],base[1],base[0]` = `take(4)` reversed. This flip appears in exactly **two** places that
must agree — `winCall` (main-walk hand-off) and `sliceDeep` (inside the recursion, `_bl - _so - _sn + _ss`).
The `+skip`/`+_ss` term is the local window composing on top, so slice-of-a-slice and `take(4).reverse`
both fall out. `FExhaustiveTest` pounds exactly this (slice-over-reverse, nested windows, deep updates).

## Part A — the codegen collapse (zero perf risk, no blockers)
The 8 emitter functions each re-write the iterative walk. Collapse to **one** parameterized `WalkSpec`
emitter — generalize the already-single-sourced `WinSpec`/`emitWindow` pattern to the main walk. Per-shape
knobs: `act` (per-element step), `state`/`ret`, `earlyStop` (ShortCircuit), `writeConv` (Scan — separate
from visit dir), `output` (Build/BuildFiltered/Partition write an array; Reduce/Foreach/SC don't). **The
emitted `Traversers.java` is byte-identical (or trivially equivalent)** — proven by a generated-output diff
+ the harness. Pure source refactor.

## Part B — wrap-in-One (the only runtime change)
`Append`/`Prepend` deferral → `stack(sp) = new ${K}One(elem)`; pop loop becomes uniform `cur = stack(sp)`.
This *does* change the generated walk, so it is harness-verified (not a no-op diff). No recursion change.

## Migration — incremental, harness-locked
One emitter at a time into the shared `WalkSpec` (Reduce → Build → BuildFiltered → Partition → Foreach →
Scan → ShortCircuit), the spec and the old templates coexisting. At each step: `bleep test tests` green +
a diff of the regenerated `Traversers.java` for that shape (Part A must be identical). Then Part B as a
distinct, harness-verified commit. Re-run the leaderboard at the end — no red regressions.

## Decide-during (the harness covers us)
- **Scan**'s `writeConv` knob is mandatory (the AIOOBE we already fixed) — visit dir ⟂ write dir.
- **Iterator** lowering: reify the same walk as a pull frame-stack, *or* keep the materialize-then-cursor
  iterator. Feasibility settles when the `WalkSpec` exists; not on the critical path.
- **Empty/One fast-path peels** (loss-audit's 24 ops) fold into the `WalkSpec` *surface* generation.
- **Cross-prim map traversers** stay a separate kind-coverage pass.
