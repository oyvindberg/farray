# FArray traversal interpreter — a precise map

READ-AND-ANALYZE reference for the redesign. Priorities (in order): provable correctness,
performance, simplicity/understandability, minimal duplication.

Sources:
- Generator: `codegen/src/scala/farray/GenCores.scala` (4725 lines).
- Concrete reference (generated): `.bleep/generated-sources/farray/farray.GenCores/farray/Traversers.java`
  (38 970 lines, 180 static methods) + `…/FArrayOps.scala` (2599 lines) + the per-node `*.java`.

There are **two physically separate traversal engines** in this codebase:
1. **The Scala inline push emitter** — `dfsBody` (GenCores 204–291), spliced inline by `lower`
   (482–558). Used only by Wave-1 ops left on the "old lowered walk": cross-prim folds, scan with
   cross-prim acc, and a few fallbacks. Also the non-inline `dfsC{K}Fwd/Back` pair (297–313) used
   ONLY by `materialize` / `flatMapCopyOne`.
2. **The Java push traversers** — `Traversers.java`, emitted by seven per-shape emitters
   (GenCores 2092–~4090). This is the primary engine for the hybrid surface (fold/map/filter/
   foreach/scan/short-circuit/partition over genuine trees).

Both engines implement the SAME walk (same node algebra, same lazy explicit stack, same
ReverseNode-as-JVM-recursion rule). The duplication is the headline finding (§6).

---

## 1. THE NODE ALGEBRA

`FBase` (`FBase.java`) is a `sealed abstract class` with `public final int length` and abstract
`applyBoxed(i)`, `take`, `drop`, `slice`, `reverse`, `init`. 49 permitted subclasses. Emitted by
GenCores (FBase permits list at FBase.java:4; node templates at GenCores `oneNode`, `primCore`,
`primAppend`, `primPrepend`, `padNode`, `updatedNode`, `concat`, `sliceNode`, `reverseNode`,
`rangeNode`, `emptyNode`).

Kinds (`opKinds`, GenCores 116): the FOUR specialized op-kinds are **Int, Long, Double, Ref**.
But the *node* prims (`prims`, GenCores 18) are **Int, Long, Double, Float, Short, Byte, Char,
Boolean** + Ref — so there is a `{K}Arr/{K}One/{K}Append/{K}Prepend` for all 8 prims + Ref, while
`{K}Pad/{K}Updated` and all traversers exist only for {Int,Long,Double,Ref}. (Float/Short/Byte/
Char/Boolean leaves are traversed by being materialized or read via `applyBoxed` — they have no
specialized traverser.)

### Leaf nodes
- **`{K}Arr`** (e.g. `IntArr.java`): holds `{prim}[] arr` + inherited `length`.
  **Invariant: `length >= 2`** (length 0 → `Empty`, length 1 → `{K}One`), and the backing array's
  usable region is `[0, length)`. NOTE: `arr.length` may be **larger** than `length` (a trimmed
  `copyOf` is avoided where possible) — the traversers deliberately loop on `a.length` in the
  WHOLE-leaf fast path (`runLeaf`) to let HotSpot drop the bounds check, but on `leaf.length` for
  windowed runs. So **`arr.length == length` is NOT guaranteed**; only `length <= arr.length`.
  (See `runLeaf` note, GenCores 2112–2114.)
- **`{K}One`** (`IntOne.java`): holds one unboxed `elem`, `length == 1`. No backing array.
- **`Empty`** (`Empty.java`): the single `Empty.INSTANCE` singleton, `length == 0`, kind-less.
  `applyBoxed` throws.

### Unary structural nodes (base shared, O(1))
- **`{K}Prepend`** (`IntPrepend.java`): `elem` then `base`; `length == base.length + 1`.
- **`{K}Append`** (`IntAppend.java`): `base` then `elem`; `length == base.length + 1`.
- **`{K}Pad`** (`IntPad.java`): `base` then `(targetLen − base.length)` copies of constant `filler`;
  `length == targetLen >= base.length`. (padTo.)
- **`{K}Updated`** (`IntUpdated.java`): `base` with element `index` overridden to `elem`;
  `length == base.length`; `0 <= index < length`.
- **`SliceNode`** (`SliceNode.java`): kind-AGNOSTIC window `base[offset, offset+length)` over any
  base. Holds `base`, `offset`. (take/drop/slice over a leaf produce this.)
- **`ReverseNode`** (`ReverseNode.java`): kind-AGNOSTIC `base` reversed; `length == base.length`.
  `applyBoxed(i) = base.applyBoxed(length-1-i)`. `reverse()` returns `base` (reverse-of-reverse
  collapses — so nesting depth is ~0–1; this is what bounds the JVM recursion, §2).

### Binary node
- **`Concat`** (`Concat.java`): `left` ++ `right`, `length == left.length + right.length`. Holds the
  shared `Concat.one(node, i)` canonicalizing length-1 factory and `elementwiseEquals`/`render`.

### Closed-form node
- **`RangeNode`** (`RangeNode.java`, Int only): `start`, `step`, `length`; element `i` =
  `start + i*step`. No backing array. Reverse is another RangeNode (negated step).

### Canonicalization invariants (upheld at every smart constructor)
- **No length-0 structural node** — collapses to `Empty.INSTANCE`.
- **No length-1 structural node** — collapses to `{K}One` (via `Concat.one(node,i)` when the kind
  must be recovered from a kind-agnostic node). Every `take/drop/slice` in the `*.java` nodes and
  every Scala `leaf(k, arr, len)` builder (GenCores 452) enforces this.
- **`FBase.concat`** drops an empty side (`that.length==0 → this`, etc.).
- **Empty has no kind**; `{K}One`/`{K}Arr` carry the kind. The op surface recovers the kind by
  `summonFrom` on `{K}Repr[A]` (the opaque-type witness), NOT from the node — so an `Empty` input
  is handled by the kind the *call site* summoned.
- The traversal engines RELY on these: e.g. the windowed segment machinery computes child sub-windows
  from `child.length`, and the leaf fast paths assume a top-level `{K}Arr` is `length >= 2`.

---

## 2. THE WALK SKELETON (the iterative DFS)

The canonical reference is the **Java** form in `reduceTraverser` (GenCores 2092–2369); the Scala
`dfsBody` (204–291) is the same shape in Scala-template form. One direction-aware driver; the
forward and backward bodies are **exact mirrors** selected by a `backward: Boolean`.

### State (per traverser invocation)
```
FBase cur = root;          // the node currently being decomposed
FBase[] stack = null;      // LAZY: deferred Concat children / structural children
{prim}[] tail = null;      // LAZY: deferred single elements (Append fwd / Prepend bwd)
boolean[] isTail = null;   // LAZY: parallel flag — is stack slot sp a tail element?
int sp = 0;                // stack pointer
+ shape-specific state: acc / out+o / oa,ob / cum / nothing
```
`stack` is allocated (`new FBase[16]`) only on the **first** Concat/structural defer; `tail`/`isTail`
are allocated (sized to `stack.length`) only on the **first** single-element defer. A bare-leaf or
pure-Concat walk therefore pays at most ONE array. `ensureStack` grows all three in lockstep
(`copyOf` to `sp*2`) so the parallel indices stay aligned. (GenCores `ensureStack`/`ensureTail`/
`pushTail`/`pushChild` 2189–2221; Scala equivalents at 229–233.)

### Driver loop
```
while (cur != null) {
  FBase next = null;
  <dispatch on cur's runtime type — the node arms below>   // sets `next` to descend, or runs a leaf
  if (next != null) cur = next;
  else { cur = null; while (sp > 0 && cur == null) { sp -= 1;
            if (isTail != null && isTail[sp]) <act on tail[sp]>   // deferred element
            else cur = stack[sp]; } }                              // deferred subtree
}
return <shape state>;
```
The pop loop reads a slot as a **tail element** only when `isTail` exists AND the flag is set; else
as a deferred subtree. A stale `isTail[sp]=true` left by a previous pop is cleared by `pushChild`
(GenCores 2216–2218; Scala `clearTail`, 246).

### Per-node arms (forward / backward mirror)
Concrete per node (Java `reduceTraverser` 2231–2348; Scala `dfsBody` 235–279):

| Node | Forward | Backward |
|---|---|---|
| `{K}Arr` | `runLeaf(arr)` ascending | `runLeaf(arr)` descending |
| `{K}One` | act on `elem` | act on `elem` |
| `{K}Prepend` | act `elem`; `next = base` | `pushTail(elem)`; `next = base` (defer elem) |
| `{K}Append` | `pushTail(elem)`; `next = base` (defer elem) | act `elem`; `next = base` |
| `Concat` | `pushChild(right)`; `next = left` | `pushChild(left)`; `next = right` |
| `ReverseNode` | **recurse** `reduceBwd(base,…)` then fold result | **recurse** `reduceFwd(base,…)` |
| `SliceNode` | leaf base → `run(arr, so, sn)`; deep base → `winCall(base, so, sn)` | leaf → `run(arr, so+sn-1, sn)` desc; deep → `winCall` |
| `{K}Pad` | base (leaf `run`/deep `winCall`) then filler run | filler run then base |
| `{K}Updated` | leaf: `run[0,ui)`, act elem, `run(ui+1,…)`; deep: `winCall`s around elem | mirror |
| `RangeNode` (Int) | ascending closed-form loop | descending |
| `_` (Float/Short/… leaves w/o traverser; only in Scala dfsBody) | `case _ => ()` | — |

### Leaf-run loop
`runLeaf(arr)` (GenCores 2115) loops on `a.length` (NOT `leaf.length`) to drop the bounds check and
vectorize — valid because the act applies to every backing slot of a true leaf (length is the array's
usable length; for a top-level leaf the surface peels it, so this is only hit for tree-internal
leaves where `arr.length == length`). `run(arr,start,count)` (2133) is the **windowed** run on
`a[start, start±count)` used for Slice/Pad/Updated leaf bases (here it loops on `start+count`, so it
respects `length`). Both have an ascending (fwd) and descending (bwd) form — a mirror pair.

### Direction-flip JVM recursion at ReverseNode
ReverseNode is the **only** recursion. Forward `reduceFwd` calls `reduceBwd(rev.base,…)` (and vice
versa); the direction flip rides the JVM call stack, and each direction keeps its OWN local explicit
stack (a reversed subtree is self-contained and never spills into the caller's stack). Because
`ReverseNode.reverse()` collapses reverse-of-reverse, literal nesting depth is ~0–1, so the JVM
recursion depth is bounded by the number of *non-collapsed* reverse layers actually present in the
tree (in practice 0–1). The Scala inline `lower` form has no driver to call, so its ReverseNode arm
is **self-contained** (`onRev`, GenCores 506–510): reverse-of-leaf = an opposite-direction window
inline; reverse-of-non-leaf = `materialize{K}` then an opposite-direction run.

### Early-stop (ShortCircuit only)
The `scTraverser` (3800–~4090) is the only shape that abandons the walk early: each leaf/element arm
does `return <globalIndex>` on a hit (discarding the explicit stack entirely). It threads a
cumulative count `cum` to map a local hit to a global index and clamps each leaf by `from`/`end`. The
ReverseNode recursion ENCODES "no hit, new cum = C" as `~C` (negative) and a hit as the index (>=0);
the caller decodes `r >= 0 ? hit : cum = ~r` (GenCores 3796–3799). The other shapes have no early
stop and always run to completion. (The Scala `dfsBody` has an unused `Exit.OnPredInCond` slot,
171–174, structured but not consumed by `lower`.)

---

## 3. THE WINDOWING (deep-base Slice/Pad/Updated)

When a `SliceNode`/`{K}Pad`/`{K}Updated` has a **deep (non-leaf)** base, the main driver cannot run a
flat array loop. The old approach read per-index via `applyBoxed` (the `runBoxed` helpers, still
present at 2152/2446/etc. but superseded). The current approach is a **(skip,take)-windowed
recursive sub-traverser** emitted ONCE PER SHAPE PER DIRECTION by `emitWindow` (1873–2050), driven by
a `WinSpec` (1840–1861) that each shape constructs.

### WinSpec
A record (1840) capturing exactly what differs between shapes: `name`/`other` (this/opposite-dir
windowed method), `tparam`, `retT`/`retState`, `stateDecl`/`stateArgs`/`fnArgs`, `backward`,
`capture` (how to fold a recursive sub-call's return into state, e.g. `v => "acc = $v;"`), `act`
(per-element action), `actRun` (leaf sub-run), `isInt`, `prelude`. The walk structure is entirely in
`emitWindow`; the WinSpec is the per-shape "hole filling."

### The windowed method
`{ret} {name}(FBase node, int skip, int take, <state>)` — visits `node` but applies the action only
to `[skip, skip+take)` **in this direction's frame** (a Bwd window counts `skip` from the END),
early-returning when `take` hits 0. It is **RECURSIVE, not stack-iterative** — each composite node is
decomposed into ordered SEGMENTS and each segment recurses with a child-LOCAL sub-window computed
from the child's KNOWN length. So skip/take never thread back across a call boundary; each parent
recomputes its own from `child.length`. (Rationale: the explicit-stack iterative walk can't carry the
collapsing window cheaply.)

### Per-segment consumers (emitWindow 1884–1917)
- **`recurSeg(child, clen, childCSkip)`**: a child node occupying the next `clen` positions. Computes
  `_ss = min(skip, _L)`, `_st = min(_L - _ss, take)`; if `_st > 0` recurse with `(childCSkip(_ss),
  _st)`; then `skip -= _ss; take -= _st`. This is how skip/take **thread + collapse**: once a segment
  is partially covered, `skip` drops to 0 for all following segments.
- **`elemSeg(elem)`**: one element — `if (skip>0) skip-=1 else { act(elem); take-=1 }`.
- **`fillerSeg(filler, flen)`**: a run of identical Pad fillers, clipped to the window.
- **`stop()`**: after each segment, `if (take<=0) return`.

### Node decomposition inside the window (emitWindow 1923–2047)
- Leaf / RangeNode: a direct clipped run via `actRun` (fwd `wa[skip, skip+n)` / bwd
  `wa[c-1-skip, …]`).
- Prepend/Append/Concat/Pad: ordered `elemSeg`/`recurSeg`/`fillerSeg` segments in visit order
  (fwd left→right / base→fillers; bwd mirrors), each followed by `stop()`.
- SliceNode: leaf base → direct clipped run; deep base → `sliceDeep` (2056–2070), which expresses
  the base as a this-direction window of `base[off, off+len)` (fwd base-pos `off+skip`; bwd
  reversed-pos `baseLen-off-len+skip`). **Updated** prefix/suffix are also SLICES of base, so they
  reuse `sliceDeep` (deep base) or `updLeafRun` (2074–2084, leaf base) — one composition rule covers
  Slice and Updated.
- **ReverseNode inside a window**: recurse into the OPPOSITE-direction windowed method with the SAME
  skip/take (`recurOtherExpr`, 1859; arm at 1966–1967). The window then lands in the reversed frame —
  the "same-skip/take opposite-direction bounce." This is the windowed analogue of the main driver's
  ReverseNode JVM recursion.

### How the main driver calls in
`winCall(node, lo, len)` (per shape, e.g. 2184–2186) calls the windowed method with fwd skip = `lo`,
bwd skip = `node.length - lo - len` (the reversed-frame offset). So the deep-base Slice/Pad/Updated
arms in the MAIN walk delegate to the windowed recursion; leaf bases stay on the flat `run`.

---

## 4. THE SHAPES (6 shapes reuse the skeleton)

Each shape is one per-element ACTION + one return convention layered over the identical skeleton +
windowing. Each lives in its own emitter and re-emits the whole skeleton (this is the duplication,
§6). The shapes and their generated method families (counts from Traversers.java):

| Shape | Emitter (GenCores) | SAM type(s) | Per-element action | Return | Methods |
|---|---|---|---|---|---|
| **Reduce** (fold/foldRight/reduce/sum/product/count) | `reduceTraverser` 2092 | `{K}To{K}Fold` (prim acc) / `{K}ToRefFold<Z>` (ref acc) | `acc = f.apply(acc, e)` | `acc` | `reduce{Fwd,Bwd}{K}{K\|Ref}` |
| **Build** (map/mapConserve) | `buildTraverser` 2378 | `{I}To{O}Fn`/`{I}ToRefFn<RO>`/`RefTo{O}Fn`/`RefToRefFn<RO>` | `out[o]=f.apply(e); o++` | `o` (cursor) | `build{Fwd,Bwd}{I}{O}` |
| **BuildFiltered** (filter/filterNot) | `buildFilteredTraverser` 2672 | `{K}Pred` | `if (p.apply(e)) out[o++]=e` | `o` | `buildFiltered{,Bwd}{K}` |
| **Partition** (partition/partitionMap subset) | `partitionTraverser` 2955 | `{K}Pred` | `p ? outA[oa++]=e : outB[ob++]=e` | packed long `(ob<<32)|oa` | `partition{Fwd,Bwd}{K}` |
| **Foreach** | `foreachTraverser` 3234 | `{K}Consumer` | `f.apply(e)` | void | `foreach{Fwd,Bwd}{K}` |
| **Scan** (scanLeft/scanRight) | `scanTraverser` 3515 | `{K}To{K}Fold`/`{K}ToRefFold<Z>` | `out[o]=f.apply(prevAcc,e); o±` | `o` | `scan{Fwd,Bwd}{Rev}{K}{K\|Ref}` |
| **ShortCircuit** (exists/forall/find/indexWhere/indexOf/segmentLength/…) | `scTraverser` 3800 | `{K}Pred` | `if (p.apply(e)) return globalIdx` | first-hit index | `sc{Fwd,Bwd}{K}` |

(That is 7 emitters; "6 shapes" in the design counts Build + BuildFiltered as the same Build family,
or folds Partition into BuildFiltered as the dual-output sibling.)

### SAM fn types (Traversers.java, emitted GenCores 1659–1716)
- Fold: `{K}To{K}Fold` (`{K} apply({K} acc, {K} v)`), `{K}ToRefFold<Z>` (`Z apply(Z acc, {K} v)`).
- Build: `{I}To{O}Fn`, `{I}ToRefFn<RO>`, `RefTo{O}Fn`, `RefToRefFn<RO>`. Ref INPUT element is plain
  `Object` (no tparam) — the Scala lambda casts `Object→A`; only Ref OUTPUT carries `RO`.
- Consumer: `{K}Consumer` (`void apply({K} v)`). Pred: `{K}Pred` (`boolean apply({K} v)`).
Coverage is restricted: Build covers `buildPairs` (1813) = prim-self + each-kind×Ref; Reduce/Scan
cover prim-self + Ref-acc. **Cross-prim** combinations (Long acc over Int input, Int→Long map) are
NOT in the Java engine — they fall back to the Scala `lower` walk (cross-prim) or a boxed
`applyAtImpl` path.

### The shared non-inline leaf methods + slim inline surfaces
The inline op surface NEVER inlines a tree walk. Pattern (per shape, e.g. `reduceLeafMethods` 671,
`mapLeafMethods` 727, `filterLeafMethods` 784, `partitionLeafMethods` 825, `foreachLeafMethods` 873,
`scanLeafMethods` 901, `scLeafMethods` 982):
- A **non-inline** `{shape}Leaf{K…}(xs, …, samOrFn)` method in FArrayOps peels `Empty`/`{K}One`/
  top-`{K}Arr` with an inlined unboxed leaf loop calling the realized SAM, and for a genuine TREE
  makes ONE call to `Traversers.{shape}{Fwd,Bwd}…`. Staying small lets the JIT inline it.
- The **inline** op surface (e.g. `foldLeftImpl`, `mapImpl`) only `summonFrom`s the kind, realizes
  the user's `inline op`/`f`/`p` into the specialized SAM (`(acc,v) => …` / `(v) => …`), and CALLS
  the shared leaf method. No walk in the surface.
So each shape has THREE layers: inline surface (kind dispatch + SAM realization) → shared leaf method
(peel small shapes, forward trees) → Java traverser (the walk).

---

## 5. THE ITERATOR (pull) vs the push traversers

**Separate, not shared.** `iteratorImpl` (FArrayOps.scala 1987–1992) returns a `{K}Cursor`
(`cursorClass`, GenCores 318–327): a trivial 2-field flat-array cursor (`a`, `pos`) over a `{prim}[]`.
- A top-level **leaf** hands its backing array straight in (`new IntCursor(leaf.arr, leaf.length)`) —
  no copy, lazy.
- A **tree** is flattened ONCE via `materialize{K}` (GenCores 369–376) — which itself uses the
  non-inline `dfsC{K}` push walk (engine #1) with an arraycopy consumer — then cursored over the
  flat array.
So the tree-walk for iteration reuses the **Scala** `dfsBody` (via `materialize`), NOT the Java
traversers. The cursor is `AbstractIterator` with `knownSize`; consumed inline it scalar-replaces
(2 fields) for foreach-speed. There is no lazy pull-walk that shares the push definition — the design
note "ONE walk def generates push traversers AND the pull iterator" (MEMORY) is the *plan*, not the
current state: today the pull iterator materializes.

---

## 6. THE CODEGEN STRUCTURE — and WHERE THE DUPLICATION IS

### Emit helper (GenCores 126–151)
A tiny indent-aware string builder: `line`/`lines`/`open(head)` (emits `head {`, indent++),
`close()` (`}`), `closeOpen(head)` (`} head {`), `closeWith`, `scope(body)` (`{ … }`). Every Java
emitter threads a single `Emit e`. This is the ONLY structural-emission abstraction; there is no
higher-level "emit a walk" combinator — each shape open-codes the walk via `e.open/line/close`.

### The codegen-only traversal AST (GenCores 153–203)
`Dir`, `Body`, `Acc`, `Exit`, `Out`, `SizeHint`, `Finalize`, `Trav` — these reify the (direction,
holes, accumulator, output, exit) tuple for the **Scala** inline emitter only. `lower(spec)` (482)
consumes a `Trav`, calls `dfsBody` with the per-element holes spliced, and wraps with acc-locals +
the leaf fast path + the Out epilogue. This is a clean, parameterized single-source design — but it
is used by FEWER and fewer ops (the hybrid surface migrated almost everything to the Java engine).

### The duplication — QUANTIFIED

**The same walk skeleton is emitted, as a separate hand-written template, 8 times:**

1. Scala `dfsBody` (204–291) — the inline/`dfsC` walk. ×1 template, instantiated fwd+bwd, ×4 kinds.
2. `reduceTraverser` (2092–2369)
3. `buildTraverser` (2378–2671)
4. `buildFilteredTraverser` (2672–2954)
5. `partitionTraverser` (2955–3233)
6. `foreachTraverser` (3234–3514)
7. `scanTraverser` (3515–3799)
8. `scTraverser` (3800–~4090)

Each of templates 2–8 independently re-defines the SAME local helpers (`runLeaf`, `run`, `runBoxed`,
`ensureStack`, `ensureTail`, `pushTail`, `pushChild`, `fromBoxed`) and the SAME 10-arm `cur match`
dispatch (leaf, one, prepend, append, concat, reverse, slice, pad, updated, range), differing only in
the per-element action and the return state. The arms are byte-for-byte parallel except for the
`act`/`writeElem`/`f.apply` line. (`grep instanceof` = 138 occurrences in GenCores, almost all from
these duplicated arms + their windowed mirrors.)

**Plus the windowing is duplicated by being instantiated per shape:** `emitWindow` is ONE function,
but each shape calls it with its own `WinSpec`, producing a separate `{shape}Window{Fwd,Bwd}…`
family (1148 `Window` lines in generated output; method families: `reduceWindow`, `buildWindow`,
`buildFilteredWindow`, `partitionWindow`, `foreachWindow`, `scanWindow` — scan has 4× per (K,acc)
for the visit/write cross-product). The windowing logic itself is NOT duplicated in the generator
(good — `emitWindow`/`sliceDeep`/`updLeafRun` are shared), only multiplied at generation time.

**Fwd vs Bwd:** within each of the 8 templates, fwd and bwd are emitted from the SAME function with
`if (!backward) … else …` branches at every arm and every run loop — a true mirror, not a second
copy in source, but it doubles the generated method count and is the densest source of "did the
mirror get flipped correctly?" risk.

**Per kind:** each template is instantiated ×{Int,Long,Double,Ref} (Reduce/Scan also ×{prim,refAcc};
Build ×`buildPairs`=10 pairs). So Traversers.java has 180 static methods that are, at the skeleton
level, the same walk ~16-fold.

### Generated method inventory (Traversers.java)
Push traversers: reduce 28 + reduceWindow 28, build 32 + buildWindow 32, buildFiltered 8 +
buildFilteredWindow 8, partition 8 + partitionWindow 8, foreach 8 + foreachWindow 8, scan 56 +
scanWindow 56, sc 16. (Window variants roughly double everything except sc.)

---

## RANKED COMPLEXITY / FRAGILITY HOTSPOTS

### 1. The fwd/bwd mirror duplication across 8 walk templates  *(highest)*
**Why complex:** The walk skeleton, its 4 run helpers, and its 10 node arms exist 8 times, each with
embedded `if (!backward)` mirrors. A correctness fix (or a new node) must be applied in 8 places ×2
directions, by hand, in string templates with no type checking until the generated Java/Scala
compiles. The arms are subtle (Prepend defers in bwd but acts in fwd; Append the reverse; the
descending `run` uses `start - count` end-bounds; the pop loop's dangling-else; the `clearTail`
stale-flag clear). This is the single biggest "understand once, then fear touching" surface.
**Recursion vs stack verdict:** A **JVM-recursive** walk would shrink each template to ~9 small
methods (one per node arm, each calling the driver on children) and eliminate the explicit stack +
the fwd/bwd run-helper triplication — but it would NOT, by itself, remove the per-SHAPE duplication
(you'd still have 7 shapes). The big win is orthogonal: **express the walk ONCE** (recursive or
iterative) parameterized by a per-shape `act`/`actRun`/return, exactly as `WinSpec`+`emitWindow`
already do for the windowed case. The windowing code is the proof that a single shared walk
function over a spec works; the main driver should adopt the same pattern. Recursion makes that
shared function dramatically smaller and removes the stack entirely (see #2), so **recursion +
single-spec-driven walk** is the high-value move. The only reason the main walk is iterative is
performance folklore (avoid deep Java recursion on degenerate left-leaning Concat chains); whether
that matters is a *measure-don't-assume* question — the windowed walk is already recursive on the
same trees.

### 2. The lazy `tail[]` / `isTail[]` parallel-array stack
**Why complex:** Three parallel arrays (`stack`/`tail`/`isTail`) that must stay index-aligned through
`ensureStack`'s lockstep `copyOf`, lazy allocation in two stages, a stale-flag-clear (`pushChild`
must zero `isTail[sp]`), and a pop loop that branch-reads each slot as element-or-subtree. Off-by-one
or a missed `clearTail` silently drops or double-acts an element. It encodes "a deferred Append
element" and "a deferred Concat subtree" in one stack because the iterative walk can't return a value
from a child.
**Recursion vs stack verdict:** **Recursion eliminates this entirely.** With a recursive driver,
Append's deferred element is just "recurse base, then act elem" — a straight-line statement, no
stack, no tail array, no flag, no alignment invariant. This is the clearest, most unambiguous win of
the whole redesign: the parallel-stack machinery is ~30 lines × 8 templates of pure accidental
complexity that exists ONLY to simulate the call stack. The recursion depth is bounded by tree depth,
and FArray's trees are built by O(1) structural ops that already recurse in Java for take/drop/
reverse (`Concat.java` note: "Java recursion is fine here"), so the depth concern is already accepted
elsewhere.

### 3. The Scan visit/write-direction cross-product (4 methods per (K,acc))
**Why complex:** Scan decouples the WRITE convention (scanLeft writes `out[o-1]→out[o]` ascending;
scanRight descending) from the VISIT direction (flips at ReverseNode). That's 4 methods —
`scan{Fwd,Bwd}` MAIN + `scan{Fwd,Bwd}Rev` reverse-arm — that bounce among each other, plus the
running acc is read from the previously-written slot (`out[o±1]`) rather than threaded, so the
ReverseNode recursion must keep the SAME write dir while flipping ONLY visit. The comment (3507–3514)
documents that an earlier version recursed into the opposite-direction scan and corrupted the output
(AIOOBE). This is the most bug-historied arm.
**Recursion vs stack verdict:** Recursion doesn't remove the visit/write decoupling (that's inherent
to "reverse changes read order but scan output order is fixed"), but it would make the four-way bounce
far easier to reason about: a recursive `scan(node, writeDir)` that flips only its internal read
direction at ReverseNode, with `out[o±1]` as the carried acc, is a single function with one boolean,
versus four mutually-recursive named methods. Net: recursion helps readability here, but Scan stays
the intrinsically hardest shape regardless.

### 4. The windowed segment machinery (skip/take threading + collapse)
**Why complex:** `emitWindow` + `recurSeg`/`elemSeg`/`fillerSeg`/`sliceDeep`/`updLeafRun` implement a
second, *different* traversal model (recursive, window-collapsing) that must produce element-for-
element the SAME sequence as the main iterative driver — but expressed via per-child sub-window
arithmetic (`_ss = min(skip,_L)`, `childCSkip`, the bwd `baseLen-off-len+skip` reversed-frame offset).
Slice/Updated reuse `sliceDeep`, and ReverseNode bounces to the opposite-direction window with the
same skip/take. Getting the reversed-frame offset wrong is silent mis-windowing.
**Recursion vs stack verdict:** This part is ALREADY recursive and ALREADY single-sourced
(`emitWindow` is one function over a `WinSpec`) — it is the *model* the rest of the engine should
converge to, not a hotspot to replace. Its complexity is the offset arithmetic, which recursion
neither adds nor removes. The redesign opportunity is **unification**: if the MAIN walk became
recursive and spec-driven like `emitWindow`, the deep-base arms could fold into the same function
(a window is just a sub-traversal with `[skip,take)` bounds, where the main walk is `[0,length)`),
collapsing two traversal models into one and removing the `winCall`/`run`/`runBoxed` split.

### 5. The deep-base arms duplicated as leaf-fast-path + window-call in every shape
**Why complex:** Every Slice/Pad/Updated arm in every shape has TWO code paths — a leaf-base flat
`run` and a deep-base `winCall` — plus the now-superseded-but-still-emitted `runBoxed` helper. The
fwd/bwd offset math for the leaf path (`so+sn-1` desc, `ul-1`/`ui-1` for Updated) is hand-derived per
shape per direction. Easy to get a leaf-base slice offset right in fwd and wrong in bwd.
**Recursion vs stack verdict:** Folding the leaf-base into the same windowed function (see #4) — i.e.
ALWAYS go through the window, letting the window's own leaf arm handle the flat run — would delete the
per-shape leaf/deep split and its mirror math, at a possible (measurable) cost of one extra call for
the common leaf-base case. Recursion is incidental here; the win is **collapsing leaf-base and
deep-base into one path**.

---

### Summary verdict for the redesign
- The dominant problem is **duplication, not the explicit stack per se**: the same walk is
  hand-templated 8× (×fwd/bwd ×kind ×shape). The fix is to express the walk **once**, parameterized
  by a per-shape spec — exactly the `WinSpec`/`emitWindow` pattern, generalized to the main driver.
- **Recursion vs explicit stack:** recursion is a clear win for *simplicity* — it deletes the entire
  `tail[]/isTail[]` parallel-stack apparatus (hotspot #2) and shrinks every template — and FArray
  already accepts Java recursion over the same trees (structural ops, the windowed walk). The only
  reason to keep the explicit stack is a performance argument that is currently **unmeasured** for the
  main driver; given the project's "measure, don't assume" rule, the honest read is: **prototype a
  single recursive spec-driven walk, benchmark it against the iterative one on deep-Concat inputs, and
  let the number decide** — but expect recursion to win on simplicity/duplication decisively and to be
  at worst a small constant slower (likely a wash) on traversal throughput, since the per-element loop
  shape (the actual measured bottleneck per project memory) is unchanged.
