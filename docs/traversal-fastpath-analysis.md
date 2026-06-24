# FArray traversal fast-path analysis

Read-only architectural analysis. No code changed. All line references are into
`codegen/src/scala/farray/GenCores.scala` (the sourcegen that emits
`.bleep/generated-sources/farray/farray.GenCores/farray/FArrayOps.scala`) unless
otherwise noted; op declarations are in `farray/src/scala/farray/FArray.scala`.

## Executive summary

- The core has **three traversal engines**: the cur-driven walk `dfsC${K}`
  (`dfsBody`, lines 126–154), the inline short-circuit `scan` helper (line 296),
  and the inline backward `foldRightV` (line 332). `dfsC` is O(n) on **every**
  tree shape; `scan`/`foldRightV` are O(n) only on a **bare leaf** and otherwise
  fall to per-element `${k.lc}At` (`atDef`, line 181).
- That `kindAt` fallback is **O(depth)/element**: O(1) for a wrapper-over-leaf
  (SliceNode/ReverseNode/Pad/Updated of a leaf) but with node-match + indirection
  overhead, and **O(n·depth) total for a `Concat` tree — O(n²) for an append/
  prepend chain** (each `${k.lc}At` re-descends from the root).
- All short-circuiting and backward ops are affected: `exists`, `forall`, `find`,
  `indexWhere`, `indexOf`, `collectFirst`, `prefixLength`, `count`,
  `foreachWhile`, `contains`, `sum`, `product`, and `foldRight`. The forward
  full-traversal ops (`foldLeft`, `foreach`, `map`, `filter`, `flatMap`,
  `mapConserve`, `scanLeft`, hashing, materialize) already run on `dfsC` and are
  fast everywhere.
- The owner's whack-a-mole workarounds (`reduceRight`, `lastIndexWhere`,
  `lastIndexOf`, `max`, `min`, `maxBy`, `minBy`) all exist **only** to dodge this
  slow path by re-routing through `dfsC`-backed `foldRight`/`foreach`.
- Recommended fix: **Stage 1** swap the `case _` in `scan`/`foldRightV` to a
  `materialize → tight loop` fallback (kills the O(n·depth) blowup cheaply,
  ~30 min); **Stage 2** add a breakable `dfsC` + a reverse `dfsRC` so early-exit
  and backward ops are O(n) *and* allocation-free on every shape; then revert the
  workarounds to their natural composition.

---

## 1. Architecture map

### The three engines

**Engine A — `dfsC${K}` (cur-driven walk).** Defined by `dfsBody` (126–154),
emitted non-inline as `dfsC${K}` via `dfsCDef` (159) consuming a 2-method
`${K}Dfs` (`onLeaf(arr,len)` / `onOne(v)`, line 158). It is a `cur`-driven loop
with a lazily-allocated stack (`stack`/`tail`/`isTail`, null until a `Concat` or
`Append` first defers a sibling, lines 133–134, 144). Crucially every node type
is handled **structurally and unboxed**:

- leaf `${K}Arr` → `onLeaf(leaf.arr, leaf.length)` (whole run, one call) (140).
- `${K}One` → `onOne(o.elem)` (141); `Prepend` → emit elem, continue base (142);
  `Append` → defer elem on `tail`, continue base (143); `Concat` → defer right,
  continue left (144).
- `ReverseNode` / `Pad` / `Updated` / `SliceNode` (145–148): **if the base is a
  leaf**, read that base array directly (backward / offset / index-swap /
  forward) in a tight loop; **else** fall back to `${k.lc}At` per element (Reverse,
  Slice, Pad) or `materialize${K}` (Updated). Nested-non-leaf wrappers are rare;
  the common `drop`/`reverse`-of-flat case hits the tight base-array loop.
- `RangeNode` (Int only) → closed-form `start + ri*step` loop (128).

So `dfsC` is **O(n) on every shape**, no per-element root re-descent. This is why
the forward ops are uniformly fast.

**Engine B — `scan` helper (inline short-circuit).** Line 296:

```scala
xs match {
  case leaf: ${K}Arr => { val ar = leaf.arr; ...; while (i < ln) { val a = ar(i); <step/break> }; <result> }
  case _             => { val ln = xs.length;  ...; while (i < ln) { val a = ${k.lc}At(xs, i); <step/break> }; <result> }
}
```

Two arms only: **bare leaf** (tight, unboxed) or **everything else** → per-element
`${k.lc}At`. No wrapper-of-leaf arm, no stack walk. Break is `i = ln`.

**Engine C — `foldRightV` (inline backward).** Line 332: same two-arm shape, but
backward. Leaf arm reads `ar(i)` for `i = length-1 .. 0` (no `ReverseNode`
allocation); `case _` walks `${k.lc}At(xs, i)` backward. Same bare-leaf-or-kindAt
dichotomy as `scan`, in reverse.

**Engine D — `${k.lc}At` / `applyAt` (per-element random access).** `atDef`
(181–193): a recursive descent from the node it is handed. Used as the fallback
inside Engines B and C, and directly by `applyAtImpl` (578) and the size-0/1
guards.

### Op → engine → cost-per-shape table

Shapes: **leaf** = bare `${K}Arr`; **wrap** = SliceNode/ReverseNode/Pad/Updated
whose base is a leaf (the `drop`/`reverse`/`padTo`/`updated`-of-flat case);
**tree** = Concat / Append / Prepend structure (including degenerate chains).

| Op (`FArray.scala`) | Impl / `*V` val | Engine | leaf | wrap | tree |
|---|---|---|---|---|---|
| `foldLeft`, `fold` | `foldLeftImpl` / `foldLeft` (288) | A `dfsC` | O(n) | O(n) | O(n) |
| `foreach` | `foreachImpl` / `foreach` (291) | A | O(n) | O(n) | O(n) |
| `map` | `mapImpl` / `mapM` (335) | A | O(n) | O(n) | O(n) |
| `filter`, `filterNot`, `collect`, `partition`, `distinct`, `diff`, `intersect` | `filterImpl` / `filter` (345) | A | O(n) | O(n) | O(n) |
| `mapConserve` | `mapConserveImpl` / `mapConserve` (359) | A (leaf) / A+materialize (`case _`, 361) | O(n) | O(n) | O(n) |
| `flatMap`, `flatten` | `flatMapImpl` / `flatMapOne` (370) | A (`flatMapCopyOne` 220 inner) | O(n) | O(n) | O(n) |
| `scanLeft` | `scanLeftImpl` / `scanLeftV` (321) | A | O(n) | O(n) | O(n) |
| `iterator`, `reverseIterator` | `iteratorImpl` / `iteratorV` (327) | A (leaf direct / materialize) | O(n) | O(n) | O(n)+alloc |
| `unzip`/`unzip3` | `unzip*V` (396/407) | own loop, leaf `applyBoxed` else `applyBoxed(i)` | O(n) | **O(n·d)** | **O(n·d)** |
| `zip`/`zipWithIndex` | `zipV`/`zipIdxV` (430/446) | own loop, leaf+leaf fast else `${k.lc}At` | O(n) | **O(n·d)** | **O(n·d)** |
| `corresponds`/`startsWith`/`endsWith`/`sameElements`/`indexOfSlice`/`lastIndexOfSlice` | `matchAll2V` (456) | own loop, leaf+leaf fast else `${k.lc}At` | O(n) | **O(n·d)** | **O(n·d)** |
| **`exists`** | `existsImpl` / `existsV` (298) | **B `scan`** | O(n) | **O(n·d)** | **O(n·d)** |
| **`forall`** | `forallV` (299) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`find`** | `findV` (300) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`indexWhere`** | `indexWhereV` (301) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`indexOf`** | `indexOfV` (302) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`collectFirst`** | `collectFirstV` (303) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`prefixLength`** (`takeWhile`/`dropWhile`/`span`/`segmentLength`) | `prefixLenV` (304) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`count`** | `countV` (305) | **B** (no break) | O(n) | **O(n·d)** | **O(n·d)** |
| **`foreachWhile`** | `foreachWhileV` (330) | **B** | O(n) | **O(n·d)** | **O(n·d)** |
| **`contains`** | `containsImpl` / `contains` (349) | **B-shaped** (inline) | O(n) | **O(n·d)** | **O(n·d)** |
| **`sum`** | `sumV` (307) | **B-shaped** | O(n) | **O(n·d)** | **O(n·d)** |
| **`product`** | `productV` (313) | **B-shaped** | O(n) | **O(n·d)** | **O(n·d)** |
| **`foldRight`** | `foldRightImpl` / `foldRightV` (332) | **C** | O(n) | **O(n·d)** | **O(n·d)** |
| `applyAt`/`apply`/`head`/`last` | `applyAtImpl` / `applyAt` (355) | D `${k.lc}At` | O(1) | O(1) | O(depth) per call |

`d` = node depth above the leaf. For **wrap** d is a small constant (1–2) so
"O(n·d)" is really O(n) with a per-element node-match + indirection tax — that is
the 0.3–0.6× slowdown the owner measured on `reduceRight`/`lastIndexWhere`. For
**tree** d can be Θ(log n) (balanced concat) up to Θ(n) (append/prepend chain →
**O(n²)**).

### Ops that route around the slow path (the workarounds)

These are written in `FArray.scala` to compose through Engine A instead of
B/C/D, confirming the diagnosis:

- `reduceRight` (120–123): comment says *"whole-array foldRight (leaf read
  backward) with a skip-first flag — avoids dropRight's SliceNode + kindAt"*. But
  it still calls `foldRight` (Engine C) — fast only because `foldRight`'s **leaf**
  arm is fast; on a tree this workaround is still O(n·d). It dodges the *extra*
  `SliceNode` that `dropRight(1).foldRight` would have built.
- `lastIndexWhere` (244–246) / `lastIndexOf` (247–249): comment *"forward
  whole-array leaf scan recording the last match — avoids reverse's ReverseNode +
  per-element kindAt"*. Routed through `foreach` (Engine A) → O(n) on all shapes.
- `max`/`min` (139–142), `maxBy`/`minBy` (133–138): hand-rolled over `foreach`
  (Engine A), recording the running best — instead of `reduceLeft`/`reduceRight`.
- `reduceLeft`/`reduce` (118–119): `xs.drop(1).foldLeft(...)` — `foldLeft` is
  Engine A so this is fine; `drop(1)` builds a SliceNode but A reads its leaf base
  directly.

---

## 2. Cost analysis of the `kindAt` fallback

`atDef` (181–193) per call, `i` already known:

```scala
def ${k.lc}At(node, i) = node match {
  case leaf  => leaf.arr(i)                                   // O(1)
  case One   => o.elem                                        // O(1)
  case Concat=> if (i<left.length) at(left,i) else at(right, i-left.length)   // recurse 1 level
  case Append=> if (i<base.length) at(base,i) else a.elem     // recurse into base
  case Prepend=> if (i==0) p.elem else at(base, i-1)          // recurse into base
  case Reverse=> at(base, base.length-1-i)                    // recurse into base
  case Pad   => if (i<base.length) at(base,i) else filler     // recurse into base
  case Updated=> if (i==index) elem else at(base,i)           // recurse into base
  case Slice => at(base, offset+i)                            // recurse into base
  case Range => start + i*step                                // O(1)
}
```

It is a **recursive descent from `node` to a leaf**, costing **O(depth from
`node` to the leaf reached for index `i`)**. There is **no memoized cursor** —
each call restarts at the root. So inside a `scan`/`foldRightV` `case _` loop
that calls `${k.lc}At(xs, i)` for `i = 0 .. n-1`:

- **wrap (Slice/Reverse/Pad/Updated of a leaf), depth 1:** each call is one
  match + one tail-recursion into the leaf = O(1)/elem, total **O(n)** — but
  with a megamorphic `match` + extra method frame per element vs. the tight leaf
  loop. This is the constant-factor loss (~0.3–0.6×) on `reduceRight`/
  `lastIndexWhere`-style ops over `drop`/`reverse`-of-flat.
- **balanced Concat tree, depth Θ(log n):** each call descends Θ(log n) levels →
  total **O(n·log n)**.
- **append chain `(((leaf :+ a) :+ b) :+ c)…`, depth Θ(n):** `${k.lc}At(xs, i)`
  for the deepest element walks Θ(n) `Append` nodes; summed over all `i` →
  **Θ(n²)**. Same for a prepend chain and for a left-leaning `Concat` spine built
  by repeated `++`. This is the catastrophic case the materialize fallback (Option
  2) eliminates outright.

For short-circuit ops the early-exit *probability* interacts: if the predicate
hits early the absolute cost is small regardless, but worst-case (no match /
match at the end) pays the full O(n·depth). For `foldRight` and `count` there is
no early exit, so they always pay it.

Note Engine A already pays an **O(base.length)** per-element penalty in the
`dfsBody` `case _` arms of Slice/Reverse/Pad **only when the wrapper's base is
itself non-leaf** — i.e. a Slice-of-Concat. That is rarer than Slice-of-leaf
(which take/drop on a flat array produce) and is out of scope here; the dominant
problem is Engines B/C taking the `kindAt` path even for a leaf base.

---

## 3. Options

### Option 1 — breakable forward `dfsC` + reverse `dfsRC` (the durable fix)

Make every shape fast while keeping early-exit and backward order.

**(a) Breakable forward walk.** Add a consumer whose `onLeaf`/`onOne` return a
`Boolean` "continue?", and a `dfsCB${K}` whose leaf-run loop breaks internally and
whose driver stops the `while (cur != null)` when the consumer says stop. Route
all Engine-B ops through it. Sketch (delta to `dfsBody`/`dfsConsumer`):

```scala
abstract class ${K}DfsB { def onLeaf(a: Array[${arr}], len: Int): Boolean; def onOne(v: ${arr}): Boolean }
// leaf arm:  { var i=0; var go=true; while(go && i<len){ go = c.onLeaf1(a(i)); i+=1 }; if(!go) return }
//            (or pass the run + a start index so onLeaf scans+breaks itself)
// driver:    add `var stop=false` to the loop guard; onOne sets stop; `while(cur!=null && !stop)`
```

`exists` becomes `onOne(v) = !p(v)` (continue while not found) with a captured
`var res`; `indexWhere` tracks a running index. The stack-unwind path (152) must
also short-circuit when `stop` — straightforward (extra `&& !stop`).

**(b) Reverse walk `dfsRC${K}`.** A mirror of `dfsBody`: visit right-to-left.
For each node emit children in reverse — `Concat` continue `right`, defer `left`;
`Append` emit `elem` first then base; `Prepend` defer `elem`, continue base;
leaf-runs and Slice/Pad/Range loop the base array **backward**; `Reverse` flips to
forward. Then `foldRight` is "`dfsRC` with `acc = op(elem, acc)`". The current
`cur`/stack structure supports this directly — it is the same machine with the
left/right and forward/backward roles swapped (the `Hashing.fill` `rev` flag at
lines 1036–1053 is an existing, working proof that every node type has a clean
reverse traversal). Combine (a)+(b) for breakable-reverse to also cover any future
backward short-circuit op.

**Effort:** medium-high. ~2 new `dfsBody`-shaped generators (breakable + reverse,
×4 kinds, emitted once each so no code-size blowup) + rewiring ~13 `*V` vals from
`scan`/`foldRightV` to the new consumers. The reverse walk needs its own careful
test pass (the hashing `rev` path is the spec to match). Estimate ~1 day.

**Tradeoffs:** + O(n), zero allocation, early-exit preserved on **every** shape;
the workarounds can all revert to natural composition. − Most code to write/
maintain; two more traversal machines to keep correct; consumer indirection adds a
tiny constant vs. the current inlined `scan` leaf loop (the inlined-`onOne`/escape-
analysis story is already proven good for `dfsC`, so this is minor).

### Option 2 — materialize fallback (cheapest correctness fix)

Change only the `case _` arm of `scan` (296) and `foldRightV` (332) from
per-element `${k.lc}At` to materialize-then-tight-loop:

```scala
// scan case _ :
case _ => { val ar = materialize${K}(xs); val ln = ar.length; var i = 0; $init
            while (i < ln) { val a = ${readVal(k, "ar(i)")}; $step }; $result }
// foldRightV case _ : same, iterate ar backward
```

`materialize${K}` (204) already runs Engine A (O(n)) and returns a flat
`Array[${arr}]`.

**Effort:** trivial (~30 min, two string edits in `scan`/`foldRightV`, regen).
Also fixes `sum`/`product`/`contains` if their inline `case _` arms are switched
the same way (they don't go through `scan` — they're hand-written at 309/311/316/
318/351/353 — so each needs its own one-line edit).

**Tradeoffs:** + Kills the O(n·depth)/O(n²) blowup outright — every non-leaf
becomes **O(n)**. Tiny code. + No new traversal machinery. − Allocates an
`n`-element array even when not needed. − **Loses early exit**: a short-circuit op
on a tree now always materializes the whole thing first, so `exists` that would
have matched at element 3 of a million-element concat still allocates and fills a
million-slot array. That regresses the (currently O(n·depth) but early-exiting)
common case where the predicate hits early. For `foldRight`/`count` (no early
exit anyway) it is a strict win. Best as a **stopgap for the non-short-circuit
ops** while Option 1 lands.

### Option 3 — extend the bare-leaf fast-path to wrapper-of-leaf

In `scan`/`foldRightV`/`contains`/`sum`/`product`, add arms that recognize
Slice/Reverse/Pad-of-leaf and read the base array directly (mirroring what
`dfsBody` already does at 145–148):

```scala
// inserted before `case _` in scan:
case s: SliceNode if s.base.isInstanceOf[${K}Arr] =>
  { val ar = s.base.asInstanceOf[${K}Arr].arr; val off = s.offset; val ln = s.length
    var i = 0; $init; while (i < ln) { val a = ${readVal(k, "ar(off + i)")}; $step }; $result }
case rev: ReverseNode if rev.base.isInstanceOf[${K}Arr] =>
  { val ar = rev.base.asInstanceOf[${K}Arr].arr; val ln = rev.length
    var i = 0; $init; while (i < ln) { val a = ${readVal(k, "ar(ln - 1 - i)")}; $step }; $result }
// + ${K}Pad-of-leaf (loop base then filler), keep `case _` -> kindAt for Concat/Updated
```

**Effort:** low-medium (~2–3 h). Per-kind arm template added to ~5 helpers, ×4
kinds. Mechanical but duplicates the node-read logic that `dfsBody` already owns.

**Tradeoffs:** + No allocation, keeps early-exit, fixes the **common**
`drop`/`take`/`reverse`/`padTo`-of-flat case (the wrap shapes, which are most of
the owner's measured losses). + Lets `reduceRight`/`lastIndexWhere`/`max`/`min`
revert *for the wrap case*. − Still falls to `kindAt` for genuine `Concat`/append
chains (the O(n²) case survives). − Duplicates per-node read logic across many
helpers — exactly the kind of node-by-node special-casing the owner wants to stop
multiplying. It is "more whack-a-mole, done in one place."

---

## 4. Recommendation

**Now (cheap, ship today):** Apply **Option 2 to the non-short-circuit ops** —
`foldRight` (332), `count` (305), `sum` (307/311), `product` (313/318). These have
no early exit, so materialize-then-loop is a pure win and removes the O(n²) risk
for `foldRight`/`count`/`sum`/`product` on append chains and concat trees. ~30 min,
no behavioral change beyond performance.

**Durable (the real fix):** Implement **Option 1** — breakable `dfsCB${K}` for the
short-circuit family (`exists`, `forall`, `find`, `indexWhere`, `indexOf`,
`collectFirst`, `prefixLength`, `foreachWhile`, `contains`) and reverse `dfsRC${K}`
for the backward family (`foldRight` and any future `findLast`/`lastIndexWhere`).
This gives O(n) + zero-alloc + early-exit on every shape and is the only option
that makes the tree case genuinely fast without giving up short-circuiting. The
`Hashing.fill` `rev` machinery (1036–1053) is a ready blueprint for the reverse
walk; the forward breakable walk is a small delta on `dfsBody`. Land Option 1, then
delete the Option-2 stopgaps (or keep them as the `case _` for the rare
nested-non-leaf-base wrappers).

**Skip Option 3** as a standalone — it only addresses the wrap shapes and adds the
very per-node duplication the owner is trying to eliminate. Its node-read logic is
better expressed once, inside the `dfsBody`-family generators of Option 1.

### Workarounds to revert once the root is fixed

After Option 1 (and even after Option 2 for the backward ones), these can return
to natural, readable composition with no perf penalty:

| Op (`FArray.scala`) | Current workaround | Revert to |
|---|---|---|
| `reduceRight` (120) | whole-array `foldRight` + skip-first flag | `xs.dropRight(1).foldRight(xs.last)(op)` (clean) — once `foldRight` is fast on the SliceNode `dropRight` builds |
| `reduceRightOption` (238) | delegates to the above | follows `reduceRight` |
| `lastIndexWhere` (244) | forward `foreach` recording last match | `xs.reverse.indexWhere(p)` mapped back (once `indexWhere` is fast on `ReverseNode`) — or keep, the foreach form is also fine |
| `lastIndexOf` (247) | forward `foreach` recording last match | `xs.reverse.indexOf(elem)` mapped back |
| `max`/`min` (139–142) | hand-rolled `foreach` + running best | `reduceLeft`/`reduce` (once those stay on fast engines) |
| `maxBy`/`minBy` (133–138) | hand-rolled `foreach` + running best key | natural `reduceLeft`-style once `reduce*` are uniformly fast |

(`reduceLeft`/`reduce` at 118–119 already compose naturally through Engine A and
need no change.)

---

## Appendix — verification notes

Every claim above was checked against `GenCores.scala` at the cited lines:

- `dfsBody` (126–154) handles all 11 node kinds structurally; Slice/Reverse/Pad
  read a **leaf** base directly (145–148) — confirmed Engine A is shape-agnostic.
- `scan` (296) and `foldRightV` (332) are two-arm `leaf`-or-`${k.lc}At` —
  confirmed; no wrapper arm.
- `existsV`…`countV` (298–305), `foreachWhileV` (330) all built from `scan` —
  confirmed via the `dispatchA(k => scan(...))` calls.
- `contains` (349), `sumV` (307), `productV` (313) are hand-written but share the
  identical `leaf`-or-`${k.lc}At` two-arm shape — confirmed.
- `atDef` (181) is a root-restarting recursive descent, O(depth)/call — confirmed;
  `Concat` recurses (184), `Append`/`Prepend`/`Reverse`/`Pad`/`Updated`/`Slice`
  recurse into base.
- Append/Prepend nodes form chains (`primAppend`/`primPrepend`, 1058/1088;
  `:+`/`+:` at FArray.scala 114), so `${k.lc}At` over an `:+`-chain is Θ(n²).
- Workarounds confirmed in `FArray.scala`: `reduceRight` 120–123, `lastIndexWhere`
  244–246, `lastIndexOf` 247–249, `max`/`min` 139–142, `maxBy`/`minBy` 133–138.
- `Hashing.fill` (1036–1053) is an existing full reverse-capable per-node walk
  (the `rev` flag) — cited as the blueprint for Option 1's `dfsRC`.
