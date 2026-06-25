# Hybrid traversal codegen — rewrite plan

Status: **plan** (supersedes `traversal-codegen-design.md`, the inlined-walk approach, which is
abandoned). This rewrites how per-element traversal ops are generated.

> **Design correction (shared leaf method).** Inlining the leaf fast-path loop at *every* call site
> cliffs when one method has several folds/maps over large arrays — 8 maps @100k measured **0.29×** vs
> iarray (normal code, not contrived): the per-site bytecode bloat falls off the JIT's compile path. The
> fix is a **small SHARED non-inline leaf method** (the leaf loop in ONE place, taking the realized
> function type): the JIT inlines it for a lone monomorphic call site (a single fold stays fast) *and*
> shares it across many sites (no per-call bloat → no cliff) — exactly how `Array.map` works. Measured:
> the shared form is **1.03×** at 8-map @100k and **ties iarray everywhere, 1.01–1.07×**. The Reduce
> family (`foldLeft`/`foldRight`/`sum`/`product`/`count`/`mkString`) is generated this way — see §5.

## 0. What we learned (the empirical basis)

- **Flat `map`/`fold` ties iarray and that is the ceiling.** iarray's `Array.map` JIT-inlines per call
  site, so a monomorphic `map(_+1)` reduces to the identical vectorized loop. We do not beat it there.
- **We crush iarray where it can't stay unboxed or O(1):**
  - *Megamorphic* map (many `map` sites → iarray's shared `Array.map` goes megamorphic → boxes):
    measured **1.3×–7.5×**, with the deterministic tell `iarray B/op` = 2–3× ours at every size ≥1000.
    `@1000 = 2.45× ±0%` is the cleanest point.
  - *Structural/lazy* ops (`reverse`, `++`, `take`, …): **1,000–30,000×** (checked-in report).
- **Inlining the whole tree walk at every call site is a hazard.** The committed inlined walk is ~642
  bytecode ops *per call site*; 8 of them in one method falls off the JIT's compile path and loses to
  iarray (0.37× @100k). The hybrid call site is ~65 ops and wins.

**Conclusion → the hybrid:** inline only the hot shapes (Empty / One / leaf) at the call site for raw
speed; share the cold tree walk in **hand-generated Java**, called via a realized function object.

## 1. Scope

**IN — the per-element traversal ops this rewrite owns:**

| shape | ops |
|---|---|
| Reduce | `foldLeft` `foldRight` `reduce` `reduceLeft` `reduceRight` `sum` `product` `count` `min` `max` `minBy` `maxBy` `mkString` |
| ShortCircuit | `exists` `forall` `contains` `find` `collectFirst` `indexWhere` `indexOf` `segmentLength` `prefixLength` `lastIndexWhere` `lastIndexOf` `foreachWhile` |
| Build | `map` `mapConserve` |
| BuildFiltered | `filter` `filterNot` `collect` `takeWhile` `dropWhile` `span` `partition` `partitionMap` |
| Scan | `scanLeft` `scanRight` |
| Foreach | `foreach` |
| Copy | `toArray` `copyToArray` `materialize${K}` (degenerate Build, identity transform) |

**OUT — untouched by this rewrite:**

- **Structural O(1)** (make a node, no walk): `++` `:+` `+:` `take` `drop` `slice` `reverse` `updated`
  `padTo`.
- **Construction**: `tabulate` `fromArray` `fromValuesN` `empty`.
- **Random access**: `applyAt` `apply`.
- **Heavy / not-yet-unified** (keep current impls; revisit later): `flatMap` (variable fan-out per
  element), `groupBy`/`groupMap` (hashmap accumulate), `zip`/`zipWithIndex`/`unzip` (multi-array),
  `sort*` (materialize + sort).
- **`iterator` is IN, but special**: it is the *pull* realization of the **same walk abstraction**
  (§4.5), generated from the one walk definition — not a push traverser, and not a separate problem.
  This is a hard constraint on the abstraction, not an afterthought (see §4.5).

## 2. Shape taxonomy

A *shape* is a traversal whose only per-op variation is the function it applies and the scalar/array it
threads. Ops within a shape share one generated traverser pair (fwd + bwd); they differ only in the
function object they realize and how the surface maps the result.

| shape | per-element step | threaded state | dir | result |
|---|---|---|---|---|
| Reduce | `acc = f(acc, elem)` | `acc: Z` (local) | fwd/bwd | `Z` |
| ShortCircuit | `if (p(elem)) stop` | stop index `i` | fwd/bwd | `Int` index (op derives bool/elem/count) |
| Build | `out[o++] = f(elem)` | `out: O[]`, `o: Int` | fwd (+bwd for reverse) | new `FBase` |
| BuildFiltered | `if (p(elem)) out[o++] = elem` | `out`, `o` | fwd (+bwd) | new `FBase` (trimmed) |
| Scan | `acc = f(acc, elem); out[o++] = acc` | `acc`, `out`, `o` | fwd/bwd | new `FBase` (len n+1) |
| Foreach | `f(elem)` | — | fwd (+bwd) | `Unit` |

Notes that collapse ops into a shape:
- `sum`/`product`/`count`/`min`/`max` are **Reduce** with a fixed combine; `minBy`/`maxBy` are Reduce
  whose combine calls a key extractor (Reduce carries one function; minBy carries the comparing combine
  built around the key fn). `mkString` is Reduce into a `StringBuilder` (Z = `StringBuilder`).
- `forall`/`exists`/`contains`/`find`/`indexOf`/… are all **ShortCircuit** returning the first hit
  index; each op is a trivial wrapper (`indexWhere>=0`, `find`→`Some(applyAt(i))`, etc.). The scan uses
  the empty-body, predicate-in-condition loop shape (see §4.4) — the measured fast form.
- `collect`/`partitionMap` are BuildFiltered carrying **two** functions (a defined-predicate + a
  transform); `partition` writes to two outputs. These are the shape variants with an extra fn/out.

## 2.1 The op table (drives the codegen — one row per op)

Each op is a row: `op → (shape, direction, function type, inline Empty/One/leaf body, tree traverser,
result map)`. Adding an op is a row, never a new traverser. This is the concrete classification.

**Reduce** — `acc = fold(acc, e)`; surface inlines `Empty`→`z` (or throw, for the no-init `reduce*`),
`One`→`fold(z, elem)`, leaf→the unboxed loop; tree→`reduce{Fwd|Bwd}${I}${Z}`.

| op | dir | realized combine | Z |
|---|---|---|---|
| `foldLeft` | Fwd | `f(acc, e)` | dispatched on Z's kind |
| `foldRight` | Bwd | `f(e, acc)` | dispatched on Z's kind |
| `reduceLeft` | Fwd | `op(acc, e)`, seed = first elem | A |
| `reduceRight` | Bwd | `op(e, acc)`, seed = last elem | A |
| `sum` | Fwd | `acc + e` | I (unboxed) |
| `product` | Fwd | `acc * e` | I (unboxed) |
| `count` | Fwd | `acc + (p(e) ? 1 : 0)` | Int |
| `min` / `max` | Fwd | `ord.{lt,gt}(e, acc) ? e : acc` | A |
| `minBy` / `maxBy` | Fwd | compare `key(e)` vs `key(acc)` — `key` captured in the SAM | A |
| `mkString` | Fwd | `sb.append(sep).append(e)` | StringBuilder |

**ShortCircuit** — empty-body predicate-in-condition scan (§4.4), result = first-hit index → derived;
surface inlines `Empty`→default, `One`→test it, leaf→the scan; tree→`sc{Fwd|Bwd}${I}`.

| op | dir | predicate | result from index `i` |
|---|---|---|---|
| `exists` | Fwd | `p(e)` | `i < n` |
| `forall` | Fwd | `!p(e)` | `i == n` |
| `contains` | Fwd | `e == x` | `i < n` |
| `find` | Fwd | `p(e)` | `i<n ? Some(applyAt i) : None` |
| `collectFirst` | Fwd | `pf.isDefinedAt(e)` | `Some(pf(applyAt i))` |
| `indexWhere` / `indexOf` | Fwd | `p(e)` / `e == x` | `i<n ? i : -1` |
| `prefixLength` | Fwd | `!p(e)` | `i` |
| `segmentLength` | Fwd | `!p(e)` from `from` | `i - from` |
| `lastIndexWhere` / `lastIndexOf` | Bwd | `p(e)` / `e == x` | `i` or -1 |
| `foreachWhile` | Fwd | `!f(e)` | Unit |

**Build** — `out[o++] = f(e)`; surface inlines `Empty`→`Empty`, `One`→`${O}One(f(elem))`, leaf→the
unboxed map loop; tree→`build${I}${O}`.

| op | fn | note |
|---|---|---|
| `map` | `${I}To${O}Fn` | — |
| `mapConserve` | `${I}To${I}Fn` | identity-check; return `xs` if nothing changed |

**BuildFiltered** — `if (p(e)) out[o++] = e`, trimmed result; tree→`buildFiltered${I}`.

| op | fn(s) | note |
|---|---|---|
| `filter` / `filterNot` | `${I}Pred` | negate for `filterNot` |
| `collect` | `${I}Pred` + `${I}To${O}Fn` | 2-fn: isDefinedAt + apply |
| `partition` | `${I}Pred` | two outputs (yes / no) |
| `partitionMap` | `${I}ToRefFn` → `Either` | two outputs |
| `takeWhile` / `span` | `${I}Pred` | build that **stops** at first `!p` (ShortCircuit hybrid) |
| `dropWhile` | `${I}Pred` | skip-while, then copy the rest |

**Scan** — `acc = fold(acc, e); out[o++] = acc`, result len n+1; tree→`scan{Fwd|Bwd}${I}${Z}`.

| op | dir | note |
|---|---|---|
| `scanLeft` | Fwd | `out[0] = z` |
| `scanRight` | Bwd | `out[n] = z` |

**Foreach** — `f(e)`, Unit; tree→`foreach${I}`. **Copy** — `out[o++] = e` (no fn), tree→`copy${I}`:
`toArray` / `copyToArray` / `materialize${K}`.

**Misfits that need a hybrid arm or a captured second function:**
- **2-function** (`collect`, `partitionMap`, `minBy`, `maxBy`): the realized SAM closes over the second
  fn (`minBy`'s combine calls `key` internally; `collect`'s fn returns a sentinel for "undefined"). One
  traverser, second fn captured — no new shape.
- **ShortCircuit ∩ Build** (`takeWhile`, `span`): a BuildFiltered whose predicate, once false, *stops*
  rather than skips — the build traverser needs the early exit. Generated as a Build arm carrying the
  ShortCircuit stop.
- **`foreachWhile`**: Foreach + early exit.
- **`count`**: Reduce whose combine folds a predicate into an `Int`.
- **`reduce*` (no init)**: Reduce seeded from the first/last element; `Empty` → throw.

## 3. Generated function types (Java interfaces)

One family per *arity*, expanded over kind permutations. **Refs are type parameters** (`A` input, `B`
output, `Z` accumulator) so the realized lambda needs no `Object` casts.

```java
// unary transform — Build / map / collect-transform        (16 perms: I×O)
public interface IntToIntFn      { int    apply(int v); }
public interface IntToLongFn     { long   apply(int v); }
public interface IntToRefFn<B>   { B      apply(int v); }
public interface RefToIntFn<A>   { int    apply(A v); }
public interface RefToRefFn<A,B> { B      apply(A v); }
// … Long*, Double* analogously

// binary fold — Reduce / Scan                                (perms: I×Z; Z=Ref is generic)
public interface IntToIntFold      { int apply(int acc, int v); }
public interface IntToRefFold<Z>   { Z   apply(Z   acc, int v); }
public interface RefToRefFold<A,Z> { Z   apply(Z   acc, A   v); }

// predicate — ShortCircuit / Filter                          (4 perms: I)
public interface IntPred    { boolean test(int v); }
public interface RefPred<A> { boolean test(A v); }

// consumer — Foreach                                         (4 perms: I)
public interface IntConsumer    { void apply(int v); }
public interface RefConsumer<A> { void apply(A v); }
```

The codegen emits only the (kind-perm × family) cells that some op actually realizes, driven by the
op→shape table — not the full cross product.

## 4. The shared Java traversers

Per `(shape, kind-perm, direction)`, generated. A flat tree-only walk (Empty/One/top-leaf never reach
here — the surface peels them off, §5). State is a **Java local**; no heap consumer, no boxed cursor.

### 4.1 The two key structural rules

- **Everything except reverse → our own explicit stack, never the JVM stack.** `Concat`/`Append`/
  `Prepend` deferrals *and* the deep-base sub-walks (`Slice`/`Pad`/`Updated` over a non-leaf) all push
  frames onto a local stack we construct. This is the perf choice: no method-call push/pop per node, and
  deep `Concat` chains never touch the JVM stack. Deep base threads a `skip`/`limit` (the Slice window)
  or carries filler/update in its frame — never `applyBoxed` / index addressing.
- **The stack is lazy — built only when a `defer` first happens, and only as deep as the tree demands.**
  A walk with no deferrals (a bare leaf, or a `Prepend`/`Append` spine with no `Concat`) never allocates
  or even touches it. The no-stack path is the common, hottest case — *faster without* — so the
  generated walk must keep it branch-light: `null` stack, no bounds checks, straight to the leaf loop.
- **`ReverseNode` → JVM call stack — the *only* recursion, for now (non-iterator usage).** On a
  `ReverseNode` the forward traverser calls the **backward** traverser on `rev.base` (and vice versa);
  the direction flip rides the JVM call boundary, which is *why* reverse is the one node we recurse on —
  the flip comes for free. Each direction keeps its own explicit stack for the deferrals above.

### 4.2 Build, forward (map Int→Int) — example of the generated shape

Note the formatting standard for all generated code: real indentation, **one statement per line**.

```java
static int buildFwdIntInt(FBase root, int[] out, int o, IntToIntFn f) {
    FBase cur = root;
    FBase[] stack = null;
    int sp = 0;
    while (cur != null) {
        FBase next = null;
        if (cur instanceof IntArr leaf) {
            int[] a = leaf.arr;
            int n = leaf.length;
            int i = 0;
            while (i < n) {
                out[o] = f.apply(a[i]);
                o += 1;
                i += 1;
            }
        } else if (cur instanceof IntOne one) {
            out[o] = f.apply(one.elem);
            o += 1;
        } else if (cur instanceof IntPrepend p) {
            out[o] = f.apply(p.elem);
            o += 1;
            next = p.base;
        } else if (cur instanceof Concat c) {
            if (stack == null) {
                stack = new FBase[16];
            } else if (sp == stack.length) {
                stack = java.util.Arrays.copyOf(stack, sp * 2);
            }
            stack[sp] = c.right;
            sp += 1;
            next = c.left;
        } else if (cur instanceof ReverseNode rev) {
            o = buildBwdIntInt(rev.base, out, o, f);
        } else if (cur instanceof SliceNode s) {
            o = buildWindowFwdIntInt(s.base, s.offset, s.length, out, o, f);
        }
        // … IntAppend (defer elem on the stack), IntPad, IntUpdated, RangeNode …
        if (next != null) {
            cur = next;
        } else {
            cur = null;
            while (sp > 0 && cur == null) {
                sp -= 1;
                cur = stack[sp];
            }
        }
    }
    return o;
}
```

`buildBwdIntInt` is the exact mirror (Concat: descend right, defer left; leaf: `i` from `n-1` down;
`ReverseNode` → `buildFwdIntInt`). Output cursor `o` still advances forward in both — backward changes
*visit order*, not write order.

### 4.3 Reduce, forward — SAM per element, specialized on accumulator kind

The function is a **SAM** applied per element (`acc = f.apply(acc, v)`). Boxing is avoided by
**specializing the function type on the accumulator kind** (§3): primitive `Z` (`sum`/`min`/`max`/
`count`, and `foldLeft` dispatched on `Z`'s kind) → the unboxed `${I}To${Z}Fold`, so `acc` stays a
primitive local; `Z = Ref` → the generic `${I}ToRefFold<Z>`, where `acc` is already an object. Either
way, no boxing. The prototype's 0.38× crater was the *generic* `IntToRefFold<Z>` realized for `Z = Int`
— a wrong-specialization bug, not an inherent SAM cost.

```java
static <Z> Z reduceFwdIntRef(FBase root, Z acc, IntToRefFold<Z> f) {
    FBase cur = root;
    FBase[] stack = null;
    int sp = 0;
    while (cur != null) {
        FBase next = null;
        if (cur instanceof IntArr leaf) {
            int[] a = leaf.arr;
            int n = leaf.length;
            int i = 0;
            while (i < n) {
                acc = f.apply(acc, a[i]);
                i += 1;
            }
        } else if (cur instanceof IntOne one) {
            acc = f.apply(acc, one.elem);
        }
        // … same node arms as Build; ReverseNode → reduceBwdIntRef …
        if (next != null) {
            cur = next;
        } else {
            cur = null;
            while (sp > 0 && cur == null) {
                sp -= 1;
                cur = stack[sp];
            }
        }
    }
    return acc;
}
```

### 4.4 ShortCircuit — the loop-shape rule

Carries the empty-body, predicate-in-condition loop (the measured fast form — vectorizes; a mid-loop
`stop`/`res` write does not). Returns the first hit index or `-1`; ops derive their result.

```java
static int scLeafFwdInt(int[] a, int from, int n, IntPred p) {
    int i = from;
    while (i < n && !p.test(a[i])) {
        i += 1;
    }
    return i; // i == n means "no hit in this run"
}
```

### 4.5 The walk is the single abstraction — push traversers AND pull iterators

**Hard constraint:** the node-visitation logic must be defined once, abstractly, in a form that
generates *both* the push traversers above *and* the pull `iterator`. So the core artifact is a
**direction-aware cursor** over the FBase tree that emits a stream of two events:

- `RUN(array, from, count)` — a contiguous leaf window;
- `SINGLE(value)` — a lone element (`One`, `Prepend`/`Append` elem, `Pad` filler, `Range` step).

Both realizations are generated from that one definition:

- **Push traversers** (§4.2–4.4) drive the cursor to completion: `RUN` → a tight inner loop applying the
  op's function (vectorizes); `SINGLE` → one call. Reverse is realized by **JVM mutual recursion** (fwd
  calls bwd) — legal *only* because a push runs to completion and never suspends.

- **Pull iterators** (per kind × direction) **reify the recursion as explicit state**. An iterator
  can't recurse — `next()` must return to the caller between elements — so the JVM call stack the push
  traverser uses is made explicit: a stack of *frames*, one per live recursive descent, each holding
  `(node, direction, position-or-window)`. `next()` reads the top frame's current run element and
  advances; an exhausted run pops the frame and steps its parent (a `Concat` frame advancing to its
  right child pushes a frame; a `Reverse` frame pushes its base flipped; a `Slice` frame carries the
  window). It is the **same recursive traversal** — the call stack simply held as iterator state. The
  iterator just carries **more state**; it is not a different or weaker walk.

Consequences that drive the rest of the design:

1. The node arms are described once — emit-`RUN` / emit-`SINGLE` / **defer**(node, window) /
   **flip-into**(base). Only `flip-into` (`ReverseNode`) is a recursion, and only in the push traversers:
   `emitTraversers` lowers it to a JVM call (`fwd`↔`bwd`) *for now*, while every `defer` (Concat, Append,
   Prepend, and the Slice/Pad/Updated deep-base) goes on our explicit stack — faster than recursing each
   node, deep chains off the JVM stack. `emitIterators` (which can't recurse) lowers `flip-into` to a
   direction-flipped frame on that same explicit stack. Same arms; the emitters differ only on
   `flip-into`.
2. The primitive iterator is generated **specialized** (`nextInt(): Int`, no boxing); it is wrapped to
   `Iterator[A]` only at the `IndexedSeq` surface boundary.
3. Push traversers and pull iterators are two emitters over **one** walk definition. They must not drift,
   or `iterator` and `foldLeft` could disagree on visitation order — the generator guarantees they share
   the node-arm logic verbatim.

## 5. The surface — a slim inline def over a SHARED leaf method

**The leaf loop lives in ONE shared method, NOT inlined per call site** (see the design-correction note up
top). For the Reduce family the codegen emits, per `(direction, input kind, accumulator shape)`, a SMALL
**non-inline** *shared leaf method* in `object FArrayOps`:

- It peels `Empty` (→ the seed), `${K}One` (one `f.apply(z, elem)`), and the top `${K}Arr` leaf (the
  unboxed leaf loop calling `f.apply`), and for a genuine **tree** makes ONE CALL to the Java traverser
  (`Traversers.reduce{Fwd,Bwd}${K}…`). The tree case is a *call*, never an inlined walk, so the method
  stays small enough for the JIT to inline it at a lone call site yet share it across many.
- Because it is **non-inline and lives in `FArrayOps`' own class**, it references the `Traversers.*`
  statics directly with no `Traversers$` module reference — so it *replaces* the old `foldTree*`
  forwarders (the `NoClassDefFoundError` gotcha only afflicts inline-def bodies).
- The realized SAM closes over the user's `op`/`r` and **wraps the raw element `v` to `A` itself**, so the
  shared leaf method is fully op-agnostic per `(kind, direction, acc-shape)`.

```scala
// SHARED (non-inline) leaf method — one per (dir, input kind, acc-shape); compiled once, JIT-inlinable.
def reduceLeafFwdIntInt(xs: FBase, z: Int, f: Traversers.IntToIntFold): Int =
  xs match
    case e: Empty => z
    case o: IntOne => f.apply(z, o.elem)
    case leaf: IntArr =>
      val a = leaf.arr
      val n = leaf.length
      var acc: Int = z
      var i = 0
      while i < n do
        acc = f.apply(acc, a(i))   // unboxed, vectorizes, == iarray
        i += 1
      acc
    case _ => Traversers.reduceFwdIntInt(xs, z, f)   // ONE call — the cold tree walk
```

The **surface** then carries *no* leaf loop. It only dispatches on the accumulator kind (`summonFrom`),
realizes the user's `inline op` into the specialized fold SAM, and CALLS the shared leaf method:

```scala
inline def foldLeftImpl[A, Z](xs: FBase, z: Z)(inline op: (Z, A) => Z): Z =
  summonFrom
    case r: IntRepr[A] =>
      summonFrom
        // self-kind primitive Z → unboxed IntToIntFold, acc stays a primitive register, NO boxing:
        case rz: IntRepr[Z] =>
          rz.wrap(reduceLeafFwdIntInt(xs, rz.unwrap(z), (acc, v) => rz.unwrap(op(rz.wrap(acc), r.wrap(v)))))
        // Z = Ref → generic IntToRefFold[Z], acc already an object:
        case rz: RefRepr[Z] =>
          reduceLeafFwdIntRef[Z & AnyRef](xs, z.asInstanceOf[Z & AnyRef],
            (acc, v) => op(acc.asInstanceOf[Z], r.wrap(v)).asInstanceOf[Z & AnyRef]).asInstanceOf[Z]
        // cross-prim Z (e.g. Long acc over Int input) → the old lowered walk (threads any-kind Z):
        case _ => …
    case r: LongRepr[A] => …
```

- A primitive-accumulator fold (`sum`, `foldLeft(0)(_+_)`) realizes the SAM as a **single arithmetic op**
  with **zero Box instructions** — the surface dispatches to the unboxed `${I}To${I}Fold` so `acc` never
  leaves a JVM register (verified via `javap`: the SAM body is `iload; iload; iadd; ireturn`).
- The realized SAM is the only function-object allocation (the accepted cost); it is built once at the
  call site and handed to the shared method.
- `sum`/`product`/`count`/`mkString` are the SAME shape: each realizes its combine SAM and calls
  `reduceLeafFwd${K}${K}` (or `reduceLeafFwd${K}Ref` for `mkString`'s `StringBuilder` acc) — no inlined
  leaf loop in any of them.

## 6. Codegen structure (the GenCores rewrite)

Delete: `dfsBody`, the `${K}Dfs` consumer classes (`abstract class ${K}Dfs`), `dfsC{K}{Fwd,Back}`,
`lower`/`Trav`. Keep: `Kind`/`opKinds`/`oneKinds`, `dispatchA`/`dispatchB`, `leaf`/canonicalizers,
`scanB`'s loop-shape knowledge (folds into ShortCircuit).

The core artifact is a single **walk definition** (§4.5): the per-node arms producing `RUN`/`SINGLE`
events over a direction-aware cursor, plus the deferral-stack model. It is data the emitters consume —
not hand-written per target — so push and pull cannot drift. Four emitters:
1. **`emitFunctionTypes`** → the Java interface family (§3), only the realized cells.
2. **`emitTraversers`** → the **push** Java methods `(shape × kind-perm × direction)` (§4.2–4.4), into
   the farray Java sources. One template per shape, expanded over perms/direction from the walk def;
   reverse specialized to JVM recursion.
3. **`emitIterators`** → the **pull** Java state machines (per kind × direction) from the *same* walk
   def; every `recurse-into` reified as a pushed frame `(node, direction, window)`; specialized
   `next${K}()`.
4. **`emitSurface`** → the Scala `inline def`s (§5), one per op, parameterized by `(shape, fn-realizer,
   result-wrapper)`; `iterator` wraps the specialized cursor to `Iterator[A]`.

An **op table** drives it: `op → (shape, fnType, inlineStep, treeRealizer, resultMap)`. Adding an op is
a table row, not a new traverser.

**Formatting (hard requirement):** a small `Emit` helper with an indent level and `line(s)` / `block`;
**never** join statements with `; ` on one line. Generated Java and Scala must read like hand-written
code (the examples above are the standard).

bleep: unchanged wiring — `farray` still has `generate: { project: codegen, main: farray.GenCores }`;
GenCores now writes both the Java function-types/traversers and the Scala surface under the generated
roots.

## 7. Migration (phased; old impl stays until each phase is at parity)

0. **Scaffolding** — `Emit` helper, `emitFunctionTypes`, the op table skeleton. No behavior change.
1. **Reduce** — most ops, validates SAM + accumulator-kind specialization end-to-end (foldLeft/Right,
   sum, product, count, min, max, mkString); confirm no boxing for `Z = Int`. Gate: `bleep test tests`
   parity + bench ≥ parity.
2. **Build + BuildFiltered** — map, mapConserve, filter, filterNot (+ collect/partition variants).
3. **ShortCircuit** — exists/forall/find/indexWhere/…/lastIndexWhere; carry the loop-shape rule; this
   is where the backward traversers get their second user.
4. **Scan, Foreach, Copy**.
5. **Delete** `dfsBody` + consumers once every shape is migrated and green.

`flatMap`, `groupBy`, `zip*`, `sort*`, `iterator` stay on current impls (out of scope §1).

Each phase: keep the old `${op}Impl` behind the new until `FListTest` (132+) is green **and** the
checked-in benchmark report shows no regression; regenerate + commit `docs/bench-results.json` +
`index.html` with the change.

## 8. Risks / open questions

- **Generic-Z boxing** (resolved): the function type is specialized on the accumulator kind — primitive
  `Z` → unboxed `${I}To${Z}Fold` (`acc` a primitive local); `Z = Ref` → generic `${I}ToRefFold<Z>` (`acc`
  already an object). The prototype's 0.38× fold was the generic type realized for `Z = Int` (wrong
  specialization), not a SAM cost. **SAM is the chosen model** — it edged run/one on tree `map`, and its
  per-element shape matches the iterator's `next()`. `foldLeft` must `summonFrom` on `Z`'s kind to pick
  the unboxed fold for a primitive `Z`.
- **Deep-base sub-walk** (`buildWindowFwd…` for Slice over a tree; Pad/Updated over a tree): the part
  the prototype *stubbed with `applyBoxed`*. Must be a real windowed recursion (skip `offset`, take
  `length`). Needs dedicated parity tests (slice/pad/updated over Concat and over Reverse).
- **Method count**: ~100–140 generated Java traversers (shape × perm × dir). Acceptable — generated,
  shared, each compiled once; the whole point is they are *not* inlined per call site.
- **Two-function shapes** (`collect`, `partitionMap`, `minBy`): the op table needs to carry an optional
  second fn; keep the traverser single-function by folding the second into the realized closure where
  possible (`minBy`'s combine wraps the key fn; `collect` realizes one fn that returns a sentinel for
  "undefined").
- **`Append` tail ordering**: forward must visit base-then-elem; reuse the prototype's deferred-tail
  slot on the explicit stack.
```
