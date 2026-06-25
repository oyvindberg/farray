# FSet — design

A fast immutable `Set` counterpart to `FArray`, with FArray's exact properties:

- **Three sibling `opaque type`s over one sealed Java core (`SBase`)** — `FSetView[A]` (membership + lazy algebra only), `FSet[A]` (materialized, enumerable — the default), `FSortedSet[A]` (ordered extras) — each `<: AnyRef`, no subtyping between them, capability split enforced by which extensions exist, transitions via explicit `materialize`/`toSorted` + up-the-lattice `Conversion`s (**see §1.4**, the chosen shape). All three alias `SBase` so there is *no wrapper allocation* and the no-import binding (extensions/givens resolve through each opaque type's implicit scope) is preserved.
- **Per-element-kind specialization** (`Int`/`Long`/`Double` stored *unboxed* in primitive arrays, plus a `Ref` set for everything else), reusing FArray's `Repr[A]` evidence so `summonFrom` picks the kind at compile time — **specialize-or-fail** (abstract `A` ⇒ no `Repr` ⇒ compile error, never a silent boxed fallback).
- **Every lambda-taking op is `inline` with `inline` function params** (`foreach`/`filter`/`map`/`exists`/…), so the user's lambda beta-reduces into the primitive loop and primitives stay unboxed — exactly like FArray.
- **Immutable value semantics**: structural `equals`/`hashCode` (set-semantics: order-independent), `+`/`-`/`++`/`&`/`diff` return new sets, sources are never mutated.

This document is design only. It mirrors the FArray codegen approach (`codegen/src/scala/farray/GenCores.scala`) so an analogous `GenSets` script can emit the `SBase` hierarchy + `FSetOps`.

> **Note on `A` variance.** Unlike `FArray[+A]`, `FSet[A]` is **invariant**. A hash/equality-based set cannot be soundly covariant (`Set[A]` is used in both `contains(a: A)` — contravariant — and iteration — covariant positions), and FArray's own `Set` interop already treats it invariantly (`toSet[B >: A]`). Invariance also removes the covariant-upcast unsoundness trap that FArray had to guard with `case _: AnyRef`.

---

## 1. Chosen representation(s) + rationale

### 1.1 The decision: hybrid **sorted-primitive-array core** + **open-addressing-hash promotion**, *not* CHAMP

FArray's whole thesis is: *primitives unboxed in flat arrays, a tight monomorphic loop the JIT keeps hot, structural sharing only for the cheap O(log n) ops.* The Set design must keep that thesis. Weighing the candidates against it:

| Approach | Unboxed primitives? | `contains` | bulk `&`/`++`/`diff` | iteration / `foreach` | structural sharing on `+`/`-` | Fit with FArray's loop thesis |
|---|---|---|---|---|---|---|
| **CHAMP** (Scala `immutable.HashSet`) | ✗ boxes (`Object[]` nodes) | O(1) amortized, pointer-chasing | node-recursion | tree walk (worse cache) | excellent (persistent) | poor — boxes, no tight prim loop |
| **Open-addressing prim hash** (fastutil/Koloboke/HPPC) | ✓ | **fastest** (one array, cache-line-local linear probe) | rebuild | linear array scan | ✗ mutable | excellent — *but mutable* |
| **Sorted primitive array** | ✓ | O(log n) branchless binary search | **merge** (linear, SIMD-able) | sequential, perfectly ordered | O(1) slice sharing; `+`/`-` = O(n) copy | excellent for small/medium |
| **Roaring bitmap** | ✓ (Int only) | O(1) | native AND/OR/ANDNOT | popcnt-driven | container sharing | excellent *but Int-only, niche* |

No single structure wins everywhere, so — exactly like FArray's flat-array-vs-tree hybrid — FSet is a **threshold hybrid**: materialized *leaf* shapes per kind, plus **lazy set-algebra tree nodes** that defer the merge (see §1.3, which is the heart of the design):

```
SBase  (sealed)
 ├── Empty                         the one empty set (kind-agnostic)
 ├── {Int,Long,Double,Ref}One      singleton, one element, no array (mirrors FArray's One)
 ├── {Int,Long,Double,Ref}Sorted   SORTED unboxed prim array  — small/medium materialized leaf
 ├── {Int,Long,Double,Ref}Hash     OPEN-ADDRESSING (frozen) prim hash table — large materialized leaf
 ├── IntDense                      ROARING-style Int bitmap — dense Int materialized leaf
 ├── Union(l, r) / Inter / Diff / Xor   LAZY algebra nodes — O(1) construct, share operands (§1.3)
 └── (SliceNode-style window over a Sorted leaf — O(1) SortedSet range ops)
```

The leaf shapes below are the *materialized/canonical* forms (what a lazy node folds into on first whole-set access). The lazy nodes are covered in §1.3.

**Why sorted-array as the default leaf (the FArray "Arr" analogue):**

1. **Branchless binary-search `contains`** on a flat `int[]`/`long[]`/`double[]` is monomorphic, cache-friendly, and unboxed — the same loop shape FArray proved the JIT keeps at IArray parity. For the small-to-medium sets that dominate real code, O(log n) on a 64-byte-cache-line-local array beats CHAMP's pointer-chasing and ties open-addressing.
2. **Bulk set ops are merges.** `&` (intersect), `++` (union), `diff`, `subsetOf` over two sorted arrays are *single linear merge passes* — no hashing, no per-element boxing, SIMD-friendly, and they emit a sorted result directly (no re-sort). This is where FSet beats every hash-based set (`immutable.HashSet`, fastutil, `j.u.HashSet`): bulk algebra on two sorted runs is the textbook-optimal shape, and galloping handles the very-different-size case in O(m log n). ([Lemire][lemire-intersect], [VLDB Inoue et al.][vldb-simd])
3. **Perfectly ordered, cache-sequential iteration / `foreach`** — falls straight out of the array, the FArray-style tight `while` loop, no tree walk. (CHAMP's headline weakness is exactly iteration/equality cache behaviour. [Steindorfer OOPSLA'15][champ-paper])
4. **Cheap structural sharing for the read-mostly ops:** `take`/`drop`/`slice`-of-a-sorted-set, `min`/`max`, `rangeFrom`/`rangeUntil` (`SortedSet` ops) are O(1)/O(log n) windows over the shared array, mirroring FArray's `SliceNode`.

**Why promote to an open-addressing hash leaf past a threshold:**

Sorted-array `+`/`-` (single-element insert/remove) is O(n) (binary-search + arraycopy). For *large* sets that is the wrong cost. Past `HASH_THRESHOLD` (a tunable ~64–128 elements; benchmark-driven, same way FArray tunes its flatten threshold) a leaf is built/stored as a **frozen open-addressing table**: one primitive `int[]`/`long[]`/`double[]` of slots (linear probing, power-of-two capacity, ~0.5–0.6 load factor, a `byte[]`/sentinel for occupancy), giving **O(1) unboxed `contains`** with fastutil-class cache locality (the next ~32 probe slots share a cache line). ([fastutil internals][fastutil-internals], [java-performance benchmarks][java-perf]) "Frozen" = built once via the fast build path, then immutable — we get open-addressing's speed without its mutability hazard (see §3).

**Why a Roaring-style `IntDense` leaf (Int only):** for large, clustered/dense Int sets (the id-set / posting-list use case) a chunked bitmap gives O(1) `contains` and turns `&`/`++`/`diff` into native word-wise `AND`/`OR`/`ANDNOT` with `popcnt` cardinality — hundreds of times faster than element-wise merging on dense data, and smaller in memory. ([Roaring][roaring], [Chambi et al.][roaring-paper]) This is a *specialization within the Int kind*, entered on a density heuristic (cardinality / span) or an explicit `FSet.denseInt` factory; for sparse/random Int sets we stay on Sorted/Hash (Roaring explicitly degrades on dense-random no-run data — there a plain bitset/hash wins). Treat `IntDense` as a **Phase-3 opt-in**, not part of the first milestone.

**Why not CHAMP at all:** CHAMP is the right structure when you need cheap *persistent* single-element `+`/`-` with deep structural sharing **and** you are already boxing (Scala's `Set[A]`). FSet's whole reason to exist is to *not box* and to keep FArray's flat-loop performance. CHAMP's `Object[]`-node representation is fundamentally incompatible with unboxed primitives, and its iteration/equality cache behaviour is its documented weak spot — the exact axes FArray optimizes. We deliberately trade CHAMP's O(log32 n) persistent single-element update for unboxed storage + linear-merge bulk algebra, betting (as FArray does) that real workloads are build-once + query/bulk-op-many, not incremental single-element churn on huge sets. Where incremental churn *does* dominate a huge set, the Hash leaf's O(1) `+`/`-`-by-rebuild-on-grow (transient builder, §3) covers it.

### 1.3 The lazy algebra tree — `contains` is the primary op, so defer the merge

**This is the core of the design, and it reuses FArray's tree directly.** The framing: the dominant set operation is **membership** (`contains`), and membership distributes cheaply over the set-algebra operators *without ever materializing the result*:

```
contains(x, Union(l, r))   = contains(x, l) || contains(x, r)      // proving: short-circuit on first hit
contains(x, Inter(l, r))   = contains(x, l) && contains(x, r)      // disproving: short-circuit on first miss
contains(x, Diff(l, r))    = contains(x, l) && !contains(x, r)
contains(x, Xor(l, r))     = contains(x, l) ^  contains(x, r)
```

So `Union`/`Inter`/`Diff`/`Xor` become **O(1)-construct lazy nodes** that *share both operands*, exactly like FArray's `Concat`. The algebra is free to build; the cost is paid lazily at query time, and membership is a tree-walk of cheap leaf oracles. Proving membership short-circuits (often O(depth) ≈ O(1) for shallow trees); disproving walks the relevant branches — bounded by the tree shape, and within a leaf it is O(log n) (Sorted) or O(1) (Hash/Dense). This is FArray's lazy-structural-op bet applied to set algebra, and the user is right that it is the natural shape: **build the algebra for free, answer `contains` cheaply, and only materialize (merge) when an op genuinely needs the whole element set.**

#### Representation: leaves + algebra nodes

```
SBase (sealed; every node knows its operands but NOT necessarily its exact size — see below)
 ├── Empty
 ├── {Int,Long,Double,Ref}One        singleton
 ├── {Int,Long,Double,Ref}Sorted     sorted unboxed prim array   (a materialized leaf)
 ├── {Int,Long,Double,Ref}Hash       frozen open-addressing table (a materialized leaf)
 ├── IntDense                         Roaring-style Int bitmap     (a materialized leaf)
 ├── Union(l, r)                      lazy ∪  — O(1) construct, shares l & r
 ├── Inter(l, r)                      lazy ∩  — O(1) construct
 ├── Diff(l, r)                       lazy ∖  — O(1) construct
 └── Xor(l, r)                        lazy ⊕  — O(1) construct
```

`+x` = `Union(this, One(x))`; `-x` = `Diff(this, One(x))`. Both O(1), both share the base — finally giving us FArray-style O(1) `+`/`-`/`++` with full structural sharing (the thing CHAMP buys with complex node-splicing, we get with a 2-field node).

#### Per-operation complexity under this model

| Op | Lazy-tree cost | Notes |
|---|---|---|
| `contains(x)` **prove** | **O(depth)**, short-circuits on first leaf hit | the hot path; shallow tree ⇒ ≈O(1) + one leaf probe |
| `contains(x)` **disprove** | O(#relevant leaves · leafProbe) | bounded by tree; `Union` must check all children, `Inter` stops at first miss |
| `+x` / `-x` | **O(1)** | `Union`/`Diff` with a `One` node, shares base |
| `++` / `&` / `diff` / `xor` | **O(1) construct** | the lazy node *is* the result; merge deferred |
| `subsetOf(that)` | O(\|this\| · containsThat) | stream `this`'s leaves, probe `that` — no materialization |
| `forall` / `exists` / `find` | O(n) walk, short-circuits | walk left leaves, probe the rest of the tree per element |
| `foreach` / `iterator` | O(n) **but must dedup** | **the catch** — see below |
| `size` | **not O(1) on a lazy node** | **the catch** — see below |
| `equals` / `hashCode` | needs canonical element set | materialize-on-demand |

#### The two real costs of laziness, and how we pay them

1. **`size` is no longer the free `length` field.** `Union(l, r).size` ≠ `l.size + r.size` when the operands overlap. Options, all viable: (a) compute `size` lazily and **memoize** it on the node (one walk, then O(1) forever — most sets get their size asked once); (b) keep cheap *bounds* on every node (`sizeLo`/`sizeHi`, e.g. `max(l,r) .. l+r` for union) for the many call sites that only need `isEmpty`/`nonEmpty` or a capacity estimate — exact size forces materialization only when truly demanded. `isEmpty` is cheap structurally (`Union` empty iff both empty; `Inter`/`Diff` need a probe but short-circuit).

2. **Order-sensitive / whole-set ops must dedup, so they trigger materialization.** `foreach`/`iterator`/`toFArray`/`equals`/`hashCode`/`map`/`filter` need *each element exactly once*. Over a lazy `Union(l, r)` a naive walk yields duplicates (elements in `l ∩ r` twice). So these ops **materialize the tree first** — fold the algebra into a single Sorted/Hash/Dense leaf via the linear merge (§3.2), which is the O(m+n) unboxed pass, then iterate the clean leaf. Crucially this is **the same merge work I previously claimed was eager — it just moves to first whole-set access and is memoized.** A set that is only ever `contains`-queried *never pays it*; a set that is iterated pays it once.

#### Materialization policy (the FArray "flatten threshold" analogue)

- **Lazy stays lazy** for `contains`/`+`/`-`/`++`/`&`/`diff` chains — the common "build an algebra, ask membership" pattern runs allocation-light and fast.
- **First whole-set op materializes** the node to a single leaf (merge), and the node **caches** the materialized leaf (replaces its operand refs, like a memo-thunk) so subsequent reads are leaf-speed. Sound because immutable: the materialized leaf is value-equal to the lazy tree.
- **Depth/width threshold**: a very deep lazy tree (e.g. `+` in a tight loop building a huge set) would make `contains` O(depth) and risk stack/cache cost — so past a tunable depth we eagerly materialize. **(Caveat — this is a *proposed* FSet mechanism, not borrowed from a working FArray feature. FArray does *not* currently have a construction-time flatten/coalesce threshold: its `concat` smart constructor just builds a `Concat` node unconditionally, and its only "flatten" is materialize-*on-read* during traversal/iteration. A construction-time threshold was planned for FArray but never landed — see the open risk below.)** The transient builder (§3.2) remains the right tool for million-element incremental construction; the lazy `+` tree shines for modest algebra, not unbounded loops.
- **Dense-Int bulk ops** (`IntDense & IntDense`) materialize *eagerly* via native `AND`/`OR`/`ANDNOT` — for Roaring leaves the word-parallel merge is so cheap there is no reason to defer it, and it keeps `contains` O(1).

#### What this changes vs. the earlier draft

This **supersedes** the "eager merge, no combinator nodes" stance. The set's no-dup invariant does *not* forbid lazy algebra nodes — it only means the invariant must be restored *at the point an op needs the canonical element set* (iteration/equality/materialization), **not** at every `contains`. Membership, `+`, `-`, and bulk algebra all stay lazy and cheap, exactly as the user argues. We still keep the materialized leaves (Sorted/Hash/Dense) as the canonical/coalesced form, and we still reuse FArray's `SliceNode` window for `SortedSet` range ops over a Sorted leaf. `Reverse`/`Updated`/`Pad` remain meaningless for a set and are dropped; `Concat`→`Union`, `Append`→`Union(_, One)` are the set-correct analogues (union, not sequence-concat, so they carry the dedup obligation forward lazily).

**Bottom line:** FArray's tree *is* useful here — more than I first credited. The right mapping is sequence-`Concat` → set-`Union`, and the key insight is that a set's defining op (`contains`) distributes over the algebra so the merge can be deferred indefinitely and is only forced (and then memoized) by ops that need the deduplicated whole.

### 1.4 Three sibling opaque types — capability split (CHOSEN)

**Decision (user):** FSet is **three distinct opaque types, with NO subtyping relationship between them** — a capability lattice expressed by *which extensions exist*, not by `<:`:

```scala
opaque type FSetView[A]   <: AnyRef = SBase   // answers ONLY membership + lazy algebra
opaque type FSet[A]       <: AnyRef = SBase   // a materialized, enumerable set (the default user type)
opaque type FSortedSet[A] <: AnyRef = SBase   // a materialized set with a known order (SortedSet extras)
```

All three alias the same Java `SBase` core (zero wrapper, no-import binding preserved per type), but each companion exposes a *different op surface*, so the compiler enforces the capability split. We **build `FSetView` first** (it is the lazy algebra tree of §1.3, the simplest to implement and the foundation the other two materialize from), then `FSet`, then `FSortedSet`.

**Name (chosen):** `FSetView` — "a view onto a set that answers membership without committing to enumerate it." Note this is *not* `scala.collection.View` and does not extend it; because of the no-import binding a user only ever brings `farray.FSetView` into scope, so the shared word "view" is cosmetic, not a type clash. The reading is apt: a `View` is queryable but lazy, and `.materialize`/`.toSet` is "force the view into a concrete set."

**Why siblings, not subtypes (settled — not relitigated):** opaque types cannot form a subtyping lattice among themselves (each is a fresh abstract type; the only `<:` you can declare is a single bound against a *transparent* type, which we already spend on `AnyRef`). `FSortedSet <: FSet <: FSetView` is simply not expressible. The user has accepted this: the three are nominally unrelated to the compiler, and movement between them is by **explicit conversion methods**, not upcast. This is *cleaner* than a fake hierarchy — each transition is a visible, intentional, once event.

#### Op surface per type (the whole point — the compiler enforces this)

| Op group | `FSetView` | `FSet` | `FSortedSet` |
|---|---|---|---|
| `contains` / `apply` / `isEmpty` / `nonEmpty` | ✅ | ✅ | ✅ |
| `subsetOf` | ✅ | ✅ | ✅ |
| algebra `+ - ++ & diff xor` (lazy nodes) | ✅ (stays in `FSetView`) | ✅ (stays in `FSet`, may re-lazy-or-eager) | ✅ |
| `sizeLo` / `sizeHi` bounds | ✅ | ✅ (exact `size` too) | ✅ |
| **exact `size`** | ❌ (would force a walk) | ✅ O(1) field | ✅ |
| `foreach` / `iterator` / `map` / `flatMap` / `filter` / `collect` | ❌ | ✅ | ✅ |
| `equals` / `hashCode` (value semantics) | ❌ (or forces) | ✅ | ✅ |
| `toFArray` / `toArray` / `toList` / `toSet` | ❌ | ✅ | ✅ |
| `min` / `max` / `rangeFrom` / `rangeUntil` / `range` / `firstKey` / `lastKey` / ordered `iterator` | ❌ | ❌ | ✅ (`using Ordering`) |

Transitions (the only ways to move between types):
- `FSetView.materialize: FSet[A]` (alias `.toSet`) — folds the lazy tree to one `Sorted`/`Hash`/`Dense` leaf via the §3.2 merge, **memoizes** it, returns the `FSet` view of the same `SBase`. The expensive, deduplicating, once event — now **syntactically explicit and type-level**, exactly the user's goal.
- `FSetView.toSorted(using Ordering[A]): FSortedSet[A]` — materialize specifically to a `Sorted` leaf.
- `FSet.toSorted(using Ordering[A]): FSortedSet[A]` — re-materialize/guarantee a sorted leaf if not already one.
- **Widening is free and implicit-where-wanted:** an `FSet` (and `FSortedSet`) trivially *also* answers membership, so a no-import `Conversion[FSet[A], FSetView[A]]` (and `FSortedSet ⇒ FSet`, `FSortedSet ⇒ FSetView`) in each companion's implicit scope lets a materialized set be passed anywhere a view/`FSet` is wanted, at zero cost (same `SBase`). The conversions only go *up the capability lattice* (more-capable ⇒ less-capable); going *down* always costs a `materialize`/`toSorted` call. This recreates the subtyping ergonomics (pass an `FSortedSet` where an `FSetView` is expected) without subtyping.

#### Internal core split mirrors the public types

Inside the Java core, the `SBase` sealed hierarchy gets two sealed intermediates so each public type's invariant has a physical home (this is free — the leaves/nodes exist regardless):

```
SBase (sealed)
 ├── SView           // lazy algebra: Union/Inter/Diff/Xor/One/Empty  — the FSetView backing
 └── SMaterialized   // deduplicated, enumerable: Sorted/Hash/Dense    — the FSet backing
        └── (Sorted leaf + SliceNode window are the FSortedSet-capable shapes)
```

A materialized `FSetView` *is* an `SBase` whose runtime node is now an `SMaterialized` (after memoize) — but its **static** type is still `FSetView` until the user calls `materialize`, which is precisely how the type system tracks "have you paid for the element set yet." `size`/`iterator` are virtuals only `SMaterialized` implements cheaply; `FSet`'s extensions can rely on that because you can only *get* an `FSet` by materializing.

#### Cost / discipline

Three opaque types ⇒ three companions' worth of no-import implicit scope for `GenSets` to emit, and the membership/algebra ops are duplicated across all three surfaces (or reached via the up-conversions). Against FArray's known JIT method-size pressure, the discipline is the same: inline only the thin dispatch + user-lambda loop; the merges/sort/freeze/`materialize` live as non-inline per-kind `FSetOps` helpers compiled once and shared by all three companions. `FSortedSet`'s extras are a small `using Ordering`-gated group over the `Sorted` leaf, so its incremental surface is light.

### 1.2 Specialization / unboxing strategy (mirrors FArray's codegen)

Identical mechanism to FArray — **reuse `Repr[A]` verbatim** (it already encodes `Int`/`Long`/`Double` prim arrays + `RefRepr.ct: ClassTag[A]`; no new evidence type):

- A `GenSets` `BleepCodegenScript` (sibling of `GenCores`) emits the sealed Java `SBase` + per-kind `${K}Sorted` / `${K}Hash` / `${K}One` leaves from one template per shape, plus `FSetOps.scala` with the per-kind specialized combinators.
- Element ops dispatch via `summonFrom { case r: IntRepr[A] => …; case r: LongRepr[A] => …; case r: DoubleRepr[A] => …; case r: RefRepr[A] => … }` — the branch is selected at the concrete inline call site, compiling to a single static path with zero runtime type test (the same `getstatic given_…Repr` reduction FArray proved).
- Primitive leaves store `int[]`/`long[]`/`double[]`; the `Ref` leaf stores `Object[]` and uses `RefRepr.ct` to allocate a *typed* `String[]`/etc. so element reads are a bare `aaload` (no per-element checkcast) — the same "typed-ref" trick FArray uses to match IArray on reference types.
- `Ref` sets need an `Ordering[A]`/`Hashing` for the sorted/hash leaves. Sorted-`Ref` requires an `Ordering` (so `RefSorted` is only available `using Ordering[A]`); without one, a `Ref` set falls to `RefHash` keyed on `hashCode`/`equals` (the only universally-available contract). Primitives use natural order + identity hash — no user evidence needed.
- Adding a primitive = **one row** in `GenSets.prims` (just like FArray).

The hash function for primitive leaves is an unboxed avalanche mix (e.g. fibonacci/`murmur`-finalizer on the raw `int`/`long`/`double`-bits), computed inline — never `Integer.hashCode` boxing. The set's *structural* `hashCode` is order-independent (sum/xor of per-element unboxed element-hashes, matching `scala.collection.Set.hashCode`'s `MurmurHash3.unorderedHash` with seed `"Set".hashCode`) — analogous to FArray's `Hashing` but commutative, so two FSets with the same elements in different leaf shapes hash equal.

---

## 2. Core API

Three sibling opaque types (§1.4): `opaque type FSetView[A] <: AnyRef = SBase`, `opaque type FSet[A] <: AnyRef = SBase`, `opaque type FSortedSet[A] <: AnyRef = SBase`. Each has a companion holding factories + `inline` extension methods + givens (no-import). The tables below show the **union** of the surface; the right-hand column / §1.4 table says *which type* each op lives on — **Query** + **Algebra** are on all three (algebra returns a lazy `FSetView`); **Transform/iterate** live on `FSet`/`FSortedSet` only (you reach them via `materialize`); **SortedSet extras** are `FSortedSet`-only. Marking: **inline** = inline method, **spec** = per-kind specialized (unboxed prim path), **lam** = takes an `inline` lambda param.

### Construction / build
| Op | Inline | Notes |
|---|---|---|
| `FSet.empty[A]` | inline / spec | returns the shared `Empty` |
| `FSet(a, b, c, …)` | inline / spec | small-arity overloads avoid varargs boxing (mirrors FArray) |
| `FSet.fromArray[A](xs: Array[A])` | inline / spec | fast unboxed build path (§3) |
| `FSet.fromFArray[A](xs: FArray[A])` | inline / spec | zero-box bridge: dedup directly off the unboxed FArray leaf |
| `FSet.from[A](it: IterableOnce[A])` | inline / spec | generic build |

### Query (the hot read path) — distributes over the lazy algebra (§1.3)
| Op | Inline | Notes |
|---|---|---|
| `contains(a: A)` / `apply(a)` | inline / spec | **the** hot op. Leaf: Sorted ⇒ branchless binary search, Hash ⇒ O(1) probe, Dense ⇒ word test. Lazy node: OR/AND/AND-NOT-walk of child `contains`, short-circuiting. Unboxed compare. |
| `isEmpty` / `nonEmpty` | inline / spec | structural & short-circuiting (`Union` empty iff both empty; `Inter`/`Diff` probe but bail early) |
| `size` | inline / spec | leaf: O(1) field. Lazy node: **computed once + memoized on the node** (folds the tree); `sizeLo`/`sizeHi` bounds answer capacity/`isEmpty` queries without materializing |
| `subsetOf(that)` | inline / spec | stream `this`'s leaves, probe `that` — no materialization; merge fast-path for sorted×sorted |
| `forall` / `exists` / `find` / `count` | inline / spec / lam | short-circuit walk of left leaves + tree probe, exact FArray scan shape |

### Algebra (where FSet wins) — lazy, O(1) construct
| Op | Inline | Notes |
|---|---|---|
| `+ (a)` / `incl` | inline / spec | `Union(this, One(a))` — **O(1)**, shares base; returns `this` if a cheap `contains` proves present (alias, sound by immutability) |
| `- (a)` / `excl` | inline / spec | `Diff(this, One(a))` — **O(1)**; returns `this` if absent |
| `++ (that)` / `union` / `\|` | inline / spec | `Union(this, that)` — **O(1)** lazy node; merge forced only on materialization |
| `& (that)` / `intersect` | inline / spec | `Inter(this, that)` — **O(1)**; dense×dense = eager bitwise AND |
| `diff(that)` / `--` / `&~` | inline / spec | `Diff(this, that)` — **O(1)**; dense = ANDNOT |
| `xor(that)` (symmetric diff) | inline / spec | `Xor(this, that)` — **O(1)**; dense = XOR |

### Transform / iterate — force materialization (dedup) once, then memoize
| Op | Inline | Notes |
|---|---|---|
| `foreach(f)` | inline / spec / lam | **materializes** the lazy tree to one leaf (merge) on first access, caches it, then tight unboxed loop; leaf is direct |
| `filter(p)` / `filterNot(p)` | inline / spec / lam | over the materialized leaf, one pass, builds the same-kind leaf (already deduped) |
| `map[B](f)` | inline / spec / lam | read kind × **write kind** matrix (like FArray's NxN map); builds a new FSet of kind `B`, **deduping** as it goes |
| `flatMap[B](f)` | inline / spec / lam | build + dedup into kind-`B` leaf |
| `collect` / `partition` | inline / spec / lam | composed from filter/map |
| `iterator` | inline / spec | over the materialized leaf — flat unboxed cursor (scalar-replaced when consumed inline), ordered for Sorted/Dense |
| `toFArray` | inline / spec | materialize → zero-box bridge to `FArray[A]` (sorted leaf array *is* an FArray leaf) |
| `toArray` / `toList` / `toSet` | inline / spec | standard egress (materialize first) |

### `SortedSet` extras (free on the Sorted leaf, `using Ordering`)
`min` / `max` (O(1)), `rangeFrom` / `rangeUntil` / `range` (O(log n) window), `firstKey` / `lastKey`, ordered `iterator` — all O(1)/O(log n) over the shared sorted array, no equivalent cheap form in a hash set. These are a *bonus* FSet gives that `immutable.HashSet` cannot.

---

## 3. Build path vs update path

### 3.1 Fast unboxed BUILD path (`fromArray`/`from`/`fromFArray`/`map`/`filter`)

The dominant real-world pattern is *build a set once, query/bulk-op it many times.* So the build path is the one to make fastest, and it is where the "build-then-freeze" idea pays off without exposing mutability:

1. **Unwrap to a primitive scratch array.** Pull the source's elements into an `int[]`/`long[]`/`double[]` (for `fromFArray`/`fromArray` this is often the backing array verbatim — zero copy if we can consume it). The `Ref` kind uses a typed `Object[]` via `RefRepr.ct`.
2. **Dedup.** Choose by size:
   - **small/medium** → **sort + unique-compact** in place (one `Arrays.sort` of the prim array — unboxed dual-pivot — then a single linear pass dropping equal neighbours). Result: a `${K}Sorted` leaf, the common case, with *no hash table allocated at all*.
   - **large** → a **transient open-addressing table** (mutable, kind-specialized, internal — never escapes): insert each unwrapped element with linear probing, then **freeze** (wrap the final slot array into an immutable `${K}Hash` leaf with **no copy** — the FArray "freeze-without-copy" trick). This is the build-then-freeze pattern: open-addressing's build speed, zero mutability surface.
   - **dense Int** → write bits straight into a Roaring-style container set.
3. **One pass, unboxed throughout.** `map`/`filter`-into-a-set thread the user's `inline` lambda directly into the unwrap loop (no intermediate boxed collection), exactly like FArray's `mapImpl`/`filterImpl` — the lambda inlines, the primitive never boxes. Dedup happens against the chosen transient structure as elements are produced.

A `GroupSet`-style growable unboxed buffer per kind (mirroring FArray's `${K}Group` for `groupBy`) backs the transient builder: doubling primitive array, `add` appends an already-unwrapped prim, `freeze`/`toLeaf` wraps without a second per-element pass.

### 3.2 Immutable UPDATE path (`+`/`-`/`++`/`&`/`diff`)

All return a fresh `FSet`, never mutate. The **default is the O(1) lazy node** (§1.3): `+a` = `Union(this, One(a))`, `++that` = `Union(this, that)`, `&` = `Inter`, `diff` = `Diff`. Construction is O(1) and shares the operands; no element is touched, nothing is merged. `+a`/`-a` first do a cheap `contains` probe and **return `this`** (alias — sound because immutable, FArray's no-op free-win) when the result is unchanged.

**The merge is the *materialization* of a lazy node**, run lazily on first whole-set access (`foreach`/`iterator`/`equals`/`map`/…) or when the depth threshold forces it, and then **memoized** on the node:

- **Sorted×Sorted** → linear merge of the two sorted prim arrays into a fresh sorted leaf — O(m+n), unboxed, no hashing, no boxing, emits sorted (no re-sort). Galloping (binary-search the larger) when sizes are very skewed (O(min·log max)).
- **Hash** materialization → probe-and-build a frozen `${K}Hash` leaf.
- **Dense Int** → native `AND`/`OR`/`ANDNOT` (materialized eagerly — word-parallel, keeps `contains` O(1)).

For **hot incremental single-element construction** (millions of `+` in a loop — where a deep lazy `Union` chain would make `contains` O(depth)), use the **transient `FSetBuilder`** (`+=` mutating into an open-addressing table, `result()` freezes) — the idiomatic escape hatch, same role as `ArrayBuffer`→FArray. The lazy `+`/`++` tree is for *modest* algebra (the common "a few unions/diffs then query" pattern), not unbounded loops. The proposed depth threshold would auto-materialize a pathological tree — but note (§1.3 caveat) FArray itself never shipped an analogous construction-time threshold, so this is unproven territory FSet must validate, not a pattern to copy.

Cross-shape materialization (Sorted with Hash, etc.) normalizes to the cheaper algorithm: materialize the smaller as a membership oracle and stream the larger, or both-to-sorted and merge — chosen by a size/shape table, analogous to FArray's leaf/tree fast-path dispatch.

---

## 4. Expected performance vs `immutable.HashSet`, fastutil, `j.u.HashSet`

Reasoning from FArray's measured results (FArray ties IArray = the unboxed-array ceiling, and beats List/Vector 2–7×) plus the algorithm literature:

| Workload | vs `scala.immutable.HashSet` | vs fastutil `IntOpenHashSet` | vs `j.u.HashSet<Integer>` | Why |
|---|---|---|---|---|
| **`contains`, primitive, medium set** | **win** (no box, no node pointer-chase; binary search on cache-local array) | **tie** (both unboxed; hash O(1) vs our O(log n) — hash leaf closes it for large) | **big win** (they box every key + node hop) | unboxed + flat array |
| **`contains`, primitive, large set (Hash leaf)** | **win** | **tie** (same open-addressing shape) | **big win** | unboxed open-addressing |
| **build from `int[]`/FArray** | **big win** (no boxing, no per-element node alloc; sort+unique or freeze) | **tie/win** (we can consume the source array; both unboxed) | **big win** | freeze-without-copy, zero box |
| **`&` / `++` / `diff`, two sorted prim sets** | **big win** (linear merge vs node-recursion + boxing) | **win** (fastutil rebuilds via per-element insert; we merge two sorted runs in one linear pass, SIMD-able) | **big win** | merge beats hash-rebuild on bulk |
| **`&` / `++` / `diff`, dense Int (Dense leaf)** | **huge win** | **win** | **huge win** | native AND/OR/ANDNOT + popcnt |
| **ordered iteration / `foreach`** | **win** (sequential array vs tree walk — CHAMP's documented weak spot) | **tie** (both array scans) | **win** | flat cache-sequential array |
| **structural `equals` / `hashCode`** | **win** (CHAMP equality is its known weakness; ours is one unboxed commutative pass) | n/a (mutable, no value equality) | **win** | unboxed unordered hash |
| **`+`/`-`/`++`/`&`/`diff` *construction* (then `contains`-query)** | **win** (O(1) lazy node vs CHAMP's per-element node-splice; `contains` distributes over the tree) | **win** (no allocation/insert at all until forced) | **big win** | lazy algebra node (§1.3) |
| **deep single-element `+` loop *materialized to a whole-set op*** | **tie/lose** (a forced deep `Union` chain ≈ CHAMP's persistent share; use the transient builder) | n/a (mutable in place) | n/a | depth threshold + transient builder blunt it |
| **reference-type sets** | **tie→win** (typed `Ref` array, no per-elem checkcast; needs `Ordering` for sorted, else hash) | n/a | **win** | typed-ref trick |

**Net:** FSet wins decisively on *build*, *bulk algebra construction* (`&`/`++`/`diff` are O(1) lazy nodes), *membership over an algebra* (`contains` distributes and short-circuits), *iteration*, and *equality* — the operations real code does most. The lazy tree (§1.3) means algebra is allocation-free until a whole-set op forces (and memoizes) the merge; a set that is only ever queried never pays it. The one concession — a pathologically deep single-element `+` loop that is then iterated — is blunted by the auto-materialize depth threshold and the transient builder, the same way FArray handles million-element incremental builds. Against fastutil it matches raw speed while adding immutability + value semantics + lazy algebra + ordered/`SortedSet` ops for free; against `j.u.HashSet` and boxed `immutable.HashSet` it wins almost everywhere by never boxing.

---

## 5. Open questions, risks, first milestone

### Open questions
1. **Lazy-tree depth threshold** — at what `Union`/`Diff` chain depth does `contains`'s O(depth) walk (and cache cost) justify auto-materializing the tree? JMH-sweep, mirroring FArray's `Concat` flatten threshold. Also: should `+` in a detected loop eagerly switch to the transient builder?
2. **Memoization mechanics for materialized lazy nodes** — replace operand refs in place (mutate-once memo thunk, needs a benign data race / `@volatile` like FArray's structural caches) vs return a fresh leaf and let the caller hold it. The in-place memo is what makes "iterate twice" cheap; confirm the immutability story (value-equal, so the race is benign).
3. **`size` strategy** — memoize-on-first-ask vs always-eager vs `sizeLo`/`sizeHi` bounds only. Probably bounds + memoized exact, computed on first exact-`size` call.
4. **`HASH_THRESHOLD` value** — where does sorted-array O(log n) `contains` + O(n) materialized `+` stop beating an open-addressing leaf? FArray-style JMH sweep (`-prof gc`, `gc.alloc.rate.norm`); likely ~64–128, possibly per-kind.
5. **Do we even ship the Hash leaf in v1, or is Sorted-only enough?** Sorted alone already beats `immutable.HashSet` on build/algebra/iteration; Hash matters mainly for huge sets with point `contains`/churn. Could be Phase 2.
3. **`Ref` ordering** — require `Ordering[A]` for `RefSorted` (gives the `SortedSet` bonus + merge algebra) vs always-`RefHash` (no evidence needed). Probably: prefer `RefSorted` when an `Ordering` is in scope, else `RefHash`. Resolve via `summonFrom` on `Ordering[A]` at the build site.
4. **Equality across leaf shapes** — a `Sorted` set and a `Hash` set with identical elements must be `equals` and hash-equal. Guaranteed by the order-independent structural hash + an element-membership equality check (not array-compare). Verify no shape leaks into `equals`.
5. **Dense-Int density heuristic** — auto-detect (cardinality/span ratio, run count) vs explicit `FSet.denseInt` factory. Roaring degrades on dense-*random* no-run data, so the heuristic must check for runs, not just density.
7. **Three-type capability split (§1.4) — DECIDED: `FSetView` / `FSet` / `FSortedSet`** as three sibling opaque types (no subtyping; opaque sub-typing isn't expressible), built `FSetView`-first, with `SView`/`SMaterialized` intermediates in the Java core and explicit `materialize`/`toSorted` transitions + up-the-lattice `Conversion`s. *Sub-questions still open:* (a) does `FSet`'s own algebra (`a ++ b` where both are already `FSet`) return a lazy `FSetView` (forcing a re-`materialize`) or stay eager and return an `FSet` directly? Probably: `FSet`-algebra returns `FSet` but may *internally* hold a lazy node until first whole-set access — same memoize trick, no type change. (b) Confirm three companions' implicit scopes coexist (no-import resolution) without ambiguity, especially the up-conversions — verify a second/third opaque type in the same package each gets its own clean implicit scope (mirrors the §5 `Repr`-reuse risk).
8. **Map sibling (`FMap`)** — the same core generalizes to a key+value set (sorted parallel arrays / open-addressing key+value). Out of scope here, but the design should not foreclose it (keep `SBase` key-centric so `FMap` can reuse the key machinery).

### Risks
- **Codegen size.** `FSetOps` adds another NxN-ish matrix (`map` write-kind × read-kind, plus per-kind merge ops). FArray already hit JIT method-size limits → moved non-inline helpers (sorts, dfs, merges) out of the inline path. Apply the same discipline: inline only the thin dispatch + the user-lambda loop; keep merges/sort/freeze as non-inline per-kind `FSetOps` methods compiled once.
- **Three leaf shapes × ops** is more surface than FArray's leaf/tree split. Mitigate by funnelling all bulk ops through "membership oracle + ordered stream" so most cross-shape combinations reuse one code path.
- **Build cost regressions** if dedup picks the wrong structure at the boundary — guard with the threshold sweep and a `contains`-cost model.
- **`Repr` reuse** assumes FArray's `Repr.scala` is importable from the set package; it is generated into `package farray`, so `GenSets` should emit into the same package (or `FSetOps` imports it). Confirm the opaque-scope/no-import resolution still holds for a second opaque type in the same package.
- **Depth threshold is unproven, not borrowed.** FArray currently has **no** construction-time flatten/coalesce threshold — its `concat` always builds a `Concat`, and "flatten" only happens as materialize-on-read during traversal; a construction-time threshold was planned for FArray but never landed. So FSet's auto-materialize-deep-`Union` policy is a *new* mechanism that must be designed and benchmarked from scratch, not assumed-working. The safe fallback if it proves fiddly: lean entirely on the transient builder for incremental construction and keep the lazy tree strictly for bounded algebra (a handful of `++`/`&`/`diff`), which is its real sweet spot anyway.

### First-implementation milestone (concrete)
**M1 — Sorted leaves + lazy algebra tree for Int + Ref, proving the thesis end to end:**
1. `GenSets` (sibling of `GenCores`) emits `SBase.java` + `Empty` + `IntOne`/`RefOne` + `IntSorted`/`RefSorted` (sorted `int[]` / typed `Object[]`) + the **lazy `Union`/`Inter`/`Diff` nodes** (kind-agnostic, 2-field, O(1) construct), reusing FArray's `Repr`.
2. `opaque type FSet[A] = SBase` + companion with no-import inline extensions: `empty`, `apply`, `fromArray`, `fromFArray`, `contains` (the OR/AND/AND-NOT tree-walk), `size`/`isEmpty`, `+`/`-`/`++`/`union`/`&`/`intersect`/`diff` (the O(1) lazy nodes), `subsetOf`, `foreach`, `filter`, `map`, `iterator`, `toFArray`, `min`/`max`.
3. Build path = sort + unique-compact (no hash leaf yet). **`contains` distributes over the lazy tree, short-circuiting**; whole-set ops **materialize via sorted linear merge** and **memoize** the leaf on the node.
4. JMH suite mirroring FArray's: 4-way `immutable.HashSet` / `fastutil` / `j.u.HashSet` / `FSet`, on `contains` (both over a leaf *and* over an `a ++ b` lazy node), `build-from-array`, `union`, `intersect`, `diff`, `contains-after-union`, `foreach`, `equals`, at sizes 1…100k, with `-prof gc`. Target: tie/beat `immutable.HashSet` on contains+iteration, *beat* it on build + **O(1) lazy algebra** + `contains`-over-algebra — confirming the lazy-tree + unboxed-sorted-array thesis before adding the Hash and Dense leaves (M2/M3).

[champ-paper]: https://michael.steindorfer.name/publications/oopsla15.pdf
[lemire-intersect]: https://lemire.me/blog/2019/01/16/faster-intersections-between-sorted-arrays-with-shotgun/
[vldb-simd]: http://www.vldb.org/pvldb/vol8/p293-inoue.pdf
[fastutil-internals]: https://oshyshkov.com/2021/08/03/fastutil-openhashmap-internals-linear-probing-hashing/
[java-perf]: https://java-performance.info/large-hashmap-overview-jdk-fastutil-goldman-sachs-hppc-koloboke-trove/
[roaring]: https://roaringbitmap.org/
[roaring-paper]: https://arxiv.org/pdf/1402.6407
