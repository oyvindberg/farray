# FSet — design

A fast, immutable, **unboxed** `Set` sibling to `FArray` — but more than a set. FSet models a
first-class element of the **Boolean algebra `2^A`**: queryable (`contains`), combinable
(`∪ ∩ ∖ ⊕ ¬`, `+ -`), with **finiteness tracked in the type**, and the classical enumerable set as
the *materialized special case*. Mainstream languages give you only that special case (a finite hash
collection) and call it "Set"; FSet models the whole algebra — symbolic, predicate, infinite, lazy —
and recovers the classical set by `materialize`. Nobody ships a clean, unboxed, JVM-native one.

It keeps FArray's proven runtime verbatim: `opaque type <: AnyRef = SBase` (zero wrapper), per-element-kind
specialization (Int/Long/Double unboxed in primitive arrays + a typed `Object[]` Ref kind), `inline`
extension ops that resolve the kind via `summonFrom` on the **reused** `farray.Repr[A]` (zero virtual
dispatch, primitives never box), and the "small shared NON-inline leaf method" discipline that dodges
the measured `HugeMethodLimit` JIT cliff.

> **Status.** This document is the converged design after a long design pass. Decisions marked
> **VERIFIED** were checked by compiling minimal sketches on Scala 3.8.3 (scala-cli). Correctness is
> parity with `scala.collection.immutable.Set`; performance is the checked-in JMH scorecard.

---

## 1. The type hierarchy — real opaque subtyping (Option C, VERIFIED)

The long-held belief "opaque types can't subtype each other" is **false**. An opaque type's upper bound
need not be spent on `AnyRef`; bound it against the **parent opaque type** and you get real, externally
visible, **zero-wrapper** subtyping (you transitively inherit `AnyRef`). VERIFIED on 3.8.3: the 4-level
chain compiles, cross-scope upcasts hold with **no `Conversion`/import**, an op defined **once** on the
top is **inherited** by every subtype, and a subtype-only op is **rejected** on a supertype.

```scala
opaque type FSet[A]             <: AnyRef            = SBase   // any set: contains + algebra + .finite
opaque type FSetFinite[A]       <: FSet[A]           = SBase   // finite, maybe still lazy; materialize
opaque type FSetMaterialized[A] <: FSetFinite[A]     = SBase   // enumerated leaf: size/iterate/map/equals
opaque type FSetSorted[A]       <: FSetMaterialized[A] = SBase // ordered extras (min/max/range/…)

opaque type FSetInfinite[-A]    <: AnyRef            = SBase   // predicate-only, never enumerable
given [A]: Conversion[FSetInfinite[A], FSet[A]] = s => s       // bridge into the algebra
```

**Why real subtyping (supersedes the earlier 3-sibling + `Conversion` scheme).** Membership + algebra
are written **once** on the highest supporting type and **inherited** downward — eliminating the
"three companions' worth of duplicated ops" the older design feared. Upcasts are free. The capability
split is enforced *by the type system*: `size`/`iterator`/`map`/`equals` exist only from
`FSetMaterialized` down, so you cannot enumerate a set you have not materialized. (Use real **opaque**
subtyping — never real *trait* subtyping, which would box a wrapper and lose the inline dispatch.)

**Capability surface per level:**

| Op group | `FSetInfinite` | `FSet` | `FSetFinite` | `FSetMaterialized` | `FSetSorted` |
|---|:--:|:--:|:--:|:--:|:--:|
| `contains`/`apply`, `∪ ∩ ∖ ⊕ ¬`, `+ -` | ✅ | ✅ | ✅ | ✅ | ✅ |
| `.finite: Option[FSetFinite[A]]` | — | ✅ | (id) | (id) | (id) |
| `materialize: FSetMaterialized[A]` | ❌ | — | ✅ | (id) | (id) |
| traversals + transforms: `foreach`/`forall`/`exists`/`count`/`filter`/`map`/`flatMap` | ❌ | ❌ | ✅ | (id) | (id) |
| exact `size` / `isEmpty` (O(1)) | ❌ | ❌ | ❌ | ✅ | ✅ |
| `iterator`/`equals`/`hashCode`/`toFArray` | ❌ | ❌ | ❌ | ✅ | ✅ |
| `min`/`max`/`range`/`firstKey`/`lastKey`/ordered `iterator` | ❌ | ❌ | ❌ | ❌ | ✅ |

> **Placement note (implemented).** The *building* transforms and the O(n) traversals moved DOWN to
> `FSetFinite`: they materialize (and memoize) internally, so requiring an explicit `.materialize`
> first added a keyword but no information. The O(1) observations (`size`/`isEmpty`) and the ops whose
> impls *require* a frozen leaf (`iterator`, value `equals`/`hashCode`, `min`/`max`) stay on
> `FSetMaterialized`, keeping "cheap read" visible in the type.

### 1.1 Variance — invariant, except `FSetInfinite` is contravariant (VERIFIED)

Variance follows which **face** of `A` the type exposes. A type that produces `A` (iteration) *and*
consumes it (`contains`) is **invariant** — that's `FSet`/`FSetFinite`/`FSetMaterialized`/`FSetSorted`
(and `FSetSorted` doubly so, carrying `Ordering[A]`). A type that only *consumes* `A` is a predicate
`A => Boolean` and is **contravariant** — that's `FSetInfinite`.

Real subtyping forces variance coherence: `FSetInfinite[-A] <: FSet[A]` is **rejected** ("contravariant
type parameter A occurs in invariant position"). So `FSetInfinite` is kept **out of the subtype chain**
— a separate contravariant opaque type bridged to `FSet` by a `Conversion`. VERIFIED to give all three:
`FSetInfinite[Animal]` usable as `FSetInfinite[Cat]` (contravariance), `FSetInfinite` passable where
`FSet` is expected (conversion), and `.finite` on the invariant `FSet`. Cost: `FSetInfinite` defines its
own (membership-only) `contains`/algebra and widens by conversion, not free upcast — a small, accepted
duplication confined to the predicate surface.

> **Phantom-`A` caveat.** Since `FSet[A] = SBase` and `SBase` does not mention `A`, the variance
> annotations are *not* representation-checked — extension methods that leak an `A` are a manual
> soundness hazard (as FArray's covariant trap). Invariant-everywhere (the chain) has no such hazard;
> `FSetInfinite[-A]` must never expose an `A`-producing extension.

### 1.2 Finiteness is tracked statically, never decided

Finiteness is a **syntactic** property: which combinator built the set. Finite combinators
(`fromArray`, `∪/∩/∖/filter` of finite) return `FSetFinite`; infinite combinators (`above`/`below`/
`universal`/`complement`/`λ`) return `FSetInfinite`; the tag propagates through the algebra's return
types. This is a **sound static over-approximation** (an abstract interpretation in `{finite, infinite}`
with the combinators as transfer functions) — it sidesteps Rice's theorem entirely because we never
*decide* finiteness of an arbitrary set, only *propagate* it. Per-operation rules (the result's
finiteness):

| op | result is finite iff | notes |
|---|---|---|
| `a ∪ b` | **both** finite | infinite if either is |
| `a ∩ b` | **either** finite | `inf ∩ finite = finite` ALWAYS — *the gem* (give it a static overload) |
| `a ∖ b` | **left** finite | removing anything from a finite set stays finite |
| `a ⊕ b` | both finite | but `inf ⊕ inf` = unknown (unlike `∪`) |
| `¬a` | never (from finite) | `complement(finite) = infinite`; `complement(inf)` = unknown |
| `a + x` / `a - x` | preserves `a`'s class | `Materialized + x` demotes to `Finite` (lazy node) |

Statically-unknown cases (`inf ∩ inf`, an `FSet` from elsewhere) fall to `FSet`, narrowable at runtime
by **`.finite: Option[FSetFinite[A]]`** — implemented by structural inspection of the `SBase` tree,
**conservatively** (never a false `Some`; a false `None` merely fails to realize an actually-finite
set). Enumeration-dependent ops (`size`/`iterate`/`map`/`equals`/`subsetOf`) live on `FSetMaterialized`+
only — so the undecidable region is unreachable through the API, and everything you *can* compare is
trivially decidable (both sides enumerated).

---

## 2. Representation

### 2.1 The sealed Java core (`SBase`)

```
SBase (sealed, abstract; int sizeHint())
 ├── SView          // lazy algebra — backs FSet/FSetInfinite when deferred
 │     SUnion · SInter · SDiff · SXor   { final SBase left, right; @volatile SBase memo; }
 │     (+ later: SAbove · SBelow · SUniversal · SComplement · SStride · SPredicate — infinite leaves)
 └── SMaterialized  // deduplicated, enumerable — abstract int size(); final sizeHint()=size()
       SEmpty · S{Int,Long,Double,Ref}One        // size 0 / 1, no array
       S{Int,Long,Double,Ref}Sorted              // packed sorted FArray — the default leaf
       S{Int,Long,Double,Ref}Hash                // frozen open-addressing index over a packed FArray
       (+ later: SIntRange · SIntRuns · IntDense)
```

**CORRECTION to the M1 scaffold:** `SEmpty` and `S{K}One` extend `SMaterialized` (not `SView`) — they
are trivially deduped/enumerable (O(1) `size`/`min`/`max`), so `FSet(x)`/`empty` answer `size`/iterate
with no materialize step. `SView` is *purely* the deferred combinators (+ infinite predicate leaves).
No node tag — plain sealed `instanceof` dispatch (measured: HotSpot folds it; an int-tag `tableswitch`
is 17–20% slower for monomorphic reads).

### 2.2 The store — one "map-like" thing, single column

A materialized leaf is **a packed `FArray` of the elements (`elems`) + an *optional* hash index**:

- **Small set** → packed sorted `elems` only; `contains` = branchless binary search (L1-resident,
  register-bound — ties O(1) without allocating a table).
- **Large set / Ref** → `elems` + an **`index`** (a sparse array of **positions** into `elems`,
  `-1` = empty); `contains` = `mix(hash) & (cap-1)` → probe `index` → `elems(pos)` compare. One hop.

The elements are *always* the packed `FArray` (reuse: iteration/equals/`toFArray`/sorted come from
FArray). The over-allocation lives in the cheap `index` (narrow `byte[]`/`short[]`/`int[]`), not in the
payload. **One column = a set.** Two parallel columns would be a map — **`FMap` is a separate later
track; do not build 2-column machinery now.**

### 2.3 The frozen hash leaf (`S{K}Hash`)

Because it's built once and frozen, we beat what a mutable table can do:

- **Primitives:** one `int[]/long[]/double[]` of values, power-of-two cap, **linear probing**,
  empty = a sentinel value proven absent at build (try `0/MIN/MAX`, else gap-scan the sorted array),
  probed **empty-first** (drops fastutil's per-call `if(k==0)` branch). Index by high-bit
  multiplicative mix `(key*seed) >>> shift`. **Build-time K=8 seed search** minimizes max displacement
  → ~1-probe `contains`. Load ~0.5–0.66 (no growth headroom needed). `contains` allocates **zero**.
  *(For Long/Double/Ref the compact position-index-over-`FArray` form §2.2 wins memory; the bare-value
  table is the Int fast path — start with one layout, split out Int only if the scorecard demands.)*
- **Ref:** `Object[] keys` (empty = `null`) + a **parallel `int[] hashes`** (cached at build). Probe
  compares the cached `int` before any `.equals`/deref — the miss path never touches the object. This
  is the one place `contains` genuinely **beats** fastutil/Koloboke (they re-`equals` on collision).
- **No Swiss/SWAR tag arrays in v1** — they lose on this Apple-Silicon/NEON box (no `PMOVMSKB`, extra
  cache line) for primitives; the `int[]` hash-cache is a stronger 32-bit filter for Ref.

### 2.4 The lazy algebra tree — `contains` distributes

`contains` is a Boolean homomorphism: `contains(x, ·)` sends `∪↦∨`, `∩↦∧`, `∖↦∧¬`, `⊕↦⊕`, `¬↦¬`,
short-circuiting. So `∪/∩/∖/⊕` are **O(1) lazy nodes** sharing their operands, and membership over an
algebra never materializes. `+x = Union(this, One(x))`, `-x = Diff(this, One(x))` — both O(1).
**No JVM set has lazy set algebra.** The tree walk is an **explicit lazily-allocated `SBase[]` worklist**
(NEVER JVM recursion — a deep `+` chain would StackOverflow); the smart constructors apply Boolean
axioms as peephole rewrites (`a∪a=a`, `a∖a=∅`, absorption, `∅/U` identities) to keep trees shallow.

### 2.5 Ranges and predicate/infinite leaves (later phases)

- **`SIntRange(lo,hi)` / `SLongRange`** (v2): contiguous, ~24 bytes for a billion-element set,
  `contains` = `lo<=x<=hi`, O(1). The algebra **closes**: `∩→0/1`, `∪→1/2`, `∖→0/1/2`, `⊕→0/1/2`
  ranges, all O(1) at construct time (fringe arithmetic in `long` to dodge overflow; `card` is `long`,
  full Int domain = 2³²). It is `SMaterialized` (enumerable-by-computation, like FArray's `RangeNode`).
  Catches: set `hashCode` is `unorderedHash` (no closed form → O(card) once, memoized; `equals` stays
  O(1) via `card`+`min`+`max`+contiguity); a **range-aware materializer is a *safety* requirement** (a
  generic merge OOMs on `Union(Range(0,1e9), …)`).
- **`SIntRuns`** (v3): sorted disjoint `[lo,hi]` runs = Roaring's run-container = DIET; `contains`
  O(log R), bulk ops = run-merges. **`IntDense`** (v3): bitmap, only for dense-*irregular* Int. The
  Roaring trinity maps onto our leaves: array-container ≈ `Sorted`, run-container ≈ `Range`/`Runs`,
  bitmap ≈ `IntDense`, with the lazy algebra as the cross-container glue.
- **Infinite predicate leaves** (`SAbove`/`SBelow`/`SUniversal`/`SComplement`/`SStride`/`SPredicate`,
  v2–v3): answer `contains` by computation, extend `SView`, back `FSetInfinite`. The **structured** ones
  (intervals/strides) are a *decidable* fragment = the **1-D Presburger / eventually-periodic sets**;
  the opaque `λ` is undecidable (Rice) and quarantined (megamorphic `contains` breaks the unboxed
  thesis) — `λ` last. `Stride` membership uses `Math.floorMod`/a power-of-two mask, never raw `%`.

---

## 3. Performance — how we crush

`contains` **ties** the specialized mutable primitive hash sets (fastutil/Koloboke) — same probe shape,
both unboxed; we **beat** them only on **Ref** (the `int[]` hash-cache) and miss-heavy moderate-n; and
we **crush** boxed CHAMP/`j.u.HashSet` always (no boxing, no node hop). The "out-hash fastutil on
primitive contains" headline is *not* airtight and erodes at huge n — **do not ship that claim.**

The real crush is **structural** — being a set is more than point-`contains`:

| we crush on | because | competitor can't follow without |
|---|---|---|
| build-from-array | unboxed sort+unique / freeze-without-copy | boxing (CHAMP/`j.u.HashSet`) |
| `∪ ∩ ∖` construction | **O(1) lazy nodes**, `contains` distributes | becoming us — no JVM set has lazy algebra |
| bulk `∪ ∩ ∖` materialized | one cache-sequential unboxed **sorted-merge**, sorted result | abandoning hash-order storage |
| ranges | 16 bytes for 10⁹ elements, O(1) | — |
| ordered iteration / `min`/`range` | flat array scan / O(1) | impossible on a hash set |
| value `equals`/`hashCode` | one unboxed commutative pass | CHAMP's documented weak spot |
| memory footprint | flat primitive arrays | 4–15× smaller than boxed trees |

Honest losses — all *outside* the build-once/query-and-bulk-many sweet spot, each with an in-family
escape hatch: **CHAMP** on persistent single-element churn with version retention (→ transient builder /
`S{K}Buffered` coalescing node); **RoaringBitmap** on dense-*irregular* Int bulk (→ `IntDense`/bridge);
**fastutil** in a hot mutate-and-query loop (→ transient builder). Every "win" ships only when the
4-way JMH scorecard (`immutable.HashSet`/fastutil/`j.u.HashSet`/FSet, + Koloboke/Roaring/TreeSet) is
green, gated on the deterministic `gc.alloc.rate.norm` B/op tell.

### Claims corrected vs the earlier draft
- ✗ "SIMD-able merge" — C2 won't vectorize a two-cursor data-dependent loop, and this box is NEON
  (no `VPCOMPRESS`). The real win is **galloping + branchless-scalar**.
- ✗ "binary search ties hash at large n" — it **loses** 2–10×; the hash leaf (v1, not Phase-2) closes it.
- ✗ "win on refs at `contains`" — **tie at best** without the hash-cache; we inherit FArray's measured
  0.30× ref-at-scale slowdown.
- ✗ `RefRepr.ct` does not exist — `RefArr` stores a bare `Object[]`; `ClassTag` is supplied only at
  `toArray` egress.

---

## 4. Mathematical foundation

FSet represents the **Boolean algebra** `(2^A, ∪, ∩, ¬, ∅, U)`; `contains`-at-a-point is the evaluating
homomorphism `2^A → 2` (Stone-duality flavor). Consequences that shape the design:

1. **`isEmpty` is the single decision primitive:** `a=b ⟺ a⊕b=∅`, `a⊆b ⟺ a∖b=∅`,
   `disjoint ⟺ a∩b=∅`. Decidable for the structured tier (intervals/strides = 1-D Presburger),
   undecidable for opaque `λ` (Rice) — which is why those ops live on `FSetMaterialized` only.
2. **The universe is finite for fixed-width kinds.** `Int` has 2³² values, so *every* `Int` set is
   finite and `|¬S| = 2³² − |S|` is computable — "infinite/unmaterializable" is a **resource** threshold
   (don't enumerate 2³¹), not literal infinity. Only `Ref`/unbounded domains are genuinely infinite. So
   the finiteness tag means "materializable-as-elements," which is the operationally-correct meaning.
3. **`map` (image) is NOT a homomorphism** (breaks `∩`/`¬`) → materialize-only, on `FSetMaterialized`.
   **`comap` (preimage) IS** a homomorphism → the natural op on predicate sets; and **`filter = ∩ predicate`**.
4. **Structured normal form** = sorted disjoint intervals + a periodic part (eventually-periodic) →
   keeps the optional decidable ops cheap. Single arithmetic progressions are *not* closed under `∪` —
   use the eventually-periodic form or stay lazy.

Prior art borrowed: Erwig's **DIET** (JFP 1998), Guava **`RangeSet`**, D'Antoni/Veanes **effective
Boolean algebra** (symbolic automata), the finite-cofinite algebra, Scala's own `Set <: A => Boolean`.

---

## 5. Specialization + runtime (FArray patterns, adopt verbatim)

- **Reuse `farray.Repr[A]`** (Int/Long/Double prim arrays + Ref `Object[]`); emit into `package farray`
  so there's no second `Repr`. Kind dispatch = `summonFrom { case r: ${K}Repr[A] => … }`, resolved at
  the concrete inline call site → one static path, zero runtime type test, primitives unboxed.
- **Specialize-or-fail:** abstract `A` ⇒ no `Repr` ⇒ compile error, never a silent boxed fallback.
- **Every lambda-taking op is `inline`** with an `inline` function param; the user's lambda
  beta-reduces into the primitive loop (`foreach`/`filter`/`map`/…).
- **Shared NON-inline leaf method discipline (§ the JIT cliff):** the inline surface does only kind
  dispatch + element unwrap + SAM realization, then calls **one small shared non-inline** per-kind
  method in `object FSetOps` (`containsLeaf{K}`, `mergeLeaf{K}`, `build{K}`, `hashAcc{K}`). Never inline
  the leaf loop per call site — FArray measured the `HugeMethodLimit` overflow at **0.29×**. Leaf loops
  bound on `a.length` (not a size field) so C2 drops bounds checks.
- **Node classes are `S`-prefixed** (`SIntOne`/`SUnion`/…) to avoid colliding with FArray's same-package
  `IntOne`/etc. **`S`-prefix all new core nodes.**
- **`@volatile SBase memo`** on the lazy nodes (benign race; value-equal leaves ⇒ publishing either is
  correct; the volatile store gives safe publication of the leaf's array fields).
- The generator is `gensets/src/scala/farray/GenSets.scala` (a `BleepCodegenScript`, sibling of
  `GenCores`). **Edit the generator, never the generated sources.** Adding a primitive = one `prims` row.

---

## 6. Build phasing

**v1 / M1 — prove the unboxed-sorted-merge + lazy-algebra thesis on the minimal store:**
1. **Three correctness fixes (first — they block any real workload):** iterative `SBase[]`-worklist
   `containsLeaf{K}` (the recursive walk StackOverflows on a deep `+`/`++` chain); Double dedup via
   `Double.compare`/`doubleToLongBits` (raw `!=`/`==` keeps duplicate `NaN`, collapses `±0.0`, never
   matches `NaN`); the `@volatile` memo slot (+ add `SXor`).
3. **Core hierarchy + Option C opaque types:** `SBase → SView | SMaterialized`; `SEmpty`/`S{K}One`
   under `SMaterialized`; the 5-level opaque lattice (`FSet`/`FSetFinite`/`FSetMaterialized`/`FSetSorted`
   + `FSetInfinite` via Conversion).
4. **The merge/materialize core (load-bearing):** unboxed sorted×sorted branchless merge + galloping,
   emits a sorted leaf, memoizes on the node; route `size`/`iterate`/`equals` through it.
5. **Frozen `S{K}Hash`** (Int/Long/Double, then Ref with the `int[]` hash-cache), gated by a swept
   `PRIM_HASH_THRESHOLD`. Bulk `∪/∩/∖/⊕` surface ops. Value `equals`/`hashCode`.
6. **Three foreclose-nothing predicate hooks:** `containsLeaf` treats "leaf" as an open per-node
   membership arm (not a closed match); route `size`/`iterate`/`materialize` through one chokepoint that
   consults `finite()` and can refuse; keep `SView`'s `permits` open to a non-enumerable leaf.
7. **The 4-way JMH scorecard**, checked in, gated on B/op.

**v2:** `SIntRange`/`SLongRange` + the closed range algebra + the safety materializer; the structured
`FSetInfinite` tier (`Above`/`Below`/`Universal`/`Complement`/`Stride`) + the decidable rewrite table;
the contravariant `FSetInfinite`.

**v3:** `SIntRuns` + `IntDense` (the Roaring trinity, one density heuristic); `S{K}Buffered` coalescing
node + the transient `FSetBuilder`; the opaque `λ` leaf last; `FSortedSet` range ops; possibly `FMap`.

---

## 7. Open knobs + verified facts

**VERIFIED (Scala 3.8.3):** 4-level opaque subtyping via parent bounds; free cross-scope upcasts;
`contains` inherited once; `size` rejected on `FSet`; `FSetInfinite[-A] <: FSet[A]` rejected (→ the
Conversion hybrid); the hybrid (invariant chain + contravariant `FSetInfinite` + `.finite`) works.

**Open (benchmark/measure):** `PRIM_HASH_THRESHOLD` (~16–32, per kind); hash load factor (0.5–0.7) +
the huge-n cache-residency crossover; seed-search depth; `cmov`/`csel` emission for branchless binary
search on ARM (PrintAssembly-verify, else the Sorted→Hash threshold drops); the lazy-tree depth/coalesce
threshold; whether to split the bare-value Int hash table out from the compact form.

[champ-paper]: https://michael.steindorfer.name/publications/oopsla15.pdf
[diet]: https://web.engr.oregonstate.edu/~erwig/papers/Diet_JFP98.pdf
[guava-rangeset]: https://github.com/google/guava/wiki/NewCollectionTypesExplained#rangeset
[sfa-eba]: https://pat.keldysh.ru/~roman/doc/Veanes_2010-Symbolic_Automata.pdf
[roaring]: https://roaringbitmap.org/
[lemire-intersect]: https://lemire.me/blog/2019/01/16/faster-intersections-between-sorted-arrays-with-shotgun/
