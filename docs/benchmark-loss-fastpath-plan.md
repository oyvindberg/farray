# FArray benchmark-loss audit + fast-path fix plan

READ-ONLY audit of `docs/bench-results.json`. A "loss" is `(op, size)` where farray's
throughput is below the **best non-farray competitor** by >5% (`best_other / farray > 1.05`).
The FArray-family variants `farrayMat` / `farrayTree` (used only in `IntMixedTreeBenchmark`)
are NOT counted as competitors; `ziochunkMat` / `ziochunkTree` ARE (they are real zio variants).

All line citations are to the generator `codegen/src/scala/farray/GenCores.scala` (the
emitting template lines 1577-1623 declare these `…Impl` defs) and the generated
`.bleep/generated-sources/farray/farray.GenCores/farray/FArrayOps.scala` (the bodies), plus the
hand-written surface `farray/src/scala/farray/FArray.scala`.

## Headline numbers

- **Total losses: 127** out of 560 measured `(op,size)` cells.
- **Size 0/1 losses: 48** — the focus of this plan; the large majority are fast-path-fixable.
- Of those 48, **~24 are a genuine missing Empty/{K}One peel** (the impl runs `summonFrom` +
  `materialize…` / DFS-consumer allocation BEFORE any length short-circuit), and the rest are the
  known `intAt` size-1 read floor or sub-1.1× measurement noise (documented per-op below).

## Category breakdown (all 127)

| count | category | meaning |
|---|---|---|
| 48 | fastpath-0/1 | size 0/1 — analyzed individually below |
| 15 | algo/hash-materialize | groupBy / toSet / distinct build a JDK Map/Set; competitor wins on hashing, not on us |
| 11 | algo/sort-vs-Arrays.sort | sort eagerly `materialize…`s + key-map + index sort; IArray's `Arrays.sort` on the raw array is faster |
| 7 | algo/reduce-engine | reduce* small-size: drop(1)+fold engine vs IArray's tight loop |
| 7 | algo/fold-engine-small | sum/product/min/max small-size: fold-engine entry vs IArray loop |
| 7 | algo/scan-loop-shape | scan/scanLeft/scanRight per-element loop shape vs IArray |
| 6 | algo/materialize-then-copy | toArray, esp. Ref (String) — second copy / ClassTag mismatch |
| 6 | algo/slice-search | indexOfSlice / lastIndexOfSlice / containsSlice / sameElements — `matchAll2` re-materializes per shift |
| 5 | construction(allocates>raw) | create02..32 — FArray allocates a node wrapper over the raw array (accepted per project goals) |
| 4 | algo/toVector-detour | combinations / permutations route through `toVector` |
| 4 | small-misc | mkString / copyToArray near-1.1× |
| 4 | noise/near-tie-or-loopshape | exists / indexWhere / foldLeft ≤1.13× |
| 1 | noise/length | `ListLikeStringBenchmark.len` 2.98× — almost certainly a measurement / inlining artifact (length is O(1)); re-measure with -f2 |
| 1 | deep-base-applyBoxed(redesign) | `IntDeepConcatBenchmark.map` 1.08× — owned by the in-flight interpreter redesign |
| 1 | algo/partition | partition@10000 2.21× |

---

## PART 1 — Size 0/1 losses (the fast-path target)

### Reference pattern (how the already-fast ops peel)

The peeling idiom is "shape-first, dispatch-second": short-circuit on `length` **before** the
`summonFrom` kind dispatch and before any `materialize…` / DFS-consumer allocation. Ops that
already do this:

- `zipWithIndexImpl` (FArrayOps.scala:2417, template GenCores:1607):
  `{ val n = xs.length; if (n == 0) Empty.INSTANCE else if (n == 1) fromValues1[(A,Int)]((applyAtImpl(xs,0),0)) else { summonFrom {…} } }`
- `zipImpl` (2391, GenCores:1606), `unzipImpl` (2259, GenCores:1604), `unzip3Impl` (GenCores:1605) — all `if (n == 0) …` first.
- `mkStringImpl` (1841, GenCores:1577) — `if (xs.length == 0) start + end else summonFrom {…}` (peels 0 only, not 1; see below).
- `updatedImpl` (2481, GenCores:1611) `if (xs.length == 1 && index == 0) fromValues1 …`, `appendImpl` (2487), `prependImpl` (2493), `mapConserveImpl` (2253) — all length-first.
- `map`'s flat leaf path collapses `_l==0 -> Empty.INSTANCE`, `_l==1 -> new ${K}One` at the tail (FArrayOps:2074-2095) — the post-build shape canonicalization.

The fix everywhere below: hoist the same `if (n==0) <trivial> else if (n==1) <single> else <current body>` to the **front** of the impl, ahead of `summonFrom`/`materialize…`.

### Size-0/1 loss table

| op (class) | size | ratio | winner | already peels? | fast-path verdict |
|---|---|---|---|---|---|
| intersect (IntSetOps) | 0 | 5.57× | list | NO | ADD Empty arm |
| partition (IntSlicing) | 0 | 5.10× | list | NO | ADD Empty arm |
| diff (IntSetOps) | 0 | 4.73× | list | NO | ADD Empty arm |
| sameElements (IntSearchSlice) | 0 | 3.35× | iarray | partial | length==0 guarded in surface but `that.length` read + call; near-noise, see note |
| toSet (IntSetOps) | 0 | 2.81× | iarray | NO | ADD Empty arm |
| groupBy (IntSetOps) | 0 | 2.51× | list | NO | ADD Empty arm |
| sortBy (IntSort) | 0 | 2.29× | vector | NO (materialize-first) | ADD Empty arm |
| sortBy (StrSort) | 0 | 2.26× | vector | NO | ADD Empty arm |
| reverse (IntSortAdaptive) | 0 | 2.25× | vector | n/a (structural) | likely noise — `reverse` is an O(1) node; re-measure |
| lastIndexWhere (IntSearchSlice) | 0 | 2.20× | ziochunk | NO (summonFrom-first) | ADD Empty arm |
| groupBy (StrNewOps) | 0 | 2.18× | ziochunk | NO | ADD Empty arm |
| distinct (IntSlicing) | 0 | 2.15× | vector | NO | ADD Empty arm |
| sorted (IntSort) | 0 | 2.13× | vector | NO (materialize-first) | ADD Empty arm |
| sortWith (StrSort) | 0 | 2.08× | vector | NO | ADD Empty arm |
| sorted (IntSortAdaptive) | 0 | 2.07× | vector | NO | ADD Empty arm |
| toSet (StrNewOps) | 0 | 2.01× | ziochunk | NO | ADD Empty arm |
| sorted (StrSort) | 0 | 1.99× | vector | NO | ADD Empty arm |
| distinct (StrNewOps) | 0 | 1.88× | list | NO | ADD Empty arm |
| lastIndexOf (IntSearchSlice) | 0 | 1.87× | iarray | NO (summonFrom-first) | ADD Empty arm |
| sortWith (IntSort) | 0 | 1.86× | vector | NO | ADD Empty arm |
| sortBy (IntSortAdaptive) | 0 | 1.85× | vector | NO | ADD Empty arm |
| toArray (StrNewOps) | 0 | 1.35× | list | NO (summonFrom-first) | ADD Empty arm |
| update (IntUpdateChain) | 0 | 1.21× | ziochunk | n/a (index!=0 path) | low ratio; update@0 is `new IntUpdated`; noise/structural |
| grouped (IntSlicing) | 0 | 1.17× | iarray | n/a (Iterator) | noise — Iterator allocation |
| copyToArray (IntConvert) | 0 | 1.16× | vector | partial (`n<=0 -> 0`) | already short-circuits; sub-1.2× noise |
| zipWithIndex (IntZip) | 0 | 1.14× | ziochunk | YES (peels) | already peels Empty; noise |
| sum (IntAggregate) | 0 | 1.10× | iarray | partial | fold engine over Empty; ADD Empty arm (low ratio) |
| product (IntAggregate) | 0 | 1.08× | iarray | partial | as sum; ADD Empty arm (low ratio) |
| reduceLeftOption (IntReduce) | 0 | 1.06× | iarray | YES (`length==0 -> None`) | already trivial; noise |
| reduceOption (IntReduce) | 0 | 1.06× | iarray | YES (`length==0 -> None`) | already trivial; noise |
| distinct (StrNewOps) | 1 | 13.97× | ziochunk | NO | ADD {K}One arm |
| distinct (IntSlicing) | 1 | 12.26× | ziochunk | NO | ADD {K}One arm |
| sameElements (IntSearchSlice) | 1 | 5.14× | iarray | NO | ADD len==1 element-compare fast-path |
| containsSlice (IntSearchSlice) | 1 | 4.30× | iarray | NO | ADD len==1 fast-path (via indexOfSlice) |
| indexOfSlice (IntSearchSlice) | 1 | 3.77× | iarray | NO | ADD len==1 fast-path |
| lastIndexOfSlice (IntSearchSlice) | 1 | 3.67× | iarray | NO | ADD len==1 fast-path |
| toArray (IntConvert) | 1 | 2.61× | fs2chunk | NO ({K}One not peeled) | ADD {K}One arm |
| toArray (StrNewOps) | 1 | 2.20× | fs2chunk | NO | ADD {K}One arm |
| scanRight (IntScan) | 1 | 1.62× | iarray | NO | ADD {K}One/{K}Arr arm |
| mkString (StrNewOps) | 1 | 1.61× | list | partial (peels 0 not 1) | ADD len==1 arm (return single elem string) |
| scan (IntScan) | 1 | 1.34× | iarray | NO | ADD {K}One arm |
| scanLeft (IntScan) | 1 | 1.32× | iarray | NO | ADD {K}One arm |
| reduceLeft (IntReduce) | 1 | 1.25× | iarray | YES (`length==1 -> applyAtImpl`) | `intAt` read floor (see note), not a missing peel |
| reduce (StrNewOps) | 1 | 1.21× | iarray | YES | `intAt`/`refAt` read floor |
| reduce (IntReduce) | 1 | 1.17× | iarray | YES | read floor |
| reduceRight (IntReduce) | 1 | 1.17× | iarray | YES | read floor |
| copyToArray (IntConvert) | 1 | 1.07× | fs2chunk | YES (`n==1 -> dest(start)=…`) | already peels; noise |
| reduceOption (IntReduce) | 1 | 1.05× | iarray | YES | read floor / noise |

---

## PART 2 — Per-op fast-path prescriptions

For each op below, the proposed arms go at the **front of the impl body in `GenCores.scala`**
(the listed template line), mirroring `zipWithIndexImpl`/`zipImpl`. `{K}One` / `{K}Arr` are the
per-kind leaf nodes; `Empty.INSTANCE` is the empty node.

### sorted / sortBy / sortWith — GenCores:1615-1617 (FArrayOps:2505-2521)

These are the most impactful 0-losses (1.85×-2.29×, 8 cells across Int/Str × 3 ops, plus size-10
1.22×-4.89×). The bodies do `val vals = materializeInt(xs); val n = vals.length; if (n < 2) xs`
— **materialize runs first**, allocating an `Array` (even length 0) and an anonymous `${K}Dfs`
consumer, then discarding them. Peel BEFORE `summonFrom`:

```
inline def sortedImpl[A, B >: A](xs: FBase)(using ord: Ordering[B]): FBase =
  if (xs.length < 2) xs else summonFrom { … current per-kind body … }
inline def sortByImpl[A, B](xs: FBase)(inline f: A => B)(using ord) : FBase =
  if (xs.length < 2) xs else summonFrom { … }
inline def sortWithImpl[A](xs: FBase)(inline lt) : FBase =
  if (xs.length < 2) xs else summonFrom { … }
```

Expected: size-0/1 sort drops from `materialize + DFS-object alloc + summonFrom` to a single
`length` compare returning `xs` — should reach IArray/vector parity (`Arrays.sort` also returns
early but pays no wrapper). This also fixes the size-10 `sortBy` 4.89× indirectly only if the
mid-size algorithm is separately improved (see Part 3); the front-peel itself is 0/1-only.
(`sortByImpl` additionally calls `mapImpl` for keys — keep that inside the `else`.)

### distinct / distinctBy — surface FArray.scala:204-207 (no Impl; inline on surface)

`distinct` is `val seen = HashSet.empty[Any]; xs.filter(a => seen.add(a))`. At size 0 this still
allocates a `HashSet` and enters `filterImpl`; at size 1 the 12-14× loss is the HashSet alloc
dominating a one-element result. Peel on the surface:

```
inline def distinct: FArray[A] =
  if xs.length <= 1 then xs
  else { val seen = scala.collection.mutable.HashSet.empty[Any]; xs.filter(a => seen.add(a)) }
inline def distinctBy[B](inline f: A => B): FArray[A] =
  if xs.length <= 1 then xs else { … current … }
```

Expected: 0/1 distinct goes from HashSet-alloc + filter traversal to an `xs` return; kills the
13.97× / 12.26× size-1 losses outright (these are the single worst 0/1 cells).

### toSet — surface FArray.scala:234

`{ val b = Set.newBuilder[B]; xs.foreach(a => b += a); b.result() }`. At size 0 the builder +
`result()` still costs; competitors return `Set.empty` directly. Peel:

```
inline def toSet[B >: A]: Set[B] =
  if xs.length == 0 then Set.empty[B]
  else { val b = Set.newBuilder[B]; xs.foreach(a => b += a); b.result() }
```

(Size-1 toSet is not in the loss list, so a `Set(elem)` arm is optional.)

### groupBy — GenCores:1622 (FArrayOps:2587)

Body allocates a `java.util.HashMap`, a `${K}Dfs` consumer, runs `dfsC…`, then a `Map.newBuilder`
— all before observing length. Peel at the front of `groupByImpl`:

```
inline def groupByImpl[A, K](xs: FBase)(inline f: A => K): Map[K, FBase] =
  if (xs.length == 0) Map.empty[K, FBase] else summonFrom { … current per-kind body … }
```

Expected: empty groupBy returns `Map.empty` (a shared singleton), eliminating HashMap + DFS-object
+ builder allocs. Fixes IntSetOps@0 (2.51×) and StrNewOps@0 (2.18×). (Note `groupBy` surface
FArray.scala:216 just casts the result, so the peel belongs in the Impl.)

### diff / intersect — surface FArray.scala:247-254

Both build a `HashMap` from `that` then `xs.filter`. At size 0 (the benchmark makes BOTH operands
empty) this still allocates the HashMap and iterates `that.foreach`. Peel on `xs` (and optionally
on `that`):

```
inline def diff[B >: A](that: FArray[B]): FArray[A] =
  if xs.length == 0 then xs
  else { val rem = …; that.foreach(…); xs.filter { … } }
inline def intersect[B >: A](that: FArray[B]): FArray[A] =
  if xs.length == 0 then xs
  else if that.length == 0 then FArray.empty[A]
  else { … current … }
```

Expected: empty diff/intersect return immediately (these are the 4.73× / 5.57× cells — the worst
size-0 losses).

### partition — GenCores:1599 (FArrayOps:2109)

`partitionImpl` dispatches straight into `partitionLeaf${K}` with no length guard. At size 0 the
leaf partitioner still allocates two output buffers / SAM. Peel at the front:

```
inline def partitionImpl[A](xs: FBase)(inline p): scala.Tuple2[FBase, FBase] =
  if (xs.length == 0) new scala.Tuple2(Empty.INSTANCE, Empty.INSTANCE)
  else summonFrom { … current … }
```

Expected: empty partition returns the shared `(Empty, Empty)` pair (5.10× cell). The surface
(FArray.scala:201) only casts, so the peel goes in the Impl. (Size-10000 partition 2.21× is a
separate mid-size algorithm issue — Part 3.)

### lastIndexWhere / lastIndexOf — GenCores:1592-1593 (FArrayOps:2047-2057)

Both are `{ val length = xs.length; summonFrom { … scBwdLeaf${K}(xs, end-clamp, pred) } }`. At
size 0, `length == 0`, `end` clamps to `-1`, and `scBwdLeaf` is entered with a dead range — still
a `summonFrom` + backward-traverser entry. Peel:

```
inline def lastIndexWhereImpl[A](xs: FBase, end: Int)(inline p): Int =
  { val length = xs.length; if (length == 0) -1 else summonFrom { … } }
inline def lastIndexOfImpl[A, B](xs: FBase, elem: B, end: Int): Int =
  { val length = xs.length; if (length == 0) -1 else summonFrom { … } }
```

Expected: empty returns `-1` with no dispatch (2.20× / 1.87× cells). `indexWhere`/`indexOf`
(`indexWhereImpl` GenCores:1589, `indexOfImpl` GenCores:1590) are NOT in the loss list but should
get the symmetric `if (length == 0) -1` peel for consistency.

### sum / product — GenCores:1618-1619 (FArrayOps:2523-2533)

Bodies are `reduceLeafFwd${K}${K}(xs, zero, op)`. The fold engine over an Empty node returns `zero`
but pays the engine entry; iarray returns `0` from an empty loop. Low ratio (1.10× / 1.08×). Peel:

```
inline def sumImpl[A, B](xs: FBase)(using num): B =
  if (xs.length == 0) num.zero else summonFrom { … }
inline def productImpl[A, B](xs: FBase)(using num): B =
  if (xs.length == 0) num.one else summonFrom { … }
```

(For the unboxed prim arms the literal `0`/`1` is `num.zero`/`num.one`; keep the cast pattern.)
Expected: marginal — these are near-noise but trivially correct to peel.

### toArray — GenCores:1578 (FArrayOps:1863)

`toArrayImpl` enters `summonFrom` then, per kind, either `materialize…` (when ClassTag matches the
flat kind) or a DFS copy. At size 0 it allocates a zero-length array via `materialize…`/`ct.newArray`
+ DFS object; at size 1 (`{K}One`, no leaf array) the Ref/prim non-matching path runs `materialize…`
then `arraycopy`. Peel:

```
inline def toArrayImpl[A, B](xs: FBase)(ct): Array[B] =
  if (xs.length == 0) ct.newArray(0)
  else if (xs.length == 1) { val out = ct.newArray(1); out(0) = applyAtImpl[A](xs, 0).asInstanceOf[B]; out }
  else summonFrom { … current … }
```

Expected: fixes size-0 (1.35×) and size-1 (2.61× / 2.20×). NOTE the large mid/size `toArray`
losses (Str@1000 3.12×, @10000 5.13×, @100000 8.51×) are a SEPARATE algorithm problem (Part 3,
materialize-then-copy + ClassTag), not a 0/1 fast-path.

### scan / scanLeft / scanRight — GenCores:1620-1621 (FArrayOps:2535-2585), surface FArray.scala:335-337

`scanLeftImpl`/`scanRightImpl` build an `out` of length `n+1`. At size 1 the result is a 2-element
leaf, but entry pays `summonFrom` + (for cross-kind acc) a per-element `applyAtImpl` loop. The
size-0 case is NOT in the loss list (scan@0 already ties). For size-1, add a `{K}One`/2-element
direct arm before the general per-element loop. Minimal version — peel n==0 and let the
specialized `scanLeftLeaf${K}${K}` self-kind path (already fast) handle the rest; the cross-kind
1.32×-1.62× size-1 losses come from the `applyAtImpl`-per-element loop:

```
inline def scanLeftImpl[A, B](xs, z)(inline op): FBase =
  { val n = xs.length
    if (n == 0) fromValues1[B](z)
    else if (n == 1) fromValues2[B](z, op(z, applyAtImpl[A](xs, 0)))
    else summonFrom { … } }
```

(scanRight symmetric: `n==1 -> fromValues2[B](op(applyAtImpl(xs,0), z), z)`.) Expected: kills the
size-1 scan losses (1.32×-1.62×); the bigger size-1000+ scan losses are loop-shape (Part 3).

### mkString — GenCores:1577 (FArrayOps:1841)

Already peels `xs.length == 0`. The size-1 1.61× (Str) loss is because for n==1 it still enters the
`reduceLeafFwd${K}Ref[StringBuilder]` engine with the `if (acc.length != base)` per-element guard.
Add a len==1 arm:

```
… = if (xs.length == 0) start + end
    else if (xs.length == 1) start + String.valueOf(applyAtImpl[A](xs, 0)) + end
    else summonFrom { … }
```

### sameElements / indexOfSlice / lastIndexOfSlice / containsSlice — surface FArray.scala:295-313

The size-1 losses (3.67×-5.14×) come from `matchAll2Impl` (FArrayOps:2423) which, for a non-leaf
or per-shift, calls `materialize…(xs)` / `materialize…(that)` to get backing arrays then loops.
For tiny inputs the materialize + summonFrom dominate. These ops are on the surface; add a
length-specialized fast-path that avoids `matchAll2` for the trivial slice:

- `sameElements`: surface already guards `xs.length == that.length`; for the common `length==1`
  case add `if xs.length == 1 then applyAtImpl[A](xs,0) == that.boxedAt(0)` before calling
  `matchAll2Impl`.
- `indexOfSlice` / `lastIndexOfSlice`: add `if (m == 1)` arm that delegates to `indexOf` /
  `lastIndexOf` of the single element (both are already specialized `scFwd/scBwd` leaf scans, no
  materialize), instead of the `matchAll2`-per-shift loop. `m == 0` is already handled.
- `containsSlice`: inherits the `indexOfSlice` fix (it just calls `indexOfSlice(that) >= 0`).

Expected: the m==1 arm turns an O(n) per-shift `matchAll2` (each shift re-reads via materialized
arrays) into a single specialized element scan — fixes the 3.7×-5.1× size-1 cells and helps the
mid-size slice-search losses (Part 3) where m is small.

### reduce family size-1 (reduceLeft/reduce/reduceRight @1) — surface FArray.scala:151-163

These ALREADY peel `length == 1 -> applyAtImpl[A](xs, 0)`. The residual 1.17×-1.25× loss is the
known **`intAt` size-1 read floor** (MEMORY: "the size-1 read floor is the `intAt` call-boundary,
NOT dispatch"). Not a missing fast-path — leave as-is unless the read floor is separately attacked
(e.g. teaching `applyAtImpl` to peel `{K}One` to a direct field read before `intAt`). Flag: if the
interpreter redesign lowers the `intAt` boundary, these auto-recover.

### update / grouped / copyToArray / zipWithIndex / reverse @0 — already-peeled or structural

- `zipWithIndex` @0 (1.14×): `zipWithIndexImpl` already returns `Empty.INSTANCE` for n==0 — noise.
- `copyToArray` @0/@1: `copyToArrayImpl` already has `if (n <= 0) 0` and `if (n == 1)` arms — noise.
- `update` @0 (1.21×): `updatedImpl` peels only `length==1 && index==0`; the @0 benchmark updates a
  chain — structural, low ratio, not a simple Empty peel.
- `grouped` @0 (1.17×): returns an `Iterator` (FArray.scala:314); the loss is `Iterator.range`
  allocation — could add `if n == 0 then Iterator.empty`, marginal.
- `reverse` @0 (2.25×, IntSortAdaptive): `reverse` is an O(1) structural node (FArray.scala:119);
  a 2.25× at size 0 is almost certainly a measurement artifact — **re-measure with -f2** before
  acting.

---

## PART 3 — Non-0/1 losses (categorized, NOT in scope for the fast-path pass)

These need algorithmic work, are accepted by project goals, or are noise. Listed for completeness.

- **algo/sort-vs-Arrays.sort** (11; sortBy@10 4.89×, sortBy@100 3.25×, sorted@1000 1.08×):
  `sortByImpl` (FArrayOps:2517) does `materialize` + `mapImpl` keys + an `Int` index sort +
  scatter — 3 passes + 2 extra arrays. IArray sorts the raw array in place. Mid-size `sortBy` is
  the biggest non-0/1 gap. A decorate-sort-undecorate that sorts `(key,value)` in one structure,
  or a primitive key+value co-sort, would close it. `sorted` with a matching `Ordering.Int` already
  uses `Arrays.sort` (FArrayOps:2512) and is near-parity at large sizes.
- **algo/hash-materialize** (15; groupBy@1000 1.53×, toSet@100000 1.54×, distinct@1000 1.19×):
  bounded by JDK HashMap/HashSet hashing; FArray's unboxed group build (commit ec62d3e) already
  won the big groupBy cells. Remaining gaps are competitor hashing wins, not FArray overhead —
  low priority.
- **algo/scan-loop-shape** (7; scan@100000 1.23×): per-element loop shape (cf. MEMORY
  "scan perf = loop shape"). The cross-kind scan arms use an `applyAtImpl`-per-element loop
  (FArrayOps:2538 etc.) rather than a leaf walk; unifying onto the `scanLeftLeaf` engine for all
  acc-kinds would help. Same-kind scan is already near-parity.
- **algo/materialize-then-copy** (6; toArray Str@100000 8.51×): the Ref `toArray` for
  `Array[String]` (runtimeClass `String != Object`, FArrayOps:1871) takes the `materializeRef` +
  `arraycopy` branch when `xs` is a tree → TWO copies. Single-pass DFS straight into the typed
  `ct.newArray` (as the Int/non-matching branch already does) would halve it. The 8.51× suggests
  the Str input is a tree, not a flat `RefArr`. Surprising magnitude — worth a dedicated look.
- **algo/slice-search** (6; indexOfSlice@100000 1.14×): `matchAll2Impl` re-materializes `xs`/`that`
  on the non-leaf path; a leaf-aware sliding compare would avoid repeated materialize. The m==1
  fast-path (Part 2) covers the small-slice benchmarks.
- **algo/reduce-engine** + **algo/fold-engine-small** (14; reduce@10 1.62×, sum@10 1.26×):
  small-size fold/reduce entry overhead vs IArray's tight loop; partly the same `applyAtImpl`
  /engine-entry cost. Improves automatically if the engine entry is cheapened.
- **construction (create02..32)** (5; 1.13×-1.43×): FArray wraps the raw array in a leaf node —
  inherently >raw-array alloc. **Accepted** per CLAUDE.md ("losing to IArray on inherently-
  allocating ops … is acceptable"). All competitors here are `iarray`.
- **algo/toVector-detour** (4; combinations/permutations 1.15×-1.34×): route through
  `xs.toVector.combinations` (FArray.scala:331-332). A native primitive generator would win but is
  niche.
- **algo/partition @10000** (1, 2.21×, iarray): mid-size partition is slower than a single raw-array
  two-pass; investigate the leaf partitioner buffer growth.
- **deep-base-applyBoxed** (1; map@deep-concat 1.08×): **owned by the in-flight interpreter
  redesign** — do not touch here.
- **noise** (~6): `len`@1000 2.98× (length is O(1) — artifact, re-measure), `exists`/`indexWhere`
  ≤1.07×, `foldLeft`@deep 1.13×, several sub-1.1× cells. Confirm with `-f2` before acting.

---

## Summary — the fast-path implementation checklist

Add a front-of-impl `length` peel (Empty → trivial; {K}One → single-element direct) to these
ops, all in `codegen/src/scala/farray/GenCores.scala` at the cited template lines (or the
hand-written surface `farray/src/scala/farray/FArray.scala`):

**Missing Empty (size-0) arm — highest value:**
1. `sortedImpl`     GenCores:1616 — `if (xs.length < 2) xs else …`
2. `sortByImpl`     GenCores:1617 — `if (xs.length < 2) xs else …`
3. `sortWithImpl`   GenCores:1615 — `if (xs.length < 2) xs else …`
4. `groupByImpl`    GenCores:1622 — `if (xs.length == 0) Map.empty else …`
5. `partitionImpl`  GenCores:1599 — `if (xs.length == 0) (Empty, Empty) else …`
6. `lastIndexWhereImpl` GenCores:1592 — `if (length == 0) -1 else …`
7. `lastIndexOfImpl`    GenCores:1593 — `if (length == 0) -1 else …`
8. `sumImpl`        GenCores:1618 — `if (xs.length == 0) num.zero else …`
9. `productImpl`    GenCores:1619 — `if (xs.length == 0) num.one else …`
10. `diff`          FArray.scala:247 — `if xs.length == 0 then xs else …`
11. `intersect`     FArray.scala:251 — `if xs.length == 0 then xs else if that.length == 0 then empty else …`
12. `toSet`         FArray.scala:234 — `if xs.length == 0 then Set.empty else …`
13. `distinct`/`distinctBy` FArray.scala:204/206 — `if xs.length <= 1 then xs else …`

**Missing {K}One (size-1) arm:**
14. `toArrayImpl`   GenCores:1578 — add `length==0 -> ct.newArray(0)` and `length==1` single-store arm
15. `scanLeftImpl`  GenCores:1620 — `n==0 -> fromValues1(z)`, `n==1 -> fromValues2(z, op(z,e0))`
16. `scanRightImpl` GenCores:1621 — symmetric n==1 arm
17. `mkStringImpl`  GenCores:1577 — add `length==1 -> start + value + end`
18. `indexOfSlice` / `lastIndexOfSlice` / `sameElements` FArray.scala:295-311 — add `m==1` /
    `length==1` arm delegating to the specialized `indexOf`/`lastIndexOf`/element-compare instead of
    `matchAll2Impl`; `containsSlice` inherits via `indexOfSlice`.

**No action (already peeled or noise — confirm with -f2, do NOT change):**
`reduceLeft/reduce/reduceRight/reduceOption/reduceLeftOption` @0/@1 (already peel; residual is the
`intAt` read floor), `zipWithIndex`@0, `copyToArray`@0/@1, `sameElements`@0, `reverse`@0,
`grouped`@0, `update`@0.

### Counts to report
- **Total losses: 127.**
- **Size-0/1 losses: 48; of these ~24 are missing-Empty/{K}One fast-arm-fixable** (items 1-18
  above cover them — roughly: sort×8, distinct×2, slice-search-1×4, toArray×3, scan×3, groupBy×2,
  diff/intersect×2, toSet×1, lastIndex*×2, mkString×1, sum/product×2). The remaining ~24 of the 48
  are the `intAt` size-1 read floor (reduce family) or sub-1.1× measurement noise.

### General principle (the invariant to enforce)
Every op must **peel `Empty` and `{K}One` at the very front of its impl, before `summonFrom` and
before any `materialize…` / builder / DFS-consumer allocation** — exactly like `zipWithIndexImpl`
(FArrayOps:2417) and `zipImpl` (2391). Then size 0, size 1, and the flat-array leaf are each at
their maximal speed: size 0 returns a shared singleton (`Empty.INSTANCE` / `Map.empty` /
`Set.empty` / `-1` / `num.zero`), size 1 returns a direct single-element result, and the
multi-element path is the existing specialized body. The current losses are precisely the ops that
allocate-then-discard for the empty/singleton case because the length check sits *after* the work.

---

## Appendix — full loss table (all 127, sorted: size 0/1 first by ratio desc, then by size)

| op (class) | params | farray/best ratio | winning competitor | category |
|---|---|---|---|---|
| IntSetOpsBenchmark.intersect | size=0 | 5.57x | list | fastpath-0/1 |
| IntSlicingBenchmark.partition | size=0 | 5.10x | list | fastpath-0/1 |
| IntSetOpsBenchmark.diff | size=0 | 4.73x | list | fastpath-0/1 |
| IntSearchSliceBenchmark.sameElements | size=0 | 3.35x | iarray | fastpath-0/1 |
| IntSetOpsBenchmark.toSet | size=0 | 2.81x | iarray | fastpath-0/1 |
| IntSetOpsBenchmark.groupBy | size=0 | 2.51x | list | fastpath-0/1 |
| IntSortBenchmark.sortBy | size=0 | 2.29x | vector | fastpath-0/1 |
| StrSortBenchmark.sortBy | size=0 | 2.26x | vector | fastpath-0/1 |
| IntSortAdaptiveBenchmark.reverse | size=0 | 2.25x | vector | fastpath-0/1 |
| IntSearchSliceBenchmark.lastIndexWhere | size=0 | 2.20x | ziochunk | fastpath-0/1 |
| StrNewOpsBenchmark.groupBy | size=0 | 2.18x | ziochunk | fastpath-0/1 |
| IntSlicingBenchmark.distinct | size=0 | 2.15x | vector | fastpath-0/1 |
| IntSortBenchmark.sorted | size=0 | 2.13x | vector | fastpath-0/1 |
| StrSortBenchmark.sortWith | size=0 | 2.08x | vector | fastpath-0/1 |
| IntSortAdaptiveBenchmark.sorted | size=0 | 2.07x | vector | fastpath-0/1 |
| StrNewOpsBenchmark.toSet | size=0 | 2.01x | ziochunk | fastpath-0/1 |
| StrSortBenchmark.sorted | size=0 | 1.99x | vector | fastpath-0/1 |
| StrNewOpsBenchmark.distinct | size=0 | 1.88x | list | fastpath-0/1 |
| IntSearchSliceBenchmark.lastIndexOf | size=0 | 1.87x | iarray | fastpath-0/1 |
| IntSortBenchmark.sortWith | size=0 | 1.86x | vector | fastpath-0/1 |
| IntSortAdaptiveBenchmark.sortBy | size=0 | 1.85x | vector | fastpath-0/1 |
| StrNewOpsBenchmark.toArray | size=0 | 1.35x | list | fastpath-0/1 |
| IntUpdateChainBenchmark.update | size=0 | 1.21x | ziochunk | fastpath-0/1 |
| IntSlicingBenchmark.grouped | size=0 | 1.17x | iarray | fastpath-0/1 |
| IntConvertBenchmark.copyToArray | size=0 | 1.16x | vector | fastpath-0/1 |
| IntZipBenchmark.zipWithIndex | size=0 | 1.14x | ziochunk | fastpath-0/1 |
| IntAggregateBenchmark.sum | size=0 | 1.10x | iarray | fastpath-0/1 |
| IntAggregateBenchmark.product | size=0 | 1.08x | iarray | fastpath-0/1 |
| IntReduceBenchmark.reduceLeftOption | size=0 | 1.06x | iarray | fastpath-0/1 |
| IntReduceBenchmark.reduceOption | size=0 | 1.06x | iarray | fastpath-0/1 |
| StrNewOpsBenchmark.distinct | size=1 | 13.97x | ziochunk | fastpath-0/1 |
| IntSlicingBenchmark.distinct | size=1 | 12.26x | ziochunk | fastpath-0/1 |
| IntSearchSliceBenchmark.sameElements | size=1 | 5.14x | iarray | fastpath-0/1 |
| IntSearchSliceBenchmark.containsSlice | size=1 | 4.30x | iarray | fastpath-0/1 |
| IntSearchSliceBenchmark.indexOfSlice | size=1 | 3.77x | iarray | fastpath-0/1 |
| IntSearchSliceBenchmark.lastIndexOfSlice | size=1 | 3.67x | iarray | fastpath-0/1 |
| IntConvertBenchmark.toArray | size=1 | 2.61x | fs2chunk | fastpath-0/1 |
| StrNewOpsBenchmark.toArray | size=1 | 2.20x | fs2chunk | fastpath-0/1 |
| IntScanBenchmark.scanRight | size=1 | 1.62x | iarray | fastpath-0/1 |
| StrNewOpsBenchmark.mkString | size=1 | 1.61x | list | fastpath-0/1 |
| IntScanBenchmark.scan | size=1 | 1.34x | iarray | fastpath-0/1 |
| IntScanBenchmark.scanLeft | size=1 | 1.32x | iarray | fastpath-0/1 |
| IntReduceBenchmark.reduceLeft | size=1 | 1.25x | iarray | fastpath-0/1 |
| StrNewOpsBenchmark.reduce | size=1 | 1.21x | iarray | fastpath-0/1 |
| IntReduceBenchmark.reduce | size=1 | 1.17x | iarray | fastpath-0/1 |
| IntReduceBenchmark.reduceRight | size=1 | 1.17x | iarray | fastpath-0/1 |
| IntConvertBenchmark.copyToArray | size=1 | 1.07x | fs2chunk | fastpath-0/1 |
| IntReduceBenchmark.reduceOption | size=1 | 1.05x | iarray | fastpath-0/1 |
| IntCombinationsPermutationsBenchmark.combinations | n=6 | 1.30x | list | algo/toVector-detour |
| IntCombinationsPermutationsBenchmark.permutations | n=6 | 1.15x | vector | algo/toVector-detour |
| IntCombinationsPermutationsBenchmark.combinations | n=8 | 1.34x | list | algo/toVector-detour |
| IntCombinationsPermutationsBenchmark.permutations | n=8 | 1.15x | iarray | algo/toVector-detour |
| IntSortBenchmark.sortBy | size=10 | 4.89x | iarray | algo/sort-vs-Arrays.sort |
| IntSortAdaptiveBenchmark.sortBy | size=10 | 4.73x | iarray | algo/sort-vs-Arrays.sort |
| IntSortBenchmark.sortWith | size=10 | 2.57x | iarray | algo/sort-vs-Arrays.sort |
| IntSortAdaptiveBenchmark.sorted | size=10 | 1.75x | iarray | algo/sort-vs-Arrays.sort |
| IntReduceBenchmark.reduce | size=10 | 1.62x | iarray | algo/reduce-engine |
| IntReduceBenchmark.reduceLeftOption | size=10 | 1.60x | iarray | algo/reduce-engine |
| IntReduceBenchmark.reduceOption | size=10 | 1.56x | iarray | algo/reduce-engine |
| IntReduceBenchmark.reduceLeft | size=10 | 1.53x | iarray | algo/reduce-engine |
| IntReduceBenchmark.reduceRightOption | size=10 | 1.51x | iarray | algo/reduce-engine |
| IntReduceBenchmark.reduceRight | size=10 | 1.51x | iarray | algo/reduce-engine |
| StrNewOpsBenchmark.toArray | size=10 | 1.41x | iarray | algo/materialize-then-copy |
| StrNewOpsBenchmark.groupBy | size=10 | 1.40x | vector | algo/hash-materialize |
| StrSortBenchmark.sorted | size=10 | 1.27x | iarray | algo/sort-vs-Arrays.sort |
| IntAggregateBenchmark.sum | size=10 | 1.26x | iarray | algo/fold-engine-small |
| StrNewOpsBenchmark.reduce | size=10 | 1.22x | list | algo/reduce-engine |
| IntSortBenchmark.sorted | size=10 | 1.22x | iarray | algo/sort-vs-Arrays.sort |
| IntAggregateBenchmark.product | size=10 | 1.20x | iarray | algo/fold-engine-small |
| IntConvertBenchmark.mkString | size=10 | 1.13x | ziochunk | small-misc |
| IntAggregateBenchmark.max | size=10 | 1.12x | iarray | algo/fold-engine-small |
| IntSetOpsBenchmark.groupBy | size=10 | 1.11x | list | algo/hash-materialize |
| IntScanBenchmark.scanRight | size=10 | 1.11x | iarray | algo/scan-loop-shape |
| StrNewOpsBenchmark.mkString | size=10 | 1.11x | list | small-misc |
| IntAggregateBenchmark.min | size=10 | 1.08x | iarray | algo/fold-engine-small |
| IntShortCircuitBenchmark.exists | size=10 | 1.06x | iarray | noise/near-tie-or-loopshape |
| IntSortAdaptiveBenchmark.sortBy | size=100 | 3.25x | iarray | algo/sort-vs-Arrays.sort |
| StrNewOpsBenchmark.toArray | size=100 | 2.22x | iarray | algo/materialize-then-copy |
| IntSetOpsBenchmark.groupBy | size=100 | 1.48x | iarray | algo/hash-materialize |
| IntAggregateBenchmark.min | size=100 | 1.30x | iarray | algo/fold-engine-small |
| IntSortBenchmark.sorted | size=100 | 1.10x | iarray | algo/sort-vs-Arrays.sort |
| IntSortAdaptiveBenchmark.sorted | size=100 | 1.09x | iarray | algo/sort-vs-Arrays.sort |
| IntSearchSliceBenchmark.containsSlice | size=100 | 1.09x | iarray | algo/slice-search |
| StrNewOpsBenchmark.toSet | size=100 | 1.08x | ziochunk | algo/hash-materialize |
| IntSortBenchmark.sortBy | size=100 | 1.07x | iarray | algo/sort-vs-Arrays.sort |
| IntShortCircuitBenchmark.indexWhere | size=100 | 1.07x | iarray | noise/near-tie-or-loopshape |
| StrNewOpsBenchmark.groupBy | size=100 | 1.06x | ziochunk | algo/hash-materialize |
| IntShortCircuitBenchmark.exists | size=100 | 1.05x | iarray | noise/near-tie-or-loopshape |
| StrNewOpsBenchmark.toArray | size=1000 | 3.12x | ziochunk | algo/materialize-then-copy |
| ListLikeStringBenchmark.len | size=1000 | 2.98x | list | noise/length |
| IntSetOpsBenchmark.groupBy | size=1000 | 1.53x | iarray | algo/hash-materialize |
| IntSlicingBenchmark.distinct | size=1000 | 1.19x | vector | algo/hash-materialize |
| StrNewOpsBenchmark.groupBy | size=1000 | 1.14x | iarray | algo/hash-materialize |
| IntDeepConcatBenchmark.foldLeft | leafSize=1000,numLeaves=200 | 1.13x | iarray | noise/near-tie-or-loopshape |
| IntSearchSliceBenchmark.indexOfSlice | size=1000 | 1.08x | iarray | algo/slice-search |
| IntDeepConcatBenchmark.map | leafSize=1000,numLeaves=200 | 1.08x | iarray | deep-base-applyBoxed(redesign) |
| IntSortBenchmark.sorted | size=1000 | 1.08x | iarray | algo/sort-vs-Arrays.sort |
| IntConvertBenchmark.toArray | size=1000 | 1.07x | iarray | algo/materialize-then-copy |
| IntScanBenchmark.scan | size=1000 | 1.06x | iarray | algo/scan-loop-shape |
| StrNewOpsBenchmark.toArray | size=10000 | 5.13x | ziochunk | algo/materialize-then-copy |
| IntSlicingBenchmark.partition | size=10000 | 2.21x | iarray | algo/partition |
| StrNewOpsBenchmark.groupBy | size=10000 | 1.20x | iarray | algo/hash-materialize |
| IntScanBenchmark.scan | size=10000 | 1.15x | iarray | algo/scan-loop-shape |
| IntSearchSliceBenchmark.indexOfSlice | size=10000 | 1.12x | iarray | algo/slice-search |
| IntAggregateBenchmark.max | size=10000 | 1.10x | iarray | algo/fold-engine-small |
| IntSearchSliceBenchmark.lastIndexOfSlice | size=10000 | 1.10x | ziochunk | algo/slice-search |
| StrNewOpsBenchmark.toSet | size=10000 | 1.10x | ziochunk | algo/hash-materialize |
| IntScanBenchmark.scanLeft | size=10000 | 1.09x | iarray | algo/scan-loop-shape |
| IntConvertBenchmark.copyToArray | size=10000 | 1.08x | fs2chunk | small-misc |
| IntSetOpsBenchmark.groupBy | size=10000 | 1.07x | vector | algo/hash-materialize |
| IntSetOpsBenchmark.toSet | size=10000 | 1.06x | iarray | algo/hash-materialize |
| IntSlicingBenchmark.distinct | size=10000 | 1.06x | vector | algo/hash-materialize |
| IntCreationBenchmark.create02 |  | 1.43x | iarray | construction(allocates>raw) |
| IntCreationBenchmark.create04 |  | 1.34x | iarray | construction(allocates>raw) |
| IntCreationBenchmark.create08 |  | 1.18x | iarray | construction(allocates>raw) |
| IntCreationBenchmark.create16 |  | 1.13x | iarray | construction(allocates>raw) |
| IntCreationBenchmark.create32 |  | 1.13x | iarray | construction(allocates>raw) |
| StrNewOpsBenchmark.toArray | size=100000 | 8.51x | ziochunk | algo/materialize-then-copy |
| StrNewOpsBenchmark.toSet | size=100000 | 1.54x | iarray | algo/hash-materialize |
| IntScanBenchmark.scan | size=100000 | 1.23x | iarray | algo/scan-loop-shape |
| IntScanBenchmark.scanLeft | size=100000 | 1.14x | iarray | algo/scan-loop-shape |
| IntSearchSliceBenchmark.indexOfSlice | size=100000 | 1.14x | iarray | algo/slice-search |
| IntSearchSliceBenchmark.lastIndexOfSlice | size=100000 | 1.13x | ziochunk | algo/slice-search |
| IntScanBenchmark.scanRight | size=100000 | 1.13x | iarray | algo/scan-loop-shape |
| StrNewOpsBenchmark.groupBy | size=100000 | 1.11x | ziochunk | algo/hash-materialize |
| IntAggregateBenchmark.max | size=100000 | 1.06x | iarray | algo/fold-engine-small |
| IntConvertBenchmark.copyToArray | size=100000 | 1.05x | fs2chunk | small-misc |
