# Fusing the inner loop too: nested fusion over adjacent runs

*A sequel to [One loop, no closures](inside-the-fusion-macro.md). That post fused a flat pipeline —
`map`/`filter`/`fold` — into a single `while` loop. This one is about the operation that pipeline couldn't
express: **per-group reduction**, where the group itself is a fused sub-pipeline. The headline is that the
groups are never built.*

Here is the shape of problem that motivates everything below. You have a stream of records already **clustered
by key** — log lines grouped by request id, ticks grouped by trading day, sensor readings grouped by device —
and you want one summary row per run:

```scala
ticks.fuse
  .groupAdjacentReduceBy(_.day)(_.map(_.amount))(Agg.sum(identity))
  // → Fuse[(Day, Long)] : one (day, totalVolume) per run
```

What does the obvious implementation cost? You walk the input, and every time the key changes you close off a
group. The naive version buffers each group's rows into a `List`, then folds the list:

```scala
xs.foldRight(Nil: List[(K, List[Row])]) { (a, acc) => /* push a onto the current run, or start a new one */ }
  .map((k, rows) => (k, rows.map(_.amount).sum))
```

Count the allocations. A `List` cell per row (the group buffers). A `List` of groups. A second `List` from the
inner `.map(_.amount)`. A boxed `Long` per amount. A tuple per group. For an input of N rows and G groups, that
is O(N) garbage to produce G numbers. The rows exist as a materialized collection for exactly long enough to be
summed and discarded.

Nested fusion deletes all of it. Here is the **verbatim** code the macro emits for the example above (over an
`Int` element, the snapshot lightly de-sugared — `a.+(b)` shown as `a + b`):

```scala
val cap = n0
val out = new Array[Int](cap)            // ← Array[Int], not Array[(Int,Int)] — see §4
var o   = 0
val s   = src0; val len = s.length
var i   = 0
while (i < len) {
  val v  = intAt(s, i)
  val v₂ = v / 10                         // key(a)
  if (!started) {                         // first row of the very first run
    curKey = v₂
    acc = 0
    acc = acc + identity(identity(v))     // seed the inner sum with row 0
    started = true
  } else if (v₂ == curKey) {              // same run → fold the row in, inline
    acc = acc + identity(identity(v))
  } else {                                // key changed → emit the finished run, start a new one
    out(o) = acc; o += 1                  // ← the run's sum, no tuple (key column is dead here)
    curKey = v₂
    acc = 0
    acc = acc + identity(identity(v))
  }
  i += 1
}
if (started) { out(o) = acc; o += 1 }     // the epilogue: flush the final run
…build an IntArr from out[0..o)…
```

One loop. One `int[]` scan. The group's "rows" are an `acc` variable that resets at each key change. No buffer,
no inner collection, no boxing, no tuple — **O(1) working memory per group, zero per-group allocation**. The
inner `.map(_.amount).sum` became `acc = acc + …` in the loop body. The rows were never anywhere.

A test pins this down. `neverMaterialized_eachRowVisitedOnce` runs the pipeline with a counting inner lambda and
asserts each input row is touched exactly **once** — proving there is no buffer-then-rescan happening behind the
combinator's back.

---

## What it is

```scala
inline def groupAdjacentReduceBy[K, B, R]
    (key:  A => K)                 // cluster boundary
    (prep: Fuse[A] => Fuse[B])     // the per-group sub-pipeline (stages)
    (agg:  Agg[B, R])              // how to reduce each group
  : Fuse[(K, R)]
```

For input already clustered by `key`, each maximal run of equal keys is reduced by the fused sub-pipeline
`prep` followed by the aggregate `agg`, and the result emitted as `(k, r)`. The precondition is **adjacency**,
not global grouping: this is `Iterator#groupBy`-meets-`foldLeft`, not `Map`-building `groupBy`. (Global grouping
needs to see the whole input before it can emit anything; it is a streaming *barrier*. Adjacent grouping emits a
run the instant the key changes — it streams in constant memory, which is the whole point.)

The aggregates are the same `Agg.*` vocabulary the flat `agg` terminal uses — `sum`, `count`, `min`, `max`,
`avg`, `fold`, `reduce`, `minBy`, `maxBy` — so per-group reduction reuses the exact accumulator machinery the
rest of the pipeline already had. And `prep` is real fused stages, so the inner work is itself DCE'd, sunk past
filters, and unboxed:

```scala
// count the even rows per run — the inner filter runs inline, no per-run buffer
xs.fuse.groupAdjacentReduceBy(_.bucket)(_.filter(_.value % 2 == 0))(Agg.count)
```

```scala
while (i < len) {
  val v  = intAt(s, i)
  val v₂ = v / 10                          // key
  if (!started) {
    curKey = v₂; c = 0
    val even = v % 2 == 0
    if (even) c = c + 1                     // inner filter + count, fused
    started = true
  } else if (v₂ == curKey) {
    val even = v % 2 == 0
    if (even) c = c + 1
  } else {
    out(o) = (curKey, c); o += 1
    curKey = v₂; c = 0
    val even = v % 2 == 0
    if (even) c = c + 1
  }
  i += 1
}
…flush final run…
```

The inner `filter` is an `if` in the loop body. There is no inner `Fuse`, no inner loop, no inner collection —
it is the same body, with the run-boundary logic wrapped around it.

---

## Why the signature is `(prep)(agg)` and not `(reduce: Fuse[A] => B)`

The natural API — the one the design doc originally specced — is a single function:

```scala
def groupAdjacentReduceBy[K, B](key: A => K)(reduce: Fuse[A] => B): Fuse[(K, B)]
//                              you'd call it: _.groupAdjacentReduceBy(_.day)(_.map(_.amount).sum)
```

It does not work, and the reason is a clean lesson about macro ordering. `reduce` is `_.map(_.amount).sum`. The
`.sum` there is an **inline terminal** — it expands into a `while`-loop macro of its own. Scala inlines
*inside-out*: the inner `.sum` expands **before** the outer `groupAdjacentReduceBy` macro ever sees the lambda.
By the time the outer macro reads `reduce`, the body is no longer `_.map(_.amount).sum` — it is a fully expanded
fused loop whose *source* is the lambda parameter `g`, an `Ident("g")` with no `FArray` behind it. The inner
`.sum`'s own parse then fails: it walks back looking for a `new Fuse(src)` base case and finds a bare parameter.
This isn't a "we didn't get to it" gap — it is unobservable to the outer macro by construction, because the
expansion already happened one level down. (Confirmed empirically, not reasoned about: the `Fuse[A] => B`
spelling does not compile.)

So the inner pipeline is **split** into the two things the macro can actually read:

- **`prep: Fuse[A] => Fuse[B]`** — the per-group *stages*. These are plain non-inline marker methods (`map`,
  `filter`, `collect`), so they survive elaboration as ordinary `Apply` nodes the outer macro peels exactly like
  the top-level chain. `prep` ends in a `Fuse[B]`, not a terminal — nothing has collapsed.
- **`agg: Agg[B, R]`** — the reduction, as a value the macro reads structurally (it already knew how to read
  `Agg.sum(f)` for the flat `agg` terminal; `parseAgg1` is that matcher, lifted out to be shared).

The cost is one extra pair of parens. The benefit is that both halves arrive at the macro intact.

---

## The `@compileTimeOnly` knot, and the `AggRaw` trick

There is a second, subtler problem, and it is worth telling because the fix is a small reusable pattern for
"I need a macro-only value to survive into an *outer* macro."

`Agg.*` is deliberately a phantom. `Agg.sum(f)` is not a usable runtime value — you can't bind it to a `val` or
build a `List` of them — and every factory is `@compileTimeOnly`, so misuse is a crisp compile error pointing at
the docs rather than a silent runtime no-op. That annotation has a precise firing rule: it errors if the
reference **survives macro expansion** without being consumed.

Now look at where the `Agg` argument lands. `groupAdjacentReduceBy` returns a `Fuse[(K, R)]` and sits in the
*middle* of the chain — the macro that consumes it is the terminal (`.run`/`.sum`/…) at the *end*. If
`groupAdjacentReduceBy` were a plain non-inline marker like `map`, the `Agg.sum(...)` argument would be a
genuine compiled argument to it, and `@compileTimeOnly` fires **before** the terminal macro ever runs. You can't
pass a compile-time-only value to a normal method and hope a macro further out will clean it up later; the check
doesn't wait.

The escape is the same one the `agg` terminals use: a compile-time-only value may be passed to an `inline`
parameter destined for a macro splice. So `groupAdjacentReduceBy` is itself `inline`, and it splices a tiny
macro whose entire job is to **consume the `Agg.*` right there** and re-emit the rest of the call in a form the
outer macro can read:

```scala
inline def groupAdjacentReduceBy[K, B, R](inline key: A => K)(inline prep: Fuse[A] => Fuse[B])(inline agg: Agg[B, R]): Fuse[(K, R)] =
  ${ FuseMacro.groupReduceStageImpl[A, K, B, R]('this, 'key, 'prep, 'agg) }
```

`groupReduceStageImpl` rewrites the `Agg.*` selection to a non-`@compileTimeOnly` twin, `AggRaw.*` — same method
names, same shapes, so `parseAgg1` reads it identically — and re-emits a plain (non-inline) marker call
`groupAdjacentReduceByMarker(key)(prep)(aggRaw)`:

```scala
val aggMod    = Symbol.requiredModule("farray.Agg")
val aggRawMod = Symbol.requiredModule("farray.AggRaw")
val rewriter  = new TreeMap:
  override def transformTerm(t: Term)(owner: Symbol): Term = t match
    case Select(qual, name) if qual.symbol == aggMod => Select.unique(Ref(aggRawMod), name)   // Agg.x → AggRaw.x
    case _                                           => super.transformTerm(t)(owner)
val aggRaw = rewriter.transformTerm(agg.asTerm)(Symbol.spliceOwner).asExprOf[Agg[B, R]]
'{ $self.groupAdjacentReduceByMarker[K, B, R]($key)($prep)($aggRaw) }
```

After this splice, the `Agg.*` is gone (consumed), the `@compileTimeOnly` obligation is discharged, and the tree
holds an ordinary `groupAdjacentReduceByMarker(...)(AggRaw.sum(f))` call that the terminal macro peels like any
other stage. The match in `parseAgg1` is on the method *name* (`Select(_, "sum")`), not the owner, so it doesn't
even notice it's now reading `AggRaw` — one matcher serves both `Agg` and its twin. The pattern generalizes:
**to forward a macro-only value into a macro further out, splice an inner macro that consumes it and re-emits a
plain surrogate.**

---

## How it reuses the buffer-stage machinery

Nested fusion is not a new traversal engine — it is a new member of an existing family. The flat fusion macro
already grew a set of **buffer stages** that hold pending state across the loop and flush it at the end:
`foldAdjacentBy` (one accumulator per run), `groupAdjacentBy` (one row buffer per run), `grouped(n)`,
`takeRight(n)`. They all share one weave point: a **prologue/epilogue** pair around the loop. `scanLeft` emits
its seed in the prologue; the adjacency stages flush their final pending run in the epilogue (because the last
run has no following key-change to trigger its emit). Nested fusion slots straight in:

- **Above the loop**, three pieces of state are declared so they persist across chunks: `curKey`, `started`, and
  the inner aggregate's accumulator. The accumulator is built by `innerAggState`, which mirrors the folding
  cases of the flat terminal's `stateFor` but exposes them as a per-run `(reset, step, finish)` triple instead
  of a once-per-pipeline fold.
- **In the loop body**, the three-way branch you saw: start a run (reset + step the first row), continue a run
  (step), or — on key change — `finish` the old run, emit `(k, r)`, and start the new one. The `step` threads
  the row through `prep`'s stages via the same `buildBody` the rest of the macro uses, so the inner stages get
  the same column-sink/DCE treatment as everything else.
- **In the epilogue**, `if (started) emit the final run` — the same `!done`-gated flush the other adjacency
  stages use, so a downstream `take(k)` still short-circuits correctly and a streaming source stops pulling.

The emitted `(K, R)` goes out through the **exact same decomposed-tuple path** as `foldAdjacentBy`: it is a
`Tup` of two columns, so a downstream `.map(_._2)` reads only the run value and the key column is dead-code
eliminated. That is why the very first generated sample built an `Array[Int]`, not an `Array[(Int,Int)]` — the
`.map(_._2)` downstream meant the key was never materialized and no tuple was ever constructed. When the tuple
*is* kept and both fields are primitive, it is a `Tuple2$mcII$sp` — unboxed — not a generic boxing `Tuple2`.

---

## Performance

The benchmark (`NestedFusionBenchmark`) runs the same logical group-and-reduce four ways: FArray fused vs the
hand-written adjacent-run fold over `List`, `Vector`, and `IArray`. Input is `Int`, clustered into runs of 16.
There is no stdlib operation for "reduce each adjacent run," so the competitors are the realistic code you'd
otherwise write by hand. Measured throughput ratios (FArray fused ÷ competitor; >1 means fused is faster), at
size 100 000:

| scenario | vs IArray | vs List | vs Vector |
|---|---|---|---|
| `groupSum` — Σ per run, emit `(k, sum)` | **1.45×** | 3.30× | 3.89× |
| `groupMin` — min per run | **1.38×** | 3.11× | 3.88× |
| `groupFilterCount` — count even rows per run (inner filter) | **1.64×** | 3.12× | 3.66× |
| `groupProject` — Σ per run, then `.map(_._2).sum` | 0.99× (tie) | 4.00× | 3.50× |
| `groupNaive` — buffer each run, then `.sum` it | **8.26×** | 26.8× | 51.2× |

Three things to read out of that:

- **Against `List`/`Vector` (boxed), fused wins outright, 3–4×**, because the competitors box every element and
  allocate a `List`/`Vector` of tuples while fused does one unboxed scan into a flat leaf.
- **Against `IArray` (the raw-array baseline), fused still wins on four of five** — even though `IArray` is a
  hand-rolled `int[]` loop — because the *result* it builds is an unboxed `Tuple2$mcII$sp` leaf, where `IArray`
  builds a boxed `(Int,Int)[]`. The one tie is `groupProject`: `.map(_._2)` discards the key, so both sides
  collapse to a scalar `int` accumulator and there is simply nothing left to win.
- **`groupNaive` is the real-world number — 8× over `IArray`, 27–51× over `List`/`Vector`** — because that is
  what people *actually* write when they don't hand-roll the fold: split into runs, materialize each as a
  collection, then `.sum` it. That O(N) materialization is exactly what nested fusion deletes.

The numbers live in [`bench-results.json`](bench-results.json) / the rendered [report](index.html), regenerated
with `scripts/bench-run.sh` like every other op. The shape to remember: nested fusion turns an O(N)-garbage
group-and-reduce into an O(1)-per-group scan, and the bigger the per-group work `prep` does, the more the gap
widens — every inner stage is one the competitors pay per row into a collection and fused pays as a register.

---

## What's in v1, and what's next

v1 deliberately ships the load-bearing core and stops:

- **One folding aggregate per group.** `sum`/`count`/`min`/`max`/`avg`/`fold`/`reduce`/`minBy`/`maxBy`. Multiple
  aggregates per group (`(sum, count)` in one pass) is a clean follow-up — the flat `agg` already does it; it's a
  matter of carrying a list of `innerAggState`s.
- **Stateless inner stages.** `map`/`filter`/`filterNot`/`collect` inside `prep`. `take`/`drop`/`takeWhile`
  inside a group need *resettable per-run counters* (the counter must zero at each key change, not once), which
  is more plumbing than v1 wanted to commit to.
- **Element-retaining / short-circuiting per-group reduces** (`topN`, `find`, `exists`) error clearly rather
  than silently doing the wrong thing — they aren't pure folds.
- The materialize escape hatch stays: `groupAdjacentBy(key)` emits each run's rows as a real `FArray`, and you
  reduce them yourself, when you genuinely want the rows.

The through-line from the flat macro holds: collapse to the smallest set of primitives, reuse the accumulator
and buffer-stage machinery rather than growing parallel paths, and prove the memory bound with a test rather
than asserting it. Nested fusion is one loop too — it just has a run boundary in the middle.
```
