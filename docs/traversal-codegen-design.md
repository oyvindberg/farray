# A codegen-time traversal abstraction for farray

> **Status:** design only. No `GenCores.scala` / `FArray.scala` changes; no build. This
> document specifies a refactor of the *generator* (`codegen/src/scala/farray/GenCores.scala`),
> not of the runtime.

## 0. The one fact this whole design rests on

`dfsBody` (GenCores.scala 134–209) is **already a parameterized walker**. It is a pure
`String`-producing function whose three *code-emitter holes*

```
onRF(arr, start, count)   // a forward array run
onRB(arr, start, count)   // a backward array run
onO(elem)                 // a single element
```

are spliced into a complete monomorphic node walk over the FArray tree
(`leaf / One / Prepend / Append / Concat / Reverse / Pad / Updated / Slice / Range`). The
arm table — the per-node `cur match { … }` — is written **once**, in `dfsBody`, and the two
mirror directions (`backward = false | true`) are encoded by that same function.

The runtime tax today is **not** in `dfsBody`. It is in the *single place its holes are
filled*: `dfsCDef` (213–231) fills them with `c.onRunF(...)` / `c.onRunB(...)` /
`c.onOne(...)` — virtual calls into a freshly-allocated heap consumer object `${K}Dfs`
(213–214). Every op (`foldLeft` 387, `map` 476, `filter` 486, `exists` 404 …) builds one of
those objects, stuffs its accumulator into a **heap field**, and drives the **non-inline**
`dfsC${K}` pair (224–230), which is **shared across all `${K}Dfs` subclasses** — i.e. its
`onRunF` callsite is megamorphic.

So the redesign is **not** "throw away the walker." It is:

> **stop routing the holes through a heap consumer; fill them with the op body directly, at
> lowering time, inlined into the op method.**

That is the strymonas / multi-stage move precisely: the `${K}Dfs` object is the *stage-1
generator's* scaffolding. It must not survive into stage-2 (the emitted code). Reifying
`(direction, holes, accumulator, output, early-exit)` as a generator-only case class and
writing one `lower` (push) + one `lowerCursor` (pull) emitter over `dfsBody`'s arm list
makes that literal.

This kills three taxes at once:

| Tax | What it is today | Killed by |
|---|---|---|
| (a) consumer allocation + heap-field accumulator | `new ${K}Dfs { var-field acc; … }` per op (387, 476, 486, 436 …) | accumulators become **plain `var` locals**; no object |
| (b) virtual / megamorphic `onRunF` | `c.onRunF(a,s,n)` into the shared `dfsC` driver (220) | the run body is **spliced inline**; no method call |
| (c) duplicated `instanceof` | surface `n==0 / n==1 / else` ladders (811–852) *plus* `applyAtImpl`'s own `match` as the n==1 path *plus* the walk's `match` | **one** `cur match` per op; small cases fall out of the `Empty`/`One` arms |

---

## (A) The traversal AST

### A.1 What it is, concretely

The AST is a **Scala case class describing one traversal**. It lives only in the codegen (the
compile-time of the *generator*), is consumed by `lower(spec): String`, and is **never a
runtime value** — it never appears in generated code. It is the reification of the
`(direction, holes, accumulator, output, early-exit)` tuple that today is passed implicitly
as lambda arguments to `scanB`, `runMethods`, `foldLeft`, `filter`.

```scala
// codegen-only. Fields are CODE FRAGMENTS (Strings) or small enums. `k` is the element Kind.
final case class Trav(
  k:    Kind,                 // existing GenCores.Kind row (Int/Long/Double/Ref)
  dir:  Dir,                  // Fwd | Bwd  (which of dfsBody's two mirror walks)
  body: Body,                 // the per-element work, as code templates
  acc:  List[Acc],            // 0..n accumulators threaded as plain `var`s
  exit: Exit,                 // Never | OnPredInCond  (the short-circuit shape)
  out:  Out                   // NoOut | Scalar | PrimArray | ResultFArray
)

enum Dir { case Fwd, Bwd }

// The op body, expressed ONCE, lowered to BOTH a tight run-loop and a single-element form.
// `elem` is a code fragment that reads the current element (already wrapped via read(k)).
final case class Body(
  perElem: String => String,  // e => "<work on e>"   (was scanB's `hit` / runMethods' `body`)
  rawArray: Boolean = false   // true when the op scans without wrapping (sum/product read a(i))
)

final case class Acc(name: String, tpe: String, init: String)   // e.g. Acc("acc","Z","z")

enum Exit {
  case Never                                       // count/sum/product/foreach/map/filter
  case OnPredInCond(                               // the EMPTY-BODY predicate-in-condition shape
    cont:   String => String,                      //   a0 => the loop-continue predicate
    onStop: String,                                //   what to record when the loop stops early
    onOne:  String                                 //   single-element form
  )
}

enum Out {
  case NoOut                                       // foreach
  case Scalar(read: String)                        // foldLeft/sum/count: result is an acc expr
  case PrimArray(elemKind: Kind,
                 write: (String /*idx-cursor*/, String /*elem*/) => String)
  case ResultFArray(elemKind: Kind,                // map/filter/collect: build then canonicalize
                    sizeHint: SizeHint,
                    write:    (String, String) => String,
                    finalize: Finalize)
}

enum SizeHint { case Exact(expr: String); case AtMost(expr: String) }   // n, or n then trim
enum Finalize { case LeafExact; case LeafTrim; case FilterIdentity }    // which epilogue
```

This captures **exactly the six properties asked for, and nothing more**:

| Property | AST field |
|---|---|
| direction (fwd/bwd) | `dir: Dir` |
| per-element body | `body.perElem` |
| per-run body (tight leaf loop) | **derived** — see A.2; not a separate field |
| accumulator (type/init/combine) | `acc` (type+init) + `body.perElem` (the combine) |
| early-exit / short-circuit | `exit: Exit` (and `OnPredInCond` carries the *empty-body* form) |
| output-construction kind | `out: Out` (NoOut / Scalar / PrimArray / ResultFArray w/ variable length) |

### A.2 The decisive choice: the "per-run body" is NOT a separate AST field

The codebase already proved (memory note *scan perf = loop shape*) that **throughput is
governed by the run loop's *shape***, and that there are exactly **two** shapes worth
emitting:

* the **full-run constant-stride** loop (`runMethods`, 376–381):
  `var i = start; val e = start+count; while (i < e) { <body>; i += 1 }`
* the **empty-body, predicate-in-condition** scan (`scanB`, 399–403):
  `while (i < e && cont(read)) i += 1; if (i < e) { …record…; stop = true }`

So the AST carries `exit`, and `lower` *picks the loop shape from it*:

```
Exit.Never         ->  runMethods shape   (process every element)
Exit.OnPredInCond  ->  scanB shape        (empty body, predicate in the while-condition)
```

This is strymonas's "loop shape is a generation-time decision," specialized to the two shapes
*this codebase measured as optimal*.

> **No `Step/Skip/Done` IR in the per-op walker.** A `Step` IR is right for a *fusion
> pipeline* (multiple ops chained — and that already exists as `.fuse`). For a *single op over
> the tree* it would force a per-element branch — exactly the loop-carried-dependency shape
> benchmarked at **1.45× slower** (memory note). Decisive call: the Step IR belongs **only** in
> `.fuse`, never in `lower`/`lowerCursor`.

### A.3 The per-node arms are described once — and stay that way

The arm table (`leaf / One / Prepend / Append / Concat / Reverse / Pad / Updated / Slice /
Range`) is **already** the `cases` list inside `dfsBody` (195–197). The redesign keeps
`dfsBody` essentially verbatim as the **single source of traversal order** and changes only
*who fills its holes*. The AST never re-describes arms. `lower(spec)` calls
`dfsBody(spec.k, spec.dir == Bwd, onRF, onRB, onO)` with the holes computed from
`spec.body` / `spec.exit` / `spec.out`. "Arms reused across all ops" is therefore not new
work — it is the existing invariant, made load-bearing.

---

## (B) The lowering: exact generated shape

`lower(spec): String` is a pure function from `Trav` to the op-body string. It does four
things:

1. **Pick the loop shape & build the three holes** from `spec.exit`:
   * `Exit.Never`: `onRF` = `runMethods`-fwd over `spec.body.perElem`; `onRB` = its mirror;
     `onO` = `spec.body.perElem` applied to the single element.
   * `Exit.OnPredInCond(cont, onStop, onOne)`: `onRF`/`onRB` = the `scanB` empty-body loops;
     `onO` = `onOne`. Early-exit becomes a **local `var stop` + a post-run `if (stop) …`**,
     with `stop`/the result living in the *op method itself*, not a consumer object.
2. **Splice the walk**: `dfsBody(spec.k, spec.dir == Bwd, onRF, onRB, onO)` — producing the
   `var cur = root; while (cur != null) { cur match { …arms… } }` body **inlined into the op
   method**. No `dfsC` call, no `${K}Dfs`.
3. **Declare the accumulators as plain locals** *outside* the walk: `var acc = z` etc. These
   are now JVM locals (registers), read/written directly by `spec.body.perElem`. This is the
   whole point — it kills tax (a).
4. **Emit the `Out` epilogue**: return the scalar; or wrap the built primitive array to a leaf
   via the **existing `leaf(k,arr,len)` canonicalizer** (370–371); etc.

### B.1 `foldLeft` — fold to a scalar

**Today** (foldLeft 387–389, surface 811): allocate `new IntDfs { var-field acc;
onRunF/onRunB/onOne }`, call the non-inline `dfsCInt`, read the field back; *and* a surface
`{ val n = xs.length; if (n==0) z else if (n==1) op(z, applyAtImpl[A](xs,0)) else … }` ladder
wraps it — so three `instanceof`-bearing matches (the n==1 `applyAt`, the walk, the runtime
class checks) plus a heap object plus a megamorphic `onRunF`.

**After lowering**, the `Int` arm of `foldLeftImpl` is, in full:

```scala
// emitted INLINE — no IntDfs, no dfsCInt call, no n==0/1 ladder
{
  var acc = z                                   // (3) accumulator is a LOCAL (a JVM register)
  var cur: FBase = root                         // (2) dfsBody spliced straight in
  var stack: Array[FBase] = null
  var tail: Array[Int] = null
  var isTail: Array[Boolean] = null
  var sp = 0
  while (cur != null) {
    var cont: FBase = null
    cur match {
      case leaf: IntArr =>                       // RUN arm: runMethods shape, body SPLICED
        var i = 0; val e = leaf.length
        while (i < e) { acc = op(acc, r.wrap(leaf.arr(i))); i += 1 }
      case o: IntOne     => acc = op(acc, r.wrap(o.elem))             // ONE arm (small case!)
      case p: IntPrepend => acc = op(acc, r.wrap(p.elem)); cont = p.base
      case a: IntAppend  => /* pushTail(a.elem) */ ; cont = a.base
      case c: Concat     => /* push c.right */    ; cont = c.left
      case rev: ReverseNode => /* cold path — see B.4 */
      case s: SliceNode  => /* leaf-window run spliced, else element loop */
      case pad: IntPad   => /* base run + filler loop, body spliced */
      case u: IntUpdated => /* leaf: prefix run, elem, suffix run; else materialize (B.4) */
      case _ => ()
    }
    if (cont != null) cur = cont
    else { cur = null
           while (sp > 0 && cur == null) {
             sp -= 1
             if (isTail != null && isTail(sp)) acc = op(acc, r.wrap(tail(sp)))
             else cur = stack(sp) } }
  }
  acc                                           // (4) Out.Scalar epilogue
}
```

* `acc` is a **local** — no heap field. **Tax (a) gone.**
* `op` is the user's `inline` lambda; it splices into the leaf loop body. **Tax (b) gone** —
  no `onRunF`, no method call, no megamorphism.
* The `Empty` case needs no code: the walk runs zero iterations and returns the `acc` init
  (`z`). The `One` case is the `IntOne` arm. So the surface `{ if (n==0) z else if (n==1)
  op(z, applyAtImpl…) else … }` ladder is **deleted**; `foldLeftImpl` becomes
  `inline def foldLeftImpl[A,Z](xs,z)(inline op) = dispatchA(k => lower(foldLeftSpec(k)(z, op)))`.
  **One match per op — tax (c) gone.**

The spec that produced it:

```scala
def foldLeftSpec(k: Kind) = Trav(k, Dir.Fwd,
  Body(e => s"acc = op(acc, $e)"),
  acc  = List(Acc("acc", "Z", "z")),
  exit = Exit.Never,
  out  = Out.Scalar("acc"))

def foldRightSpec(k: Kind) =                        // direction is now ONE field
  foldLeftSpec(k).copy(dir = Dir.Bwd, body = Body(e => s"acc = op($e, acc)"))
```

`foldRight` falling out of `foldLeft` by `.copy(dir = Bwd)` is the proof the abstraction
pays: the direction-mirroring `dfsBody` already encodes is now *exposed as one field*, so
backward ops are **declarations, not hand-written mirrors**. (Same for `lastIndexWhere =
indexWhere.copy(dir = Bwd, …)`.)

### B.2 `map` — build a primitive-array result

**Today** (`mapM` 476–485, surface 826–829): a `dispatchA × dispatchB` nest allocates `out`,
allocates `new ${KA}Dfs { var-field o; onRunF writes out(o); … }`, drives non-inline
`dfsC${KA}`, then `new ${KB}Arr(out, n)`. Plus the surface `if (n==0) … else if (n==1)
fromValues1(f(applyAt…)) else …` ladder.

**After lowering** — the `Int → Double` arm of `mapImpl` (read kind `Int`, write kind
`Double`), in full:

```scala
// dispatchA(ka=Int) -> dispatchB(kb=Double); emitted INLINE, no IntDfs, no dfsCInt
{
  val out = new Array[Double](n)                 // SizeHint.Exact("n")
  var o   = 0                                    // the write cursor — a LOCAL
  var cur: FBase = root
  var stack: Array[FBase] = null
  var tail: Array[Int] = null
  var isTail: Array[Boolean] = null
  var sp = 0
  while (cur != null) {
    var cont: FBase = null
    cur match {
      case leaf: IntArr =>                        // RUN arm, body SPLICED (the write)
        var i = 0; val e = leaf.length
        while (i < e) { out(o) = rb.unwrap(f(r.wrap(leaf.arr(i)))); o += 1; i += 1 }
      case x: IntOne     => out(o) = rb.unwrap(f(r.wrap(x.elem))); o += 1
      case p: IntPrepend => out(o) = rb.unwrap(f(r.wrap(p.elem))); o += 1; cont = p.base
      case a: IntAppend  => /* pushTail */ ; cont = a.base
      case c: Concat     => /* push right */; cont = c.left
      case rev: ReverseNode => /* cold path (B.4) */
      case s: SliceNode  => /* leaf-window run spliced */
      case pad: IntPad   => /* base run + filler, body spliced */
      case u: IntUpdated => /* leaf segments, else materialize (B.4) */
      case _ => ()
    }
    if (cont != null) cur = cont
    else { cur = null
           while (sp > 0 && cur == null) {
             sp -= 1
             if (isTail != null && isTail(sp)) { out(o) = rb.unwrap(f(r.wrap(tail(sp)))); o += 1 }
             else cur = stack(sp) } }
  }
  // Out.ResultFArray finalize = LeafExact -> the EXISTING canonicalizer, upholds Empty/One:
  { val _l = n; if (_l == 0) Empty.INSTANCE else if (_l == 1) new DoubleOne(out(0))
                else new DoubleArr(out, _l) }
}
```

* `out`, `o` are locals; no `${KA}Dfs`. **Taxes (a)/(b) gone.**
* The epilogue is **exactly the existing `leaf(kb, "out", "n")`** (370–371), so the result is
  still canonical (length 0 ⇒ `Empty`, 1 ⇒ `DoubleOne`). The surface n==0/1 ladder is
  **deleted** — `Empty` is the zero-iteration walk; `One` is the `IntOne` arm; the epilogue
  re-canonicalizes. **Tax (c) gone.**

```scala
def mapSpec(ka: Kind, kb: Kind) = Trav(ka, Dir.Fwd,
  Body(e => s"out(o) = ${wr(kb, s"rb.unwrap(f($e))")}; o += 1"),
  acc  = List(Acc("o", "Int", "0")),
  exit = Exit.Never,
  out  = Out.ResultFArray(kb, SizeHint.Exact("n"),
                          write = (o, e) => s"out($o) = $e",
                          finalize = Finalize.LeafExact))
// surface: inline def mapImpl[A,B](xs)(inline f) =
//   dispatchA(ka => dispatchB(kb => lower(mapSpec(ka, kb))))
```

### B.3 `filter` — build a result with a variable kept-count

`filter` is `map` whose write is *guarded by the predicate*, so the kept-count `o` is **not**
`n`. The size hint is `AtMost(n)` and the finalize is `FilterIdentity` (the `if (o==n) xs`
shortcut, then trim).

**Today** (`filter` 486–488, surface 830–833): `new ${K}Dfs { var-field o; … }`, drive
`dfsC${K}`, then `if (o==n) xs else if (o==0) Empty else if (o==1) One else new Arr(copyOf
…)`; plus the surface n==0/1 ladder.

**After lowering** — the `Int` arm of `filterImpl`, in full:

```scala
{
  val out = new Array[Int](n)                    // SizeHint.AtMost("n")
  var o   = 0                                    // kept-count — a LOCAL, NOT == n
  var cur: FBase = root
  var stack: Array[FBase] = null
  var tail: Array[Int] = null
  var isTail: Array[Boolean] = null
  var sp = 0
  while (cur != null) {
    var cont: FBase = null
    cur match {
      case leaf: IntArr =>                        // RUN arm: guarded write, body SPLICED
        var i = 0; val e0 = leaf.length
        while (i < e0) {
          val e = r.wrap(leaf.arr(i))
          if (p(e)) { out(o) = r.unwrap(e); o += 1 }
          i += 1
        }
      case x: IntOne     => val e = r.wrap(x.elem); if (p(e)) { out(o) = r.unwrap(e); o += 1 }
      case p2: IntPrepend=> val e = r.wrap(p2.elem); if (p(e)) { out(o)=r.unwrap(e); o+=1 }; cont=p2.base
      case a: IntAppend  => /* pushTail */ ; cont = a.base
      case c: Concat     => /* push right */; cont = c.left
      case rev: ReverseNode => /* cold path (B.4) */
      case s: SliceNode  => /* leaf-window guarded run */
      case pad: IntPad   => /* base + filler, guarded */
      case u: IntUpdated => /* leaf segments, else materialize (B.4) */
      case _ => ()
    }
    if (cont != null) cur = cont
    else { cur = null
           while (sp > 0 && cur == null) {
             sp -= 1
             if (isTail != null && isTail(sp)) { val e=r.wrap(tail(sp)); if (p(e)) { out(o)=r.unwrap(e); o+=1 } }
             else cur = stack(sp) } }
  }
  // Out finalize = FilterIdentity (kept its identity shortcut + Empty/One canonicalization):
  if (o == n) xs
  else if (o == 0) Empty.INSTANCE
  else if (o == 1) new IntOne(out(0))
  else new IntArr(java.util.Arrays.copyOf(out, o), o)
}
```

`o` is a local; no consumer object; one `match`; the `if (o==n) xs` identity is preserved in
the epilogue. The variable kept-count is just the accumulator `o` not equalling `n`, which the
`Out.ResultFArray(AtMost, FilterIdentity)` epilogue handles.

```scala
def filterSpec(k: Kind) = Trav(k, Dir.Fwd,
  Body(e0 => s"val e = $e0; if (p(e)) { out(o) = ${wr(k, "r.unwrap(e)")}; o += 1 }"),
  acc  = List(Acc("o", "Int", "0")),
  exit = Exit.Never,
  out  = Out.ResultFArray(k, SizeHint.AtMost("n"),
                          write = (o, e) => s"out($o) = $e",
                          finalize = Finalize.FilterIdentity))
```

### B.4 Short-circuit ops keep the measured-optimal shape

For `exists`, `lower` is called with

```scala
def existsSpec(k: Kind) = Trav(k, Dir.Fwd,
  Body(_ => ""),                                   // empty body: the work is the predicate
  acc  = List(Acc("res", "Boolean", "false")),
  exit = Exit.OnPredInCond(
           cont   = a0 => s"!p($a0)",              // continue while NOT found
           onStop = "res = true",                  // run stopped early -> found
           onOne  = "if (p(a0)) res = true"),
  out  = Out.Scalar("res"))
```

and emits the leaf arm as the **empty-body** loop verbatim from `scanB` (400), with
`res`/`stop` as **locals**:

```scala
case leaf: IntArr =>
  var i = 0; val e = leaf.length
  while (i < e && !p(r.wrap(leaf.arr(i)))) i += 1      // EMPTY body — only `i += 1`
  if (i < e) { res = true; stop = true }              // inspect WHY it stopped, ONCE
```

The crucial loop-carried-dependency-free shape is preserved **by construction**: the AST's
`Exit.OnPredInCond` carries the *empty-body predicate*, never a mid-loop write. The generator,
not luck, guarantees the ~IArray-speed shape (memory note *scan perf = loop shape*).

**The `return` subtlety.** Today the `if (c.stop) return` sits in `dfsBody`'s holes (220–222)
and returns from the *non-inline* `dfsC` driver. Inlined, a bare `return` returns from the
*whole op method* — which is exactly right for `exists` (its last statement is the walk;
returning `res` is fine). For an op where the walk is **not** the last statement, `lower`
emits a **`scala.util.boundary` / `boundary.break`** boundary (zero-cost in Scala 3 when local
and monomorphic) instead of a bare `return`, so the empty-body shape is untouched.

> **Decisive call:** prefer `boundary` over reintroducing a per-element flag re-test. If Wave 2
> benchmarks show `boundary` costs anything vs today's `if (c.stop) return`, fall back to a
> *per-run* (not per-element) `stop` re-test — the memory note already shows after-loop checks
> are fine; only **mid-loop** writes hurt.

### B.5 How the duplicated `instanceof` collapses to ONE match

This is taxes (b) and (c), and the collapse is mechanical:

* **The surface `n==0 / n==1 / else` ladder is deleted.** The single lowered walk handles
  `Empty` (zero iterations ⇒ return the `acc` init / the `Out` empty case), `One` (the
  `${K}One` arm), and the leaf/tree arms — *in one place*. So e.g. `foldLeftImpl` becomes just
  `dispatchA(k => lower(foldLeftSpec(k)))` with **no length pre-check and no `applyAtImpl`
  n==1 path**. The `applyAtImpl`-as-n==1 fast path (the *third* `instanceof`) vanishes for
  every converted op — `applyAtImpl` survives only for genuine random access `apply(i)`.
* **The Empty/One canonicalization invariant does the rest.** Because farray maintains
  "length-0 ⇒ `Empty`, length-1 ⇒ `${K}One`" at every build site (the `leaf(k,arr,len)`
  helper 370–371, the smart constructors), the lowered walk's `Empty`/`${K}One` arms are
  *correctness-complete* for the small cases — there is no separate small-case logic to write.
  The output epilogue routes through the **same** `leaf` canonicalizer, so `map`/`filter`
  results stay canonical.

Net structural change to the file: the ~25 star-Impls (811–852) lose their hand-written guards
and become one-liners delegating to `lower(...)`. The `dfsConsumer`/`dfsCDef` emitters
(213–231) are **deleted** for every converted op. `dfsBody` (134–209) stays as the shared arm
table, now invoked at *lowering time* with op-specific holes instead of `c.onRunF`.

### B.6 Reverse and the deep-base arms (the only non-mechanical part)

`dfsBody`'s `reverseCase` (173) is `case rev: ReverseNode => { dfsC${k}$other(rev.base, c);
if (c.stop) return }` — it leans on the **non-inline driver pair** to carry the opposite
direction (the JVM stack carries the nesting). Once inlined there is no `c` and no driver to
call. Resolution:

* The lowered inline walk handles every **hot** arm with the body spliced: `leaf`, `One`,
  `Concat`, `Append`, `Prepend`, leaf-`Slice`, `Range`, `Pad`-over-leaf, and
  reverse-**of-leaf** (a descending run window).
* `ReverseNode`-over-a-**non-leaf**, and `Updated`/`Slice`/`Pad`-over-a-non-leaf, fall back to
  `materialize${k}(thatSubNode)` then a run over the fresh array — **exactly what `dfsBody`
  already does for `Updated`-over-non-leaf today** (192, 194). This is rare (smart constructors
  keep reverse shallow / over-leaf and collapse reverse-of-reverse; the memory note already
  parks `Updated`-over-non-leaf as rare). So the inline walk needs **no** mutually-recursive
  driver; the cold path materializes one sub-node, removing the only arm that didn't inline
  cleanly, at the cost of a rare, already-accepted sub-materialization.

---

## (C) Iterator as another lowering of the same AST

Iterator is the **pull** lowering: the *same arm table*, a *second emitter*. Today (`iteratorV`
465–466) a non-leaf is **materialized** into a flat array and wrapped in a 2-field cursor
(236–245). The pull emitter removes that materialize for every common shape.

`lowerCursor(k): String` re-walks **the same `dfsBody` arm list** with a different
interpretation of the three holes:

* `onRF(a, s, n)` ⇒ *install a run window `(runArr = a, runIdx = s, runEnd = s+n)` and return
  from `advance()`* (suspend mid-run).
* `onO(v)` ⇒ *install a length-1 computed run (`cVal = v, cEnd = 1`) and return.*
* `cont` / `push` / `pushTail` ⇒ **verbatim** (same stack discipline).

The driver's locals (`cur/stack/tail/isTail/sp`) become **cursor fields**; the new state is the
run-window triple + a small computed-run quintet (range/filler/one). `next()` drains the live
run (`runArr(runIdx); runIdx += 1`) in amortized O(1); when empty it calls `advance()`.

**Sketch of the generated `IntCursor` (pull = an explicit resumable state machine):**

```scala
final class IntCursor(root: FBase, len0: Int) extends scala.collection.AbstractIterator[Any] {
  // ---- traversal state: what were dfsC LOCALS are now FIELDS ----
  private var cur: FBase = root
  private var stack: Array[FBase] = null
  private var tail: Array[Int] = null
  private var isTail: Array[Boolean] = null
  private var sp = 0
  // ---- the live run window (a leaf/slice/pad-base array, NO materialize) ----
  private var runArr: Array[Int] = null
  private var runIdx = 0
  private var runEnd = 0
  // ---- a computed run for One / Range / Pad-filler (no array) ----
  private var cVal = 0
  private var cIdx = 0
  private var cEnd = 0
  private var cStep = 0                       // Range stride; 0 for One/filler
  private var remaining = len0

  def hasNext: Boolean = remaining > 0
  override def knownSize: Int = remaining

  def nextInt(): Int = {
    remaining -= 1
    if (runIdx < runEnd) { val r = runArr(runIdx); runIdx += 1; r }       // drain array run
    else if (cIdx < cEnd) { val r = cVal + cIdx * cStep; cIdx += 1; r }   // drain computed run
    else { advance(); nextInt() }                                        // refill, then retry
  }
  def next(): Any = java.lang.Integer.valueOf(nextInt())

  // advance(): SAME arm table as dfsBody, holes reinterpreted as "install a run & return".
  private def advance(): Unit = {
    while (cur != null) {
      var cont: FBase = null
      cur match {
        case leaf: IntArr  => runArr = leaf.arr; runIdx = 0; runEnd = leaf.length
                              cur = stepCont(); return                     // onRF: suspend mid-run
        case o: IntOne     => cVal = o.elem; cIdx = 0; cEnd = 1; cStep = 0
                              cur = stepCont(); return                     // onO
        case p: IntPrepend => cVal = p.elem; cIdx = 0; cEnd = 1; cStep = 0
                              cur = p.base; return
        case a: IntAppend  => /* pushTail(a.elem) */ ; cur = a.base
        case c: Concat     => /* push c.right */     ; cur = c.left
        case rng: RangeNode=> cVal = rng.start; cIdx = 0; cEnd = rng.length; cStep = rng.step
                              cur = stepCont(); return                     // Range: NO materialize
        case s: SliceNode  => s.base match {
                                case lf: IntArr => runArr = lf.arr; runIdx = s.offset
                                                   runEnd = s.offset + s.length
                                                   cur = stepCont(); return
                                case _ => runArr = materializeInt(s); runIdx = 0
                                          runEnd = s.length; cur = stepCont(); return }  // cold (B.4)
        case pad: IntPad   => /* base run then filler computed-run, suspend */
        case u: IntUpdated => /* leaf segments, else materializeInt(u) */
        case rev: ReverseNode => /* reverse-of-leaf: descending window; else materializeInt(rev) */
        case _ => cur = stepCont()
      }
    }
  }
  // pop the deferred stack exactly as the dfsC pop loop does, returning the next `cur` (or null)
  private def stepCont(): FBase = {
    var nx: FBase = null
    while (sp > 0 && nx == null) {
      sp -= 1
      if (isTail != null && isTail(sp)) { cVal = tail(sp); cIdx = 0; cEnd = 1; cStep = 0; return null }
      else nx = stack(sp)
    }
    nx
  }
}
```

* **No `materialize`** for leaf / Concat-tree / Append-Prepend chains / `Range` / `Pad`-filler
  / reverse-of-leaf / leaf-`Slice` — killing today's `iteratorV` materialize (466).
* `ReverseNode`-over-tree and non-leaf `Slice`/`Pad`/`Updated` bases keep the targeted
  sub-materialize (same cold path as B.6).
* **`iterator` is the only op that gets the pull lowering.** Everything internal
  (fold/foreach/scan/map/filter/sum) gets the push lowering. The arm table is shared:
  `lower` and `lowerCursor` are **two interpreters over one arm table** — exactly the
  two-emitter structure. "Iterator is just another lowering of the same AST" holds literally.

---

## (D) Codegen structure to invest in

```
EXISTING (keep):
  Kind                                    // Int/Long/Double/Ref row (103, 115)
  dfsBody(k, backward, onRF, onRB, onO)   // THE single arm table = traversal order (134)
  runMethods(k, body, rawA)               // full-run constant-stride loop, both dirs (376)
  scanB(...)  / its onRunF/onRunB innards // empty-body pred-in-cond loop (399)
  dispatchA / dispatchB                   // per-kind summonFrom threading — UNCHANGED (382)
  leaf(k, arr, len)                       // the Empty/One canonicalizer (370) — Out reuses it
  materialize${k}                         // cold-path sub-node flatten (289) — B.4/C reuse it

NEW (this design):
  Trav / Body / Acc / Exit / Out          // the AST (codegen-only case classes, §A)
  lower(spec: Trav): String               // PUSH emitter: declares accs as locals, picks
                                           //   runMethods vs scanB from spec.exit, splices
                                           //   dfsBody, emits the Out epilogue (§B)
  lowerCursor(k: Kind): String            // PULL emitter (§C), reuses the dfsBody arm list

DELETE (per converted op):
  dfsConsumer(k)  = `abstract class ${K}Dfs …`      (213)   // the heap consumer
  dfsCDef(k)      = `dfsC${K}Fwd/Back/inline …`     (215)   // the non-inline megamorphic driver
  cursorClass(k)  = the flat-array `${K}Cursor`     (236)   // replaced by lowerCursor's output
  the n==0/1 surface ladders                        (811-852)
```

**How an op declares its traversal** — one `Trav` value per op (specs in B.1–B.4). Per-kind
specialization threading is **unchanged and is the cleanest part**: `dispatchA`/`dispatchB`
(382–385) stay exactly as they are. Each op's surface impl becomes
`dispatchA(k => lower(spec(k)))` (or `dispatchA(ka => dispatchB(kb => lower(spec(ka, kb))))`
for `map`/`scanLeft`). The `summonFrom` resolves to **one** kind at each concrete call site, so
the inlined walk is **monomorphic per site** — no megamorphism is even possible. *That is why*
inlining the walk wins where the shared `dfsC` driver couldn't: the driver was shared across all
`${K}Dfs` subclasses ⇒ its `onRunF` callsite was megamorphic.

---

## (E) Feasibility verdict and migration path

**Verdict: feasible, high-value, and ~80% mechanical — because `dfsBody` is already the
parameterized walker and `dispatchA/B` already monomorphizes.** The redesign is "inline the
walker the generator already builds, fill its holes with op bodies instead of `c.onRunF`, and
delete the surface guard ladder." The two non-mechanical pieces (ReverseNode-over-tree,
deep-base Slice/Pad/Updated) already have an accepted fallback (`materialize` one sub-node), so
they don't block conversion.

**Migration order** — convert in waves; each wave is independently shippable and
benchmarkable:

1. **Wave 1 — scalar folds, no output, no early exit:** `foldLeft`, `foldRight`, `foreach`,
   `count`, `sum`, `product`. Pure `lower` with `Out.Scalar`/`Out.NoOut`, `Exit.Never`. Lowest
   risk; immediately deletes their `n==0/1` guards (811, 812, 814, 815, 850, 851) and their
   `${K}Dfs` allocations (387, 390, 433, 436, 442, 473). Validate inlined-walk parity vs current
   on the JMH suite before proceeding.
2. **Wave 2 — short-circuit family:** `exists`, `forall`, `find`, `indexWhere`, `indexOf`,
   `collectFirst`, `prefixLength`, `contains`, `foreachWhile`, `lastIndexWhere`, `lastIndexOf`.
   All `Exit.OnPredInCond`. This is where the `boundary`-vs-`return` decision is validated, and
   where the empty-body shape must be **confirmed preserved** — re-run the exact scan benchmarks
   (this is the family the memory note warns about). `lastIndexWhere`/`lastIndexOf` are just
   `.copy(dir = Bwd)` of their forward twins.
3. **Wave 3 — array-building ops:** `map`, `filter`, `scanLeft`, `mapConserve`.
   `Out.ResultFArray`; reuse `leaf(k,arr,len)` (370) for the epilogue so the Empty/One invariant
   is upheld by construction. `filter` keeps its `if (o==n) xs` identity shortcut in the
   epilogue.
4. **Wave 4 — iterator** via `lowerCursor` (the pull emitter), removing the `materialize` in
   `iteratorV` (466) and the hand-written `cursorClass` (236).
5. **Leave alone (Wave never):** `flatMap`, `zip`/`unzip`/`zipWithIndex`, `sort*`,
   `groupBy`/`groupMap`, `toArray`/`copyToArray`. See (F.5).

**How the scattered 0/1/arr matches get removed:** each converted op's star-Impl drops its
`{ val n = xs.length; if (n==0)… else if (n==1)… else …}` wrapper and becomes
`inline def fooImpl(...) = dispatchA(k => lower(fooSpec(k)))`. The `Empty`/`One` arms inside the
lowered walk + the `Out` epilogue cover the small cases. The third match — `applyAtImpl` as the
n==1 fast path — is removed wherever it was *only* serving the n==1 guard; it remains for
genuine random access `apply(i)` (841, 496).

**Interplay with the Empty/One canonicalization invariant:** the invariant is what makes this
safe — it guarantees the small cases are *already nodes the arm table handles*, so deleting the
guards loses nothing. The output side must *uphold* it: every `Out.ResultFArray.finalize` routes
through `leaf(k,arr,len)`, so produced results stay canonical. This is the **only** correctness
coupling, and it is a hard invariant the lowering must respect.

---

## (F) Ranked risks and where it does NOT pay

1. **Code size / JIT method-size limit (highest risk).** Inlining the full arm walk into
   *every* op call site multiplies bytecode: the walk is ~10 arms × per-element body, landing
   at every `inline def …Impl` use site. The shared `dfsBody`/`dfsC` exists today *specifically*
   so the tree-walk is compiled once. **Mitigations, in order:**
   * (i) Keep the per-element/run *body* tiny — it splices the already-`inline` user lambda, so
     no growth there; the arm skeleton is the bulk.
   * (ii) **Hot/cold arm split.** For ops where the walk dwarfs the body, emit the **cold** arms
     (`Pad`/`Updated`/`Reverse`/deep-`Slice`) as calls to **shared non-inline per-kind helpers**,
     inlining only the **hot** arms (`leaf`/`One`/`Concat`/`Append`/`Prepend`/leaf-`Slice`/
     `Range`) where the win is. This bounds bytecode where size hurts while keeping speed where
     it matters.
   * (iii) Gate inlining by measuring `-Xmax-inlines` / method size on the JMH classes.
   This is the **main reason to convert in waves and benchmark each.** If `map`'s inlined walk
   regresses from method-size bloat, fall back to hot/cold for that op.
2. **The short-circuit shape regressing (high impact, well-understood).** The empty-body
   predicate-in-condition shape is load-bearing (**1.45×** measured). `Exit.OnPredInCond`
   preserves it *by construction*, and the `boundary`-not-flag decision keeps the loop body
   empty. **Mitigation:** Wave 2 re-runs the exact scan benchmarks; if `boundary` adds overhead
   vs today's `if (c.stop) return`, fall back to a **per-run** (not per-element) flag re-test,
   which the memory note already shows is fine (the cost is mid-*loop* writes, not after-loop
   checks).
3. **Maintainability / two emitters drift.** `lower` (push) and `lowerCursor` (pull) both
   consume `dfsBody`'s arm list; if someone adds a node kind, both must handle it.
   **Mitigation:** the arm list stays *only* in `dfsBody`; both emitters get arms by filling
   holes, so a new node is added once in `dfsBody` and both emitters inherit it. The residual
   risk is the cold-path fallback (materialize) needing per-emitter handling — keep that list
   explicit and tested.
4. **Compile time of the generator's output.** More inlined code ⇒ longer Scala compile of the
   generated `FArrayOps`. Likely modest (the expansions are mechanical) but measurable.
   **Mitigation:** the hot/cold split (risk 1) also bounds this; benchmark full-build time after
   Wave 3.
5. **Where it does NOT pay — leave as shared / non-inline:**
   * **`flatMap`, `zip`, `unzip`, `sort`, `groupBy`/`groupMap`, `toArray`/`copyToArray`:** these
     already use `materialize`/`flatMapCopyOne`/`sortInt`/the group buffers as *deliberately
     non-inline* helpers (the comments say so: "compiled once, not dumped at every site, so
     multiple sorts can't blow past the JIT method-size limit"). Their cost is dominated by the
     array build / merge / hashmap — **not** per-element dispatch — so inlining the walk buys
     little and risks the size limit most. **Decisive: do not convert these.** They are the
     *correct* place for a shared walker.
   * **Ops where `n` is tiny or the body is heavy** (`collectFirst` with a `PartialFunction`,
     `groupBy` with a `HashMap`): the dispatch is already amortized into the heavier per-element
     work; inlining the walk is noise. Convert only if a benchmark shows the consumer allocation
     matters.

---

## Bottom line

The abstraction is real and the codebase is already 80% shaped for it — `dfsBody`'s hole-based
design **is** the traversal AST in disguise, and `dispatchA/B` already guarantees the
monomorphism that makes inlining the walk a win rather than a megamorphic loss. Reify the holes
+ direction + accumulator + exit + output as a `Trav` case class; write one `lower` (push) and
one `lowerCursor` (pull) emitter over the existing arm table; delete the `${K}Dfs`/`dfsC`
consumer machinery (213–231), the hand-written `${K}Cursor` (236–245), and the scattered
`n==0/1` guards (811–852) for the converted ops; and keep the heavy array-building ops on the
shared non-inline helpers. Convert in five waves, benchmarking each, with a hot/cold arm split
held in reserve as the mitigation for the one genuine risk (code size).

### Source map (GenCores.scala)

| Lines | What | Fate |
|---|---|---|
| 134–209 | `dfsBody` — the arm table / traversal order | **KEEP**, invoke at lowering time |
| 213–214 | `dfsConsumer` — `${K}Dfs` heap consumer | **DELETE** per converted op |
| 215–231 | `dfsCDef` — non-inline megamorphic `dfsC` driver pair | **DELETE** per converted op |
| 236–245 | `cursorClass` — flat-array `${K}Cursor` | **REPLACE** with `lowerCursor` (§C) |
| 289 | `materialize${k}` | **REUSE** on the cold path (B.4 / §C) |
| 370–371 | `leaf(k,arr,len)` — Empty/One canonicalizer | **REUSE** in every `Out` epilogue |
| 376–381 | `runMethods` — full-run loop | the shape `lower` picks for `Exit.Never` |
| 399–403 | `scanB` innards — empty-body pred-in-cond loop | the shape `lower` picks for `Exit.OnPredInCond` |
| 382–385 | `dispatchA`/`dispatchB` — kind threading | **UNCHANGED** |
| 387–495 | the op specs (`foldLeft`/`map`/`filter`/`exists`/…) | **REWRITE** as `Trav` values |
| 465–467 | `iteratorV` — materialize + cursor | **REPLACE** with `lowerCursor` |
| 811–852 | star-Impls with `n==0/1` ladders | **DROP** the guards; delegate to `lower(...)` |
