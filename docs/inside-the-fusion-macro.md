# One loop, no closures: inside FArray's fusion macro

*For people who have spent a decade typing `.view` and then squinting at flight recordings wondering where all the `Function1`s came from.*

You know the pitch for lazy collections. `xs.view.map(f).filter(p).map(g).toArray` is supposed to be a single pass with no intermediate collections. And it is — *one* of the three promises is kept. The other two leak everywhere:

- **It still allocates per element.** Every `map`/`filter` on a `View` (or `Iterator`, or `LazyList`) is a stored `Function1`. Your `_ + 1` is a heap object; calling it is an `INVOKEINTERFACE` through `Function1.apply$mcII$sp` if you're lucky and `apply(Object): Object` with boxing if you're not. The view nodes themselves are objects. `LazyList` additionally memoizes every cell.
- **It is megamorphic.** One `View.Map` class handles every `map` in your program. The JIT sees a call site that has witnessed forty different lambdas and gives up on inlining. Your tight numeric loop becomes a sequence of virtual calls it can't see through.

So the "no intermediate collections" win is real and the "fast" part is a lie. You reach for views to avoid allocating arrays and you get a different pile of garbage instead.

`FArray` is a high-performance Scala 3 array type (a flat primitive array with an O(1) tree spine for cheap `concat`/`slice`). This post is about its `fuse` pipeline: `xs.fuse.map(...).filter(...).<terminal>`. It is a **macro**, not a runtime data structure. It reads the *entire* chain off the typed AST at compile time and emits one specialized `while` loop with the lambdas inlined into the body. No view nodes. No `Function1`. No boxing. No megamorphism — every call site is monomorphic because there are no calls.

Here is the whole thesis in one example. Input:

```scala
xs.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toFArray
```

Generated code (lightly de-sugared from the snapshot test — `a.+(b)` rendered as `a + b`, etc. — but otherwise verbatim):

```scala
val src0 = xs                              // the FArray as its sealed `FBase` core
val cap  = src0.length
val out  = new Array[Int](cap)             // sized to the source — this pipeline can only shrink
var o    = 0
val s    = src0; val len = s.length
if (s.isInstanceOf[IntArr]) {              // leaf fast-path: a flat int[] under the hood
  val a = s.asInstanceOf[IntArr].arr
  var i = 0
  while (i < len) {
    val v  = a(i)                          // unboxed array read
    val v₂ = v + 1                         // map(_ + 1)
    val v₃ = v₂ % 2
    val v₄ = v₃ == 0                       // filter(_ % 2 == 0)
    if (v₄) {
      val v₅ = v₂ * 2                      // map(_ * 2)
      out(o) = v₅; o += 1
    }
    i += 1
  }
} else { /* identical loop using FArrayOps.intAt(s, i) for tree-shaped sources */ }
if (o == 0) IntArr.EMPTY
else if (o == cap) new IntArr(out, o)
else new IntArr(java.util.Arrays.copyOf(out, o), o)
```

Three `map`/`filter` stages, one `int[]` scan, three `val`s and an `if`. No `Function1` was allocated; `_ + 1` became the bytecode `iadd`. That is the floor we are building on. The rest of this post is about everything that goes *wrong* when you try to make that floor hold up under real pipelines — name capture, deep nesting, predicates at five different levels, runtime values, short-circuiting across loop boundaries — and how the macro keeps the floor flat anyway.

---

## How it reads the chain

The combinators are **markers**. `Fuse#map`, `#filter`, etc. have bodies that just return `this`; they never run. A terminal like `toFArray` is an `inline def` whose macro receives the `Expr` of the *whole* receiver — `xs.fuse.map(f).filter(p).map(g)` — and walks it backwards (`Apply(Select(prev, "map"), List(f))` → recurse on `prev`) until it hits `new Fuse(src)`, collecting a `List[Stage]` on the way.

If you dump the raw expansion you see two parts. There's a preamble that reconstructs the marker chain as typed trees:

```scala
val Fuse_this: Fuse[Int] =
  new Fuse[Int](xs).map(_ + 1).filter(_ % 2 == 0).map(_ * 2)
```

…and then the actual loop in a separate block. The preamble is **dead** — nothing reads `Fuse_this` — so `-opt` deletes it and the benchmarks confirm zero allocation for it. It exists only because the macro needs the typed trees to peel; once peeled, the wrapper evaporates. The lambdas you wrote (`_ + 1`) are extracted as `Term`s and `betaReduce`d straight into the loop body. They are never reified as functions.

That "betaReduce into the body" is where the first hard problem lives.

---

## 1. Name conflicts (hygiene)

Inline a lambda's body into a loop and you are textually substituting code that came from somewhere else into a scope full of other people's bindings. Do it naively and you get capture. Watch what happens when three stages all call their parameter `x`:

```scala
xs.fuse.map(x => x + 1).filter(x => x > 2).map(x => x * x).toFArray
```

```scala
while (i < len) {
  val v  = a(i)
  val v₂ = v + 1                 // first  x  ⇒ v₂
  val v₃ = v₂ > 2               // second x  ⇒ reads v₂, not v
  if (v₃) {
    val v₄ = v₂ * v₂            // third  x  ⇒ reads v₂, squares it
    out(o) = v₄; o += 1
  }
  i += 1
}
```

There are three `x`s in the source and zero `x`s in the output. Each one became a distinct, freshly-named binding. The subscripts (`v₂`, `v₃`, `v₄`) are not something the macro invents — they are `quotes.reflect`'s pretty-printer disambiguating several distinct *symbols* that happen to share the human name `v`. The macro never builds names by string-concatenation; it asks the quotation machinery for fresh symbols, and binds every intermediate it materializes (`letBind`) so that a value computed once is a `val`, referenced by symbol identity, not recomputed and not shadowed.

The subtle part is that the second `x` resolves to `v₂` (the result of `map(x => x + 1)`), *not* the original element `v`. The macro tracks "what is the current element value at this point in the pipeline" as a `Term`, threaded through the lowering; when it reduces `filter`'s `x => x > 2`, the `x` is substituted with that `Term`. Stage boundaries are just function composition on `Term`s, and `Term`s carry their binding structure with them.

It gets more interesting across a `flatMap`, where the same name lives at two different loop depths:

```scala
xs.fuse.flatMap(x => FArray(x, x * 10)).map(x => x + 1).toFArray
```

```scala
while (i < len) {
  val v  = a(i)                       // outer x
  val s₂ = /* FArray(v, v * 10) built inline */
  val len₂ = s₂.length
  if (s₂.isInstanceOf[IntArr]) {
    val a₂ = s₂.asInstanceOf[IntArr].arr
    var i₂ = 0
    while (i₂ < len₂) {
      val v₂ = a₂(i₂)                  // inner element
      val v₃ = v₂ + 1                  // the *map*'s x ⇒ binds the INNER value
      if (o >= out₂.length) out₂ = ensureCapInt(out₂, o + 1)
      out₂(o) = v₃; o += 1
      i₂ += 1
    }
  } else { /* …intAt fallback… */ }
  i += 1
}
```

The outer `x` is `v`; the `flatMap` opens a nested loop; the downstream `map`'s `x` correctly binds `v₂`, the *inner* element, at the inner depth. Two `x`s, two loop levels, two symbols (`v` and `v₂`), and the loop counters `i`/`i₂`, source handles `s`/`s₂`, lengths `len`/`len₂` are all independently fresh. There is no scope in which two of these collide, because none of them were ever named by hand.

---

## 2. Predicates at every level

A "filter" in this world is not a stage that produces a filtered collection; it's an `if` that wraps everything downstream of it. That compositional definition is what makes predicates stack cleanly no matter where they appear.

**Three filters become three nested guards in one pass** — not three passes, not three intermediate arrays:

```scala
xs.fuse.filter(_ > 1).filter(_ < 7).filter(_ % 2 == 0).toFArray
```

```scala
while (i < len) {
  val v = a(i)
  if (v > 1) {
    if (v < 7) {
      if ((v % 2) == 0) {
        out(o) = v; o += 1
      }
    }
  }
  i += 1
}
```

Short-circuiting falls out for free: if `v > 1` fails, the JVM never evaluates `v < 7`. A `View` would have three `Filtered` nodes each doing an interface call per element; here it's three `if_icmp` branches the JIT can predict.

Now mix levels. Put a filter *before* a `flatMap`, and another *inside* it:

```scala
xs.fuse.filter(_ > 0).flatMap(x => FArray(x, -x)).filter(_ % 2 == 0).map(_ + 1).toFArray
```

```scala
while (i < len) {
  val v = a(i)
  if (v > 0) {                          // OUTER filter — guards whether we even expand
    val s₂ = /* FArray(v, -v) built inline */
    val len₂ = s₂.length
    if (s₂.isInstanceOf[IntArr]) {
      val a₂ = s₂.asInstanceOf[IntArr].arr
      var i₂ = 0
      while (i₂ < len₂) {
        val v₃ = a₂(i₂)
        if ((v₃ % 2) == 0) {            // INNER filter — runs per produced element
          val v₆ = v₃ + 1
          if (o >= out₂.length) out₂ = ensureCapInt(out₂, o + 1)
          out₂(o) = v₆; o += 1
        }
        i₂ += 1
      }
    } else { /* …intAt fallback… */ }
  }
  i += 1
}
```

The outer predicate sits at the outer loop depth and decides whether the inner loop runs at all; the inner predicate sits inside the inner loop and runs per produced element. They're the same combinator (`filter`) lowered at two depths, and the nesting structure of the generated `if`s mirrors the data dependency exactly. There is no place in a lazy-collection design where you can *see* this — the inner and outer filters are both just `Filtered` nodes in a flat chain, and which loop they end up "in" is an emergent property of the iterator protocol you can't inspect or optimize.

`takeWhile` is a predicate at yet another level — it doesn't gate one element, it ends the stream:

```scala
xs.fuse.takeWhile(_ < 5).toFArray
//  while (i < len && !done) { val v = a(i); if (v < 5) { out(o)=v; o+=1 } else done = true; i += 1 }
```

…which brings us to `done`, the mechanism behind all short-circuiting, covered under Dynamism below.

---

## 3. The applicative core: compute-for-survivors, and dead code that's actually dead

Here is the thing lazy collections *fundamentally cannot do*, and the reason this project exists. Consider:

```scala
xs.fuse.map(x => (x % 3, x * 7, x * 13)).filter(_._1 == 0).map(_._2).toFArray
```

A `View` builds a 3-tuple per element (heap allocation + three boxed `Integer`s), runs the filter, then projects. Every `x * 13` is computed and immediately thrown away. Every tuple is allocated and immediately discarded. Now the macro:

```scala
while (i < len) {
  val v  = a(i)
  val v₂ = v % 3                 // column 0 — the filter needs it, so it's computed eagerly
  if (v₂ == 0) {                 // filter(_._1 == 0)
    val v₄ = v * 7               // column 1 — computed INSIDE the guard: only for survivors
    out(o) = v₄; o += 1
  }
  i += 1
}
```

Read that carefully. The tuple `(x % 3, x * 7, x * 13)` is **never built**. Column 0 (`x % 3`) is computed before the filter because the filter reads it. Column 1 (`x * 7`) is computed *inside the `if`* — it is only needed by the final `map(_._2)`, which is downstream of the filter, so it runs **only for elements that survive the filter**. Column 2 (`x * 13`) does not appear in the output *at all* — nothing downstream reads `_._3`, so it is dead, and dead means gone.

This isn't peephole cleanup hoping the JIT notices. The macro models a `map` body that constructs a product as a set of **independent lazy columns** (`Shape = Sc(scalar) | Tup(parts, rebuild)`). A `Tup` is not a value; it's a promise to produce N values. Each column is a CPS thunk that binds itself to a `val` on first read and memoizes. So:

- a column read only inside a guard emits its binding inside that guard → **compute-for-survivors**;
- a column never read emits nothing → **dead-column elimination**;
- a sub-expression shared between columns is hash-consed and bound once → **CSE**.

The product is only ever `rebuild`-materialized if something uses it *whole* (passed to an opaque function, compared with `==`, emitted as the output element). Projections (`._1`, `._2`) and the matching field reads of case classes are routed to columns; they never force the tuple.

CSE, concretely — `f(x)` written twice, computed once:

```scala
xs.fuse.map(x => (x*x + 1, x*x + 2)).map(t => t._1 + t._2).toFArray
```

```scala
while (i < len) {
  val v  = a(i)
  val v₂ = v * v                // x*x — hash-consed across both tuple components, bound ONCE
  val v₃ = v₂ + 1
  val v₄ = v₂ + 2
  val v₅ = v₃ + v₄
  out(o) = v₅; o += 1
}
```

This generalizes through arbitrary products, including **nested** ones, and the DCE reaches all the way down. `((x, x+1), (x+2, x+3))` keeping only the outer-left and inner-right leaves:

```scala
xs.fuse.map(x => ((x, x + 1), (x + 2, x + 3))).map(t => t._1._1 + t._2._2).toFArray
```

```scala
while (i < len) {
  val v  = a(i)
  val v₂ = v + 3                // t._2._2  =  x + 3
  val v₃ = v + v₂              // t._1._1 + t._2._2  =  x + (x + 3)
  out(o) = v₃; o += 1
}
```

Four leaves in the source; `x + 1` and `x + 2` are dead and absent; two nested pairs allocated by a `View`, zero allocated here. The decomposition is driven by `Symbol.caseFields` — Scala 3's reflective view of `Mirror.ProductOf` — so it works uniformly for tuples, case classes, and named tuples, monomorphic or generic.

The independence is *read off your syntax*, not inferred by dataflow analysis. When you write a tuple, you are declaring "here are independent values"; when you write `._2`, you are declaring "I need this one." The macro just honors those declarations and binds lazily. Anything it doesn't structurally recognize (a method call, a named function) becomes one opaque memoized column — correct, just not decomposed. The worst case is "as good as a hand-written loop that computes the whole thing"; the common case is dramatically better.

---

## 4. Deep nesting

`flatMap` opens a real nested loop, and everything downstream lives inside it. Two `flatMap`s nest two levels deep:

```scala
xs.fuse.flatMap(x => FArray(x, x + 1)).flatMap(y => FArray(y * 100)).toFArray
```

Structurally (eliding the `IntArr`/tree dual paths and the inline `FArray(...)` construction):

```scala
while (i < len) {                         // over xs
  val v  = a(i)
  val s₂ = FArray(v, v + 1)
  var i₂ = 0
  while (i₂ < s₂.length) {                 // over FArray(v, v+1)
    val v₂ = s₂.arr(i₂)
    val s₃ = FArray(v₂ * 100)
    var i₃ = 0
    while (i₃ < s₃.length) {               // over FArray(v₂ * 100)
      val v₃ = s₃.arr(i₃)
      if (o >= out₃.length) out₃ = ensureCapInt(out₃, o + 1)
      out₃(o) = v₃; o += 1
      i₃ += 1
    }
    i₂ += 1
  }
  i += 1
}
```

Three loop levels, each with its own fresh counter/handle/length, each with the leaf-vs-tree fast path, all writing into one growable output. The lambdas `x => …` and `y => …` are inlined at their respective depths. A chain of `flatMap`s on `Iterator` would be a tower of `flatMap` iterators each calling `hasNext`/`next` through an interface, boxing at every level. Here it's a plain loop nest the JIT can unroll and vectorize.

---

## 5. Dynamism

"Dynamism" is everything that isn't known at compile time: runtime limits, data-dependent stopping, output sizes you can't predict. The macro handles each with a small piece of generated machinery.

**Runtime limits.** `take(k)` where `k` is a value, not a literal:

```scala
xs.fuse.filter(_ % 2 == 0).take(k).toFArray
```

```scala
var done = false
val lim  = { val t = k; if (t < 0) 0 else t }     // clamp the runtime limit once, above the loop
var c    = 0
val cap  = java.lang.Math.min(n0, lim)             // output upper bound = min(source, limit)
val out  = new Array[Int](cap)
…
while (i < len && !done) {
  val v = a(i)
  if ((v % 2) == 0) {
    val cv = c
    if (cv >= lim) done = true
    else { out(o) = v; o += 1; c += 1; if (cv + 1 >= lim) done = true }
  }
  i += 1
}
```

`k` is read once, clamped, and the output is preallocated to `min(sourceLength, k)` — no growth, no waste — because the macro can prove a `take` only shrinks. The counter `c` advances only past the filter (positions are post-filtering, matching `List`), and `done` flips the instant the quota is hit.

**`done` short-circuits across loop boundaries.** This is the part that's genuinely annoying to get right by hand. `done` is threaded onto *every* loop condition, so a limit reached deep inside a nested loop unwinds all of them:

```scala
xs.fuse.flatMap(x => FArray(x, x, x)).take(5).toFArray
```

```scala
while (i < len && !done) {                         // outer
  val v = a(i)
  val s₂ = FArray(v, v, v)
  while (i₂ < len₂ && !done) {                      // inner — SAME done flag
    val v₂ = s₂.arr(i₂)
    val cv = c
    if (cv >= lim) done = true
    else { out₂(o) = v₂; o += 1; c += 1; if (cv + 1 >= lim) done = true }
    i₂ += 1
  }
  i += 1
}
```

Once the fifth element is written, `done` is `true`, the inner `while`'s `&& !done` fails on the next check, the inner loop exits, and the outer `while`'s `&& !done` fails too. The source is read no further than necessary — `xs.fuse.flatMap(huge).take(5)` touches `huge` exactly enough to produce 5. The same flag powers `find`, `head`, `exists`, `forall`, `takeWhile`. Every short-circuit terminal is one boolean.

**Unpredictable output size.** A `flatMap` or `scanLeft` can produce more than the source length, so the output array grows via `ensureCap` (amortized doubling) instead of being preallocated — you saw `if (o >= out.length) out = ensureCapInt(out, o + 1)` in the nests above. The macro picks preallocate-and-trim vs. grow based on whether any stage can expand.

**`scanLeft`** is the most interesting bit of dynamism, because it emits one *more* element than its input — the seed, even for empty input. That seed can't be emitted inside the loop (the loop might run zero times), so the macro emits a **prologue**:

```scala
xs.fuse.scanLeft(0)(_ + _).toFArray
```

```scala
var acc0 = 0
var out  = new Array[Int](math.max(8, n0))
var o    = 0
out(o) = acc0; o += 1               // PROLOGUE: emit the seed once, before the loop
while (i < len) {
  val v = a(i)
  acc0 = acc0 + v                   // running fold in an above-loop var
  out(o) = acc0; o += 1            // emit each accumulator
  i += 1
}
```

The accumulator is a single mutable `var` above the loop (not an `Option`, not a fold object), the seed is emitted by a prologue that runs for every terminal — `count` counts it, `last` sees it, `sum` adds it — and empty input correctly yields `[0]`.

---

## The `collect` aside: inlining a `PartialFunction`'s match

One more, because it surprised me. `collect` takes a `PartialFunction`, which is normally a heap object with `isDefinedAt` and `applyOrElse` — two virtual calls and (in the usual `filter(isDefinedAt).map(apply)` encoding) the pattern match evaluated *twice* per survivor. The macro instead reaches into the PF literal's AST. A Scala 3 `{ case … }` is `Block(DefDef($anonfun, x$1 => x$1 match { … }), Closure(_, PartialFunction))` — a closure over an `$anonfun` whose body is the match. The macro lifts that match out and splices it into the loop with `cur` as the scrutinee:

```scala
xs.fuse.collect { case x if x % 2 == 0 => x * 10 }.toFArray
```

```scala
while (i < len) {
  val v = a(i)
  v match {
    case x if (x % 2) == 0 => out(o) = x * 10; o += 1   // your case → emit
    case _ => ()                                          // synthesized skip (the match is partial)
  }
  i += 1
}
```

No `PartialFunction` object, no `isDefinedAt`, one evaluation of the match. And because the case body goes through the same `Shape` machinery as a `map` body, `collect { case … => (a, expensive(b)) }.map(_._1)` gets DCE on the discarded column too.

---

## Why this is the thing you actually wanted

Lazy collections sold you fusion and delivered a different allocation profile. This delivers the fusion:

- **No `Function1`, ever.** Lambdas are beta-reduced into the loop body. `_ + 1` is `iadd`.
- **No intermediate collections, and no per-stage nodes.** One output array.
- **No boxing.** Specialized over `Int`/`Long`/`Double` with a leaf `int[]`/`long[]`/`double[]` fast path; reference types use a typed `T[]`.
- **Monomorphic by construction.** There are no calls to be megamorphic. The JIT sees one flat loop.
- **Compute-for-survivors and dead-column elimination** — work the lazy model literally cannot express, because it has no idea which columns a downstream stage will read.

The measured wins (FArray→FArray, size 1000): the basic `map/filter/map` runs ~1.7× a 3-pass eager version with −50% allocation; an expensive column behind a selective filter is **6.5×**; a dead column is **28×**; a dead zipped side is **12×** — because the fast version simply does not do the work.

There are two things you give up, and they're the honest cost. First, **purity is assumed**: a fused stage may run fewer times than the strict `List` equivalent (it computes for survivors, short-circuits, eliminates dead columns), so a side-effecting or throwing lambda can observe a different call count. For pure functions the result is identical — and it's checked: a fuzz harness runs thousands of random pipelines against the `List` reference. Second, **inlining has a size budget**: a long enough chain could blow the JVM's `HugeMethodLimit` (~8000 bytecodes) and fall back to the interpreter. So there's a regression test that parses the `.class` file and asserts a 13-stage chain stays at ~700 bytecodes, fully inlined.

Every code block above was extracted from a snapshot test — a golden file, checked into git, that pretty-prints the post-typer expansion of each pipeline. When the lowering changes, the diff is right there in the PR. The generated code is not a thing we hope the compiler produces; it's a thing we assert it produces, on every build.

It turns out the loop you would have written by hand was always available. You just needed a macro patient enough to read your whole pipeline before emitting a single instruction — and stubborn enough to refuse to allocate a closure on the way.
