# FArray

**An immutable, covariant Scala 3 sequence that never boxes — with the Scala collections API you already know, at the speed of a raw array.**

```scala
val xs: FArray[Int] = FArray(1, 2, 3, 4, 5)
xs.map(_ * 2).filter(_ > 4).foldLeft(0)(_ + _)   // same API you'd write for List…
//                                               // …but it's a real int[] underneath. No Integer. Ever.
```

`List[Int]` and `Vector[Int]` box **every single element** into a heap `java.lang.Integer`. `Array[Int]` doesn't — but it's mutable, invariant, and has a thin, awkward API. `FArray` gives you the best of both: a proper immutable, covariant collection with the full familiar API, backed by a genuine unboxed primitive array.

---

## Why you'll want it

### 🚫 Zero boxing — primitives stay primitives, end to end
An `FArray[Int]` is an `int[]`. An `FArray[Long]` is a `long[]`. `map`, `filter`, `fold`, `sum`, `sort` — none of them ever allocate an `Integer`. The whole library is **specialized per primitive type** (`Int`/`Long`/`Double`/reference) by a code generator, so there's no boxing on any path and no megamorphic virtual dispatch. That death-by-a-thousand-paper-cuts you see when a Scala program leans on collections in a profiler? It just disappears.

### ⚡ As fast as `Array`, and it crushes `List`/`Vector`
Every combinator is a hand-written, fully-inlined `while` loop over a contiguous array — the fastest thing the JVM can run — with excellent memory locality. Structural operations (`++`, `:+`, `reverse`, `slice`, `take`, `drop`) are **O(1) lazy tree nodes**, not copies, so they don't touch the data until you read it.

Measured with JMH against the alternatives (full suite — run it yourself, see below):

| operation | vs `Array` / `IArray` | vs `List` / `Vector` |
|---|---|---|
| `++` · `concat` · `reverse` · `slice` · `take` · `drop` | lazy, ~free | **10–845× faster** |
| `flatMap` | **beats** `Array` (Int), beats `IArray` (String) | 2–5× faster |
| `sort` — `sortWith` · `sortBy` · `sorted` | **beats `IArray` on every op**, Int *and* String | 1.5–6× faster |
| `map` · `filter` · `foldLeft` | matches `IArray` | 1.3–3× faster |
| `unzip` · `mapConserve` | — | 3–9× faster |

Across the *entire* operation suite, on a mix of large and small sequences, `FArray` is the fastest of the bunch — ahead of both `List` and `IArray`.

### 🧩 The same API as Scala collections — drop-in, no learning curve
`map`, `flatMap`, `filter`, `filterNot`, `collect`, `collectFirst`, `foldLeft`/`foldRight`, `reduce`, `groupBy`, `groupMap`, `partition`, `partitionMap`, `zip`, `zipWithIndex`, `unzip`/`unzip3`, `sortBy`/`sortWith`/`sorted`, `takeWhile`/`dropWhile`/`span`/`splitAt`, `distinct`/`distinctBy`, `mkString`, `++`, `:+`, `+:`, `head`/`last`/`tail`/`init`, `exists`/`forall`/`find`/`indexOf`, `min`/`max`/`minBy`/`maxBy`, `transpose`, `padTo`, `updated` … it's all there, with the signatures you expect. Short-circuiting ops short-circuit. `xs.hashCode == xs.toList.hashCode`, so it interops cleanly with anything that hashes sequences.

If you can write it for `List`, you can write it for `FArray` — and your hot paths get dramatically faster for free.

### 🌳 The persistence of `List`/`Vector`, without their cost
`FArray` is immutable and **persistent** with structural sharing, just like `List` and `Vector`: `xs :+ y`, `a ++ b`, `xs.updated(i, v)`, `xs.reverse` all return a new `FArray` that **shares** the original's storage and copies nothing. So you get the same value-semantics and cheap "modification" you'd reach for `Vector` for — except the data lives in flat, contiguous, unboxed arrays with perfect cache locality, instead of `List`'s pointer-chasing cons cells or `Vector`'s 32-way boxed trie. That's why it has their ergonomics while running 10–845× faster on those very operations.

---

## Under the hood

### The tree
An `FArray[+A]` is a zero-overhead **opaque type** over `FBase`, a small sealed tree:

- **Leaves** hold the data: `IntArr` / `LongArr` / `DoubleArr` / … / `RefArr` — each a genuine flat primitive array (`int[]`, `long[]`, …) plus a length. The overwhelmingly common case; an `FArray[Int]` that came from `tabulate`/`map`/etc. *is* one of these, i.e. a real `int[]`.
- **Lazy inner nodes**, each O(1) to build and sharing their children's storage:
  `Concat(l, r)` · `Append(base, elem)` · `Prepend(elem, base)` · `ReverseNode(base)` · `SliceNode(base, off, len)` · `Pad(base, filler, len)` · `Updated(base, i, elem)` · `RangeNode(start, step, count)`.

So `a ++ b` is just a `Concat` pointing at both arrays; `xs.reverse` is a `ReverseNode`; `FArray.range(0, 1_000_000)` is a single `RangeNode` (it hashes in O(1) and never allocates the elements). Nothing is copied or computed until you **read** the data, at which point a single specialized **unboxed traversal** walks the tree and hands each leaf's backing array to the consumer in bulk (`System.arraycopy` where possible). Materialization to a flat leaf happens on demand (e.g. before a sort).

### Compile-time specialization: `inline` + type-matching
The library is specialized per element kind by a `Repr[A]` typeclass — `IntRepr`, `LongRepr`, `DoubleRepr`, `RefRepr`. Every combinator is `inline` and dispatches with Scala 3's `summonFrom`:

```scala
inline def mapImpl[A, B](xs: FBase)(inline f: A => B): FBase =
  summonFrom {
    case r: IntRepr[A]    => /* tight while-loop over int[]    */
    case r: LongRepr[A]   => /* tight while-loop over long[]   */
    case r: DoubleRepr[A] => /* … */
    case r: RefRepr[A]    => /* … */
  }
```

`summonFrom` is resolved **at compile time**. At a concrete call site where `A` is known — say `FArray[Int]#map` — the compiler picks exactly the `IntRepr` branch, drops the other three entirely, and inlines just that unboxed loop. The lambda `f` is an `inline` parameter, so it's spliced straight into the loop body — no closure allocation, no virtual `apply`. The net effect: `xs.map(_ + 1)` on an `FArray[Int]` compiles, *at that line*, to a bare `while` loop over an `int[]` writing into a fresh `int[]` — byte-for-byte what you'd hand-write, with zero dispatch and zero boxing.

### The Java/Scala split
The **data** is generated **Java** — the `FBase` hierarchy and each leaf/node are compact `final` Java classes holding real primitive-array fields. The **operations** are generated **Scala**: the `Repr` typeclasses, the `inline` + `summonFrom` combinators, and the per-kind tree walk (`dfsC<Kind>`) — which is a single *non-inline* method **compiled once**, so the heavy traversal lives in exactly one place and never bloats your call sites or trips the JVM's method-size limit. On top of that sits a thin layer of hand-written Scala: the `FArray` opaque type and its extension methods, which just forward to the generated ops. You write idiomatic Scala; underneath it's flat Java arrays and one tight loop. All of it — Java data classes and Scala ops — is emitted from a single generator in `codegen/`.

### Adaptive, unboxed sort
Sorting is done directly on the materialized primitive array with a run-detecting (natural) mergesort — no boxed indices, no intermediate `Vector`. Already-sorted or reverse-sorted input is O(n); references use Java's TimSort. It beats `IArray` on `sortWith`/`sortBy`/`sorted` for both primitives and references.

## Compromises (the honest part)

- **Single-element random access** (`apply(i)`, `head`, `last`) carries one type-check that a bare array doesn't — because `FArray` is a sum type, it must confirm it's looking at a leaf before indexing. It's ~0.3 ns (these run at ~1.3 *billion* ops/s, ~0.65× of a raw `Array`), so it's never a real bottleneck — but if your workload is dominated by scattered single-element reads rather than bulk traversal, a raw `Array` edges it.
- **Deep linear node chains.** Building a huge sequence by *thousands of individual* `:+`/`+:`/`updated` calls without materializing grows a deep tree, which makes random access O(depth) and traversal a little heavier. The library materializes when it pays to; for that pattern, materialize once (or use bulk `++`).
- **Short-circuit ops** (`exists`/`find`/`indexWhere`) pay a small per-call sum-type dispatch (~1.2× a raw array loop) — inherent to a tree representation, and still hundreds of millions of ops/s.
- **References don't get the dramatic primitive wins.** Unboxing is the headline; reference elements were never boxed to begin with, so `FArray[String]` is competitive with `IArray[String]` rather than crushing it. The structural-op and sort wins still apply.

---

## Try it

```scala
// a normal pipeline — every element stays an unboxed Int
FArray.tabulate(1_000_000)(i => i)
  .map(_ + 1)
  .filter(_ % 3 == 0)
  .take(10)
  .foldLeft(0)(_ + _)

// and when you want the whole chain as ONE loop, add a word:
FArray.tabulate(1_000_000)(i => i).fuse
  .map(_ + 1)
  .filter(_ % 3 == 0)
  .foldLeft(0)(_ + _)   // one while-loop over the int[]; nothing allocated in between
```

📖 **[Read the FArray story →](https://oyvindberg.github.io/farray/)** — how it's built and why it's fast, told through hoverable JMH charts and code extracted from source that compiles. Four pages: the story itself, [the fuse optimizer](https://oyvindberg.github.io/farray/#/fusion), [the fused JSON parser](https://oyvindberg.github.io/farray/#/json) it powers, and [the complete benchmark reference](https://oyvindberg.github.io/farray/#/reference) — every operation at every size, wins and losses alike.

Regenerate the numbers on your own machine:
```bash
scripts/bench-run.sh                     # runs the JMH suite → docs/bench-results.json
cd site && npm install && npm run dev    # the site, live, on your fresh numbers
npm run pages                            # rebuild the published site into docs/
```
The raw `docs/bench-results.json` also loads directly into <https://jmh.morethan.io> for an interactive view.

---

## Status

Immutable · covariant · Scala 3 · fully unboxed · `hashCode` equal to `List`. The core is stable and heavily benchmarked.

There's an active design for **compile-time fused pipelines** — `xs.map().filter().take()` lowered to a single unboxed loop with no intermediate collections (and a small compile-time query optimizer on top) — on the `fused-pipeline` branch: see [`docs/fused-pipeline-design.md`](docs/fused-pipeline-design.md).

Contributions welcome — especially if you're handy with JMH. See [`contributing.md`](contributing.md).
