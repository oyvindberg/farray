# FBuilder — the unboxed imperative builder

`FBuilder[A]` is FArray's answer to `xs.newBuilder` / `ArrayBuffer` / `ArrayBuilder`: accumulate
elements one at a time with `+=` / `++=`, then take an `FArray[A]` out with `result()`. Its reason to
exist is a single hard requirement — **zero boxing of primitive elements on the `+=` path** — which the
standard-library builder interface structurally cannot meet.

## Why `scala.collection.mutable.Builder[A, _]` boxes

`Builder[A, To]` declares `def addOne(elem: A): this.type`. `A` is an ordinary (erased) type parameter,
so at the bytecode level the method's descriptor is `addOne(Ljava/lang/Object;)Lscala/collection/mutable/Builder;`.
There is exactly one entry point and its argument type is `Object`. When you write `b += anInt` with
`b: Builder[Int, _]`, the compiler must widen the `int` to a `java.lang.Integer` (`Integer.valueOf(...)`)
*before the call*, because the callee only accepts a reference. The allocation happens at the call
boundary; nothing the builder does internally can undo it. Specialized builders like
`ArrayBuilder.ofInt` or `zio.ChunkBuilder.Int` store an `int[]`, but they still inherit the erased
`addOne(Object)` from the trait — held at the trait's static type, every append round-trips through a
box + unbox. (You can dodge it only by holding the *concrete* specialized builder type so the
`addOne(Int)` overload is selected statically — brittle, and unavailable through any generic code.)

FArray's whole design already solved this for `map`/`foldLeft`: the surface op is `inline`, the element
kind is resolved at the concrete call site via `summonFrom` on `${K}Repr[A]`, and the primitive is read
and written through a per-kind primitive array. `FBuilder` reuses that machinery verbatim.

## API surface

```scala
val b = FArray.newBuilder[Int]   // kind resolved HERE, at the call site (compile-time)
b += 1                            // unboxed: writes an int straight into an int[]
b += 2
b ++= FArray(3, 4, 5)            // bulk: whole leaves arraycopy'd, trees materialized once
b.sizeHint(1024)                  // pre-grow the backing array
val fa: FArray[Int] = b.result()  // hands the buffer to a leaf, house slack rules, no defensive copy
b.clear()                         // reset to empty, KEEP the array (cheap reuse)
```

| member | note |
| --- | --- |
| `FArray.newBuilder[A]` / `FBuilder[A]()` | fresh empty builder; kind chosen at this call site |
| `FBuilder[A](sizeHint: Int)` | pre-sized to hold `n` without a regrow |
| `+= (elem: A)` / `addOne` | **inline, unboxed** append; returns the builder for chaining |
| `++= (xs: FArray[A])` / `addAll` | bulk append; leaves `System.arraycopy`, trees materialize once |
| `sizeHint(n)` | grow backing array so `n` elements fit |
| `clear()` | logical reset (`size = 0`), backing array retained |
| `length` / `knownSize` / `isEmpty` / `nonEmpty` | accumulated count |
| `result(): FArray[A]` | slack-wrap the buffer into a leaf (no copy unless <¼ full) |
| `asScala` | **second-class** `mutable.Builder[A, FArray[A]]` — the boxing interop path |

## Design: opaque type + inline dispatch

```
opaque type FBuilder[A] <: FBuilderBase = FBuilderBase
```

`FBuilderBase` is a generated `sealed abstract class`; its concrete subclasses are the per-kind
`${K}Group` buffers (the very same growable primitive buffers already used by `groupBy`/`collect`,
now given this shared root). Because the opaque type's upper bound is `FBuilderBase`, every structural
method (`addNode`, `sizeHint`, `clear`, `knownSize`, `result`, `addBoxed`) is reachable as a plain
virtual call — none of them touch a primitive element type, so none can box.

The **only** kind-sensitive method is the unboxed append. It is generated as

```scala
inline def builderAddImpl[A](b: FBuilderBase, elem: A): Unit =
  summonFrom {
    case r: IntRepr[A]  => b.asInstanceOf[IntGroup].add(r.unwrap(elem))   // int  -> int[]
    case r: LongRepr[A] => b.asInstanceOf[LongGroup].add(r.unwrap(elem))  // long -> long[]
    ...
    case r: RefRepr[A]  => b.asInstanceOf[RefGroup].add(elem.asInstanceOf[Object])
    case _ => compile-error("no element-kind specialization for A ...")
  }
```

`FBuilder.+=` is `inline`, so at a call site with a concrete `A` the `summonFrom` collapses to a single
arm: a cast to the concrete `${K}Group` and a primitive store. No boxing, no runtime type test, no
virtual dispatch on the hot path — identical to how `map` stays unboxed.

The entry `FArray.newBuilder[A]` is likewise `inline` and dispatches once to `new ${K}Group()`.

## Growth strategy

Each `${K}Group` starts with a length-16 primitive array and **doubles** on overflow
(`Arrays.copyOf(arr, size << 1)`) — the same amortized-O(1) append as `ArrayBuffer`/`ArrayBuilder`.
`sizeHint(n)` grows the array to `n` up front (only ever grows, never shrinks), so a builder with a
known target size pays a single allocation and zero regrows. `clear()` keeps the (now warm) array, so a
builder reused across many `result()` cycles amortizes its allocation to nothing.

`++=` avoids the per-element path entirely: an `${K}Arr` leaf is `System.arraycopy`-ed in one shot; an
`${K}One` is a single `add`; `Empty` is a no-op; a genuine structural tree is `materialize`-d once
(itself arraycopy-per-run) and then bulk-copied. So concatenating existing FArrays never re-dispatches
or re-boxes element by element.

## `result()` slack semantics

`result()` hands the backing array **directly** to a leaf — no defensive copy — reusing the exact
`trimLeaf` heuristic the rest of FArray uses: if the accumulated `size` is at least ¼ of the array's
capacity, the array is wrapped verbatim as an `${K}Arr` of logical length `size` (leaves iterate to
`length`, so trailing slack is invisible and free). Only a badly over-hinted buffer (kept < ¼ full)
pays one `Arrays.copyOf` to reclaim the waste. Sizes 0 and 1 canonicalize to `Empty` / `${K}One`,
upholding FArray's size-0/1 leaf invariant.

Note the buffer is *shared* after `result()` — the builder still points at the same array. Continuing to
`+=` after `result()` and before `clear()` would mutate an array a returned FArray may be viewing; the
documented contract is `result()` then `clear()` before reuse (matching how one normally drives a
builder). This is the deliberate price of "no defensive copy".

## Interop (second-class, boxing)

`b.asScala` returns a real `scala.collection.mutable.Builder[A, FArray[A]]` so `FBuilder` plugs into
`Factory` / `IterableOnce#to(...)` style code. Its `addOne(elem: A)` necessarily takes an erased `A` and
routes through `FBuilderBase.addBoxed(elem: Any)`, which unboxes per kind
(`BoxesRunTime.unboxToInt`, …) into the same primitive array. Correct, but it pays the box the native
surface avoids — hence "second-class", to be used only for stdlib integration, never on a hot loop.

## Alternatives considered

- **Reuse `mutable.Builder` directly.** Rejected: the erased `addOne(Object)` boxes every primitive at
  the call boundary (see above). This is the entire motivation.
- **A `${K}Builder` class per kind exposed directly (no opaque type).** Rejected: it would force callers
  to name the kind (`IntBuilder` vs `RefBuilder`) and lose the uniform generic `FBuilder[A]` surface;
  the opaque-type + `summonFrom` approach keeps one type while still resolving kind statically.
- **A brand-new per-kind buffer class.** Rejected in favour of reusing the existing `${K}Group` (already
  battle-tested by `groupBy`/`collect`), extended with `addNode`/`sizeHint`/`clear`/`addBoxed` and a
  shared sealed root. Less generated code, one grow strategy to reason about.
- **Defensive copy in `result()`.** Rejected: it would double the allocation for the common
  build-once-then-hand-off pattern. The `trimLeaf` slack rule already right-sizes pathological cases; the
  shared-array contract covers the rest.
- **fs2 chunk builder in the benchmark.** fs2 exposes no first-class unboxed public chunk builder
  (building goes through `Chunk.array`/collectors), so the shootout uses zio's specialized
  `ChunkBuilder.Int` as the chunk-library rival and notes fs2's absence.
