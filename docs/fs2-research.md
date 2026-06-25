# fs2 research — through the lens of `fuse`

> **Purpose.** Inform the design of `fuse`'s evolution into a **constant-memory, unboxed streaming engine** with **ox-style structured concurrency** (virtual threads, direct style) instead of an `IO`-effect monad. This document studies [fs2](https://fs2.io) (typelevel/fs2, read against tag/branch **3.13**, `main` @ `50b0bde`, cloned to `~/pr/fs2`) and reimagines its capabilities for a compile-time-fused, unboxed pipeline.
>
> Every signature and internal claim below was verified against fs2 3.13 source under `core/shared/src/main/scala/fs2/` (`Stream.scala`, `Pull.scala`, `Chunk.scala`, `internal/Scope.scala`, `concurrent/{Channel,Topic,Signal}.scala`). ox names verified against `softwaremill/ox` `master`.

**Priorities (the lens for everything):**
1. **Constant memory** is THE feature — including reading streamed/unbounded data (files, sockets, iterators) in O(1) working set.
2. **Unboxed** end-to-end — our categorical advantage; fs2 boxes through `Chunk`/`Pull`/`IO`.
3. **Structured concurrency** via ox / virtual threads, NOT `IO`. Effect composition is explicitly LOW priority.

**Where `fuse` stands today (recap, for grounding the proposals below).** `xs.fuse.map(f).filter(p).<terminal>` is read off the typed AST at compile time and lowered to ONE specialized `while` loop: lambdas beta-reduced in (no `Function1`), fully unboxed over Int/Long/Double + reference types (`int[]`/`long[]`/`double[]` backing). ~17 stages (map/flatMap/filter/collect/take/drop/takeWhile/dropWhile/distinct/zip/zipWithIndex/map2/scanLeft/tapEach/…), ~45 terminals (run/foldLeft/sum/min/reduce/groupBy/groupMapReduce/partition/grouped/…). An optimizer does dead-column elimination, compute-for-survivors (sink past filters), CSE, unboxed tuple materialization. **Structural fact that makes streaming a natural fit: every fuse terminal is already an `(init-state-above-loop, step-per-element, finalize)` shape — i.e. a streaming fold.** The whole streaming story below is "make that step run over chunks pulled lazily from a source, carrying the state across chunks."

---

## 1. Combinator inventory, categorized by memory behavior

Memory classes used throughout:

- **CONST** — constant / O(1) or O(chunk) working set. True streaming.
- **BUFFER(k)** — bounded buffer of size proportional to an explicit parameter `k`.
- **SINK-O(1)** — *consumes* the whole stream to a single terminal value, but with O(1) memory (one accumulator). Distinct from buffering the stream.
- **BARRIER** — O(N): must hold the whole stream (or a whole unbounded group) before it can emit/complete.

> Two corrections to commonly-cited fs2 API: in current fs2, bare `broadcast`, `balance`, `balanceThrough`, `throttle`, `sort`, `sortBy` are **not** `Stream` methods. `Broadcast.scala`/`Balance.scala` were removed; fan-out is now `broadcastThrough` (built on `Topic`); rate-limiting is `metered`/`debounce`/`spaced`; there is no built-in sort.

### Transform

| combinator | signature | class | why |
|---|---|---|---|
| `map` | `map[O2](f: O => O2): Stream[F,O2]` | CONST | chunk-by-chunk, nothing held. |
| `mapChunks` | `mapChunks[O2](f: Chunk[O] => Chunk[O2])` | CONST (O(chunk)) | one chunk in, one out. |
| `evalMap` | `evalMap[F2>:F,O2](f: O => F2[O2])` | CONST | effect per element, emits singletons. |
| `evalMapChunk` | `evalMapChunk[F2>:F: Applicative,O2](f: O => F2[O2])` | CONST (O(chunk)) | sequences effects within a chunk. |
| `evalTap` | `evalTap[F2>:F,O2](f: O => F2[O2]): Stream[F2,O]` | CONST | effect for side-effect, passes original. |
| `mapAccumulate` | `mapAccumulate[S,O2](init: S)(f: (S,O) => (S,O2))` | CONST | fold-with-output; carries only `S`. |
| `scanChunks` | `scanChunks[S,O2>:O,O3](init: S)(f: (S,Chunk[O2]) => (S,Chunk[O3]))` | CONST (O(chunk)) | per-chunk stateful transform, carries `S`. |
| `scanChunksOpt` | `scanChunksOpt[S,O2>:O,O3](init: S)(f: S => Option[Chunk[O2] => (S,Chunk[O3])])` | CONST (O(chunk)) | like `scanChunks`, `None` terminates. |
| `intersperse` | `intersperse[O2>:O](sep: O2)` | CONST | inserts separator inline. |
| `mapAsync`/`parEvalMap` | `parEvalMap[F2>:F,O2](n: Int)(f: O => F2[O2])(using Concurrent)` | BUFFER(n) | ≤ n effects in flight; results re-emitted **in order**. |
| `parEvalMapUnordered` | same shape | BUFFER(n) | bounded concurrency, emits as they complete. |

### Filter / take / drop

| combinator | signature | class | why |
|---|---|---|---|
| `filter` / `filterNot` | `filter(p: O => Boolean)` | CONST | drops in place. |
| `collect` | `collect[O2](pf: PartialFunction[O,O2])` | CONST | filter+map fused. |
| `collectFirst` / `find` | `find(f: O => Boolean)` | CONST | emit first match, halt. |
| `take` | `take(n: Long)` | CONST | pass first `n`, halt — nothing buffered. |
| `takeWhile` | `takeWhile(p, takeFailure=false)` | CONST | pass until predicate fails. |
| `drop` / `dropWhile` | `drop(n: Long)` | CONST | skip, no retention. |
| `takeRight` | `takeRight(n: Int)` | **BUFFER(n)** | last `n` unknown until end → ring of `n` (`unconsN`). |
| `dropRight` | `dropRight(n: Int)` | **BUFFER(n)** | can't emit until an element is known not to be in the final `n` → `n`-window. |

### Fold / aggregate

| combinator | signature | class | why |
|---|---|---|---|
| `scan` | `scan[O2](z: O2)(f: (O2,O) => O2)` | CONST | emits running accumulator; holds only the accumulator. |
| `scan1` | `scan1[O2>:O](f)` | CONST | seeded by first element. |
| `fold` | `fold[O2](z)(f)` | **SINK-O(1)** | consumes whole stream, emits one value; carries only the accumulator. |
| `fold1`/`reduce` | `reduce[O2>:O](f)` | SINK-O(1) | same, seeded by first. |
| `foldMonoid` | `foldMonoid[O2>:O](using Monoid)` | SINK-O(1) | one accumulator. |
| `foldMap` | `foldMap[O2](f)(using Monoid)` | SINK-O(1) | `map(f).foldMonoid`. |
| `exists` / `forall` | `exists(p): Stream[F,Boolean]` | SINK-O(1) | single Boolean, short-circuits. |

### Chunking

| combinator | signature | class | why |
|---|---|---|---|
| `chunks` | `chunks: Stream[F,Chunk[O]]` | CONST (O(chunk)) | re-emits existing chunks. |
| `unchunk` | `unchunk: Stream[F,O]` | CONST | flatten chunks to singletons. |
| `chunkN` | `chunkN(n: Int, allowFewer=true)` | **BUFFER(n)** | accumulates exactly `n` before emitting. |
| `chunkMin` | `chunkMin(n: Int, allowFewerTotal=true)` | BUFFER(~n) | coalesce until ≥ `n`. |
| `chunkAll` | `chunkAll: Stream[F,Chunk[O]]` | **BARRIER** | concatenates everything into one chunk. |
| `sliding` | `sliding(n: Int)` (also `(size,step)`) | **BUFFER(n)** | window `Queue` of size `n`. |
| `groupAdjacentBy` | `groupAdjacentBy[O2](f)(using Eq)` | **BARRIER per group** | a run of equal keys is fully buffered before its `(key,Chunk)` emits → a long run is O(N). |
| `groupAdjacentByLimit` | `groupAdjacentByLimit[O2](limit)(f)` | BUFFER(limit) | bounded variant — the safe choice. |
| `rechunkRandomly` | `rechunkRandomly(minFactor=0.1, maxFactor=2.0)` | BUFFER(×maxFactor) | bounded by maxFactor × inbound chunk. |

### Combine / zip

| combinator | signature | class | why |
|---|---|---|---|
| `flatMap` | `flatMap[F2>:F,O2](f: O => Stream[F2,O2])` | CONST | runs each inner stream fully before the next; no cross-element buffer. |
| `flatten` | `flatten` (O <:< Stream) | CONST | `flatMap(identity)`. |
| `zip` / `zipWith` | `zip[F2>:F,O2](that)` | CONST (O(chunk)) | lock-step one chunk each side; halts when **either** ends. |
| `zipAll` | `zipAll(that)(pad1,pad2)` | CONST (O(chunk)) | continues to the **longer** side padding the other; surplus is consumed+emitted, not stored. |
| `zipWithIndex` | `zipWithIndex: Stream[F,(O,Long)]` | CONST | running `Long` counter. |
| `zipWithNext` / `zipWithPrevious` | — | CONST | one-element lookahead / previous only. |
| `interleave` / `interleaveAll` | built on `zip`/`zipAll` | CONST (O(chunk)) | alternate, end at shorter / longer. |

### Concurrency

| combinator | signature | class | why |
|---|---|---|---|
| `merge` / `mergeHalt{Both,L,R}` | `merge[F2>:F,O2](that)(using Concurrent)` | BUFFER(≈2 chunks) | both sides run on fibers, hand off ≤1 chunk each via a **synchronous** `Channel` + per-side `Semaphore(1)`. |
| `parJoin` | `parJoin(maxOpen)(using Concurrent)` on `Stream[F,Stream[F,O]]` | BUFFER(maxOpen) | ≤ `maxOpen` inner streams open (`Semaphore(maxOpen)`); all share a synchronous output channel. |
| `parJoinUnbounded` | `parJoinUnbounded` | UNBOUNDED-topology | no cap on simultaneously-open inners; element memory still bounded by synchronous output. |
| `parEvalMap`/`parEvalMapUnordered` | (see transform) | BUFFER(n) | `Semaphore(n)` + `Channel.bounded(n+1)`. |
| `broadcastThrough` | `broadcastThrough[F2>:F,O2](pipes: Pipe*)(using Concurrent)` | BUFFER(≈1/pipe) | `Topic`, each subscriber a 1-slot channel; slowest pipe backpressures. |
| `concurrently` | `concurrently[F2>:F,O2](that)(using Concurrent)` | CONST | `that` runs in the background **for effect only**; output drained → no buffer. |
| `prefetch` / `prefetchN` | `prefetchN(n)(using Concurrent)` | BUFFER(n chunks) | `Channel.bounded(n)` look-ahead. |
| `hold` / `holdOption` / `holdResource` | `hold[F2>:F,O2](initial): Stream[F2,Signal[F2,O2]]` | CONST (1 value) | `Signal` keeps only the latest value. |

### Timing / buffer

| combinator | signature | class | why |
|---|---|---|---|
| `buffer` | `buffer(n: Int)` | BUFFER(n) | requests `n` eagerly. |
| `bufferAll` | `bufferAll` | **BARRIER** | `bufferBy(_ => true)` — entire stream before emitting. |
| `bufferBy` | `bufferBy(f: O => Boolean)` | BUFFER(per block) | bounded by longest true-run (O(N) if `f` never flips). |
| `groupWithin` | `groupWithin(chunkSize, timeout)(using Temporal)` | BUFFER(chunkSize) | emit at `chunkSize` **or** `timeout`, whichever first. |
| `debounce` | `debounce(d)(using Temporal)` | CONST (1 value) | keeps latest within window, emits after quiet. |
| `metered`/`meteredStartImmediately`/`spaced` | `metered(rate)(using Temporal)` | CONST | paces via backpressure; buffers nothing. |

### Source (companion factories)

| combinator | signature | class | why |
|---|---|---|---|
| `repeat` | `repeat: Stream[F,O]` | CONST | re-runs the source forever. |
| `iterate` | `iterate(start)(f: A => A)` | CONST | `start, f(start), …`; one value. |
| `unfold` | `unfold[S,O](s)(f: S => Option[(O,S)])` | CONST | from a single threaded state. |
| `unfoldEval` | `unfoldEval[S,O](s)(f: S => F[Option[(O,S)]])` | CONST | effectful unfold. |
| `unfoldChunk` | `unfoldChunk[S,O](s)(f: S => Option[(Chunk[O],S)])` | CONST (O(chunk)) | chunk per step. |
| `range` | `range[O: Numeric](start, stopExclusive)` | CONST | lazy counter. **emits one element per chunk** — use `emits(start until stop)` for one big chunk. |
| `emit`/`emits` | `emits(os: Seq[O])` | O(input) | wraps an already-materialized `Seq` as one chunk. |
| `constant` | `constant(o, chunkSize=256)` | CONST | repeats forever in 256-chunks. |
| `eval` | `eval(fo: F[O])` | CONST | one effect → one element. |
| `fromIterator` | `fromIterator[F](it, chunkSize)` | CONST (O(chunkSize)) | fills array up to `chunkSize` per pull. |
| `awakeEvery`/`fixedRate`/`fixedDelay` | `fixedRate(period)(using Temporal)` | CONST | clock ticks; holds nothing. |

### Resource / error

| combinator | signature | class | why |
|---|---|---|---|
| `bracket` | `bracket[R](acquire: F[R])(release: R => F[Unit])` | CONST | acquire/release pairing; one handle. |
| `resource` | `resource[O](r: Resource[F,O])` | CONST | streams a `Resource` value, guaranteed release. |
| `handleErrorWith` | `handleErrorWith[F2>:F,O2>:O](h: Throwable => Stream[F2,O2])` | CONST | on error switch to handler stream. |
| `onFinalize` / `onComplete` | `onFinalize(f: F2[Unit])` | CONST | finalizer / append at termination. |

### Compile (terminal)

`compile[F2>:F,G[_],O2>:O](using Compiler[F2,G]): CompileOps[F2,G,O2]` — all consume the whole stream; the question is whether they **store** it.

| combinator | class | why |
|---|---|---|
| `compile.drain` | SINK-O(1) | fold over chunks discarding output. |
| `compile.count` | SINK-O(1) | `foldChunks(0L)(_ + chunk.size)`. |
| `compile.fold(init)(f)` | SINK-O(1) | single accumulator; does **not** buffer the stream. |
| `compile.last` | SINK-O(1) | keeps only most-recent element. |
| `compile.to(Collector)` / `toList` / `toVector` | **BARRIER** | materializes all output. |

### The O(N)-buffering set (and exactly why)

A small, sharply-defined set genuinely holds the whole stream (or an unbounded group):

- **`bufferAll`** = `bufferBy(_ => true)`; the flip never fires → every chunk accumulates before the first emit.
- **`chunkAll`** — by definition concatenates all elements into one `Chunk`.
- **`compile.to`/`toList`/`toVector`** — materializes the entire output.
- **`groupAdjacentBy`** (unbounded form) — `= groupAdjacentByLimit(Int.MaxValue)`; one long run of equal keys buffers O(N). Use `groupAdjacentByLimit(limit)` / `groupWithin(chunkSize,timeout)`.
- **`bufferBy(f)`** — O(longest true-run); degenerates to O(N).
- **(implied) sorting** — no built-in; `compile.toList.map(_.sorted)` is O(N) by construction (total order needs every element).

**Consumes-whole-stream but O(1) memory** (the distinction that matters most for fuse): `fold`/`fold1`/`foldMonoid`/`foldMap`/`reduce`/`exists`/`forall`/`compile.{fold,drain,count,last}`. They *consume* O(N) over time; they never *hold* O(N) at once. This is exactly the shape every fuse terminal already has.

**Bounded buffers (O(k), not O(N))**: `takeRight(n)`/`dropRight(n)` (ring of n), `chunkN(n)`/`chunkMin(n)`/`sliding(n)`, `buffer(n)`/`prefetchN(n)`, `groupWithin(chunkSize,_)`, `parEvalMap(n)`/`parJoin(maxOpen)`, `broadcastThrough`/`merge` (≈1 chunk/branch). `parJoinUnbounded` is the one with no element cap on topology.

---

## 2. The streaming architecture — how fs2 achieves constant memory

**One-line thesis:** *fs2 evaluates nothing until a downstream consumer pulls; each pull step produces exactly one `Chunk`, hands it downstream, then discards it. Memory is bounded by chunk size, not stream length.* The price: per-step allocation of free-monad algebra nodes (`Bind`, `FlatMapOutput`) and boxing inside `Chunk` for primitive elements — exactly the surface fuse beats.

The guide states it directly (`site/guide.md:156`): *"Regardless of how a `Stream` is built up, each operation takes constant time… The runtime of these operations do not depend on the structure of `s`."* And on the cover (`_coverpage.md:7`): *"I/O (networking, files) computations in constant memory."*

### 2.1 The `Chunk` model (≈ our `FArray`)

`abstract class Chunk[+O]` is the **unit of work** — the granularity at which fs2 moves data and amortizes per-element overhead. Its two abstract members:

```scala
def size: Int
def apply(i: Int): O           // Chunk.scala:72  — abstract, erases to Object
```

Representations: `Chunk.empty`, `Chunk.singleton(o)` (`size=1`), `Chunk.array[O: ClassTag](values: Array[O], offset, length)` → `ArraySlice[O](values: Array[O], offset, length)` (a **view** over an array; `take`/`drop`/`splitAt` make new slices over the same array, zero-copy — `Chunk.scala:859`), plus `Chunk.vector`, `Chunk.iterator`, `Chunk.constant`, `Chunk.byteBuffer`, `ByteVectorChunk`. `++` does **not** copy — it builds a `Chunk.Queue` of constituents (indexed lookup amortized O(log #chunks)); call `compact` (needs `ClassTag`) to flatten to one array.

**This is precisely our `FArray`'s role.** `Chunk.ArraySlice[Int]` over a real `Array[Int]` ≈ `FArray`'s `IntArr` (an `int[]` + length). `Chunk.Queue` ≈ our `Concat` tree node. `compact` ≈ our materialize-to-flat-array. The structural correspondence is exact: **a chunk is an FArray; a stream is a lazily-produced sequence of FArrays.**

**Where Chunk boxes (verified):** Storage *can* be unboxed (`ArraySlice` over `Array[Int]`), but the generic API boxes:
- `Chunk.map` always allocates `new Array[Any](size)` and stores boxed values (`Chunk.scala:237,247,292,525`), so `Chunk[Int].map(_ + 1)` produces an `Array[Any]` of `java.lang.Integer`. (`Singleton.map` is the one exception.)
- `Chunk` is **not** `@specialized`; `def apply(i: Int): O` erases `O` to `Object`, so any element crossing the generic interface boxes — even over a primitive backing array.

### 2.2 `Pull` — the pull-based core

`sealed abstract class Pull[+F[_], +O, +R]` — *"a program that may pull values from one or more streams, write output of type `O`, and return a result of type `R`."* It is a **monad in `R`** that emits `O`s on a side channel. It is an interpreted free-monad / GADT algebra. Leaves (verified `Pull.scala`):

- **Terminals**: `Succeeded[R](r)`, `Fail(error)`, `Interrupted(token, deferredError)`.
- **Actions**: `Output[O](values: Chunk[O])` (`:762`), `Eval[F,R](value: F[R])` (`:797`), `Acquire[F,R](resource, release: (R,ExitCase) => F[Unit], cancelable)` (`:799`), `Translate` (`:765`), `FlatMapOutput[F,O,P](stream, fun: O => Pull[F,P,Unit])` (`:770`) — what `Stream.map`/`flatMap` compile to, `Uncons[F,O](stream)` (`:782`), `InScope` (`:805`), `StepLeg`, `InterruptWhen`, `GetScope`.
- **`Bind`** (`:690`) — the monadic glue: `flatMap`/`>>`/`map` each allocate a fresh anonymous `Bind` closing over `f`.

`Stream[+F[_], +O]` is a **thin newtype-style wrapper over `Pull[F, O, Unit]`** (`Stream.scala:157`: `final class Stream[+F[_],+O] private[fs2] (private[fs2] val underlying: Pull[F,O,Unit])`). Every combinator delegates to `underlying`; e.g. `Stream.flatMap`/`map`/`evalMap` route through `underlying.flatMapOutput(...)` (`:1040,1116,1143`), allocating a `FlatMapOutput` node plus a `Function1` closure.

**`uncons` — "next chunk + the rest".** The primitive every manual combinator is built on, exposed via `stream.pull`:

```scala
def uncons:  Pull[F, Nothing, Option[(Chunk[O], Stream[F,O])]]
def uncons1: Pull[F, Nothing, Option[(O,        Stream[F,O])]]
def unconsN(n, allowFewer=false): Pull[F, Nothing, Option[(Chunk[O], Stream[F,O])]]
```

`uncons` returns a `Pull` whose **result** is `None` (exhausted) or `Some((headChunk, tail))`. You get one chunk plus a continuation stream, recurse on the tail, and decide per chunk what to emit. **This is the single most important abstraction to port** — it is the pull-based generator interface, and it maps onto `fuse`'s incremental `Source` (§4).

### 2.3 The interpreter / compile loop

`Stream.compile` builds a `CompileOps` (no execution). The engine is `Pull.compile`, with a trampolined stepper `go`:

```scala
go[G[_], X, End](scope, extendedTopLevelScope, translation, runner, stream): F[End]
```

Each iteration: (1) `viewL(stream)` left-normalizes the `Bind` chain to expose the next primitive action; (2) dispatch threading the current `Scope[F]` — `Output(chunk)` feeds `foldChunk(b, chunk)` then continues with the tail; `Eval(fa)` runs the effect; `Acquire(...)` registers the finalizer in the scope; `Uncons(s)` steps `s` one chunk and resumes; `InScope`/`CloseScope` open/close child scopes.

**Why this is constant memory:** demand flows downstream → upstream. The terminal fold is the only thing that *requests* output. To produce one unit of result, the interpreter steps the Pull until it hits an `Output`, runs `foldChunk(acc, chunk)`, and **immediately drops its reference to that chunk** before stepping again. At any instant the live data is: the accumulator `B`, one `Chunk[O]` in flight, and the (lazy) tail `Pull`. A billion-element stream at chunk size 4096 holds ~4096 elements live, independent of total length. The tail is a *description* (a `Pull` value), not materialized data.

### 2.4 `Scope` & resource safety

`Scope[F]` is *"a period of stream execution in which resources are acquired and released."* It holds a `Ref[State]` where `Open(resources: Chain[ScopedResource], children: Chain[Scope])` — resources **prepended on acquire** → released **LIFO**; scopes form a **tree** (a child never outlives its parent). `Stream.bracket`/`Stream.resource`/`Resource` compile to the `Pull.Acquire` node, whose `release: (R, ExitCase) => F[Unit]` is registered via `acquireResource`. On `close(ec)` the scope walks children then resources, invoking each finalizer with the right `ExitCase`: `Succeeded` (normal), `Errored(e)` (exception), or **`Canceled` (interruption / short-circuit)**. Multiple finalizer errors fold into a `CompositeFailure`. This is the *"once and only once"* guarantee (`guide.md:226`).

**The canonical short-circuit example** (`guide.md:862`):
```scala
(Stream(1) ++ Stream.raiseError[IO](Err)).take(1).compile.toList.unsafeRunSync()
```
never raises, because `take(1)` short-circuits before the second element is evaluated — yet any resource opened upstream still releases (the enclosing scope `close`s with `ExitCase.Canceled`). **This is exactly the resource-safety semantics fuse must replicate** when `take`/`find`/`exists` short-circuit over a source that holds an open file/socket. The guide also notes: a stream is *never* interrupted while acquiring or releasing a resource.

### 2.5 Back-pressure

fs2 is **demand-driven**: back-pressure is intrinsic because production is literally driven by consumption. The terminal fold steps the Pull by one `uncons`, gets one chunk, folds it, loops. A slow consumer simply doesn't issue the next step → the upstream effect (`Eval`, `Acquire`, the iterator `next()`) is not run until then. The producer is paced exactly by the consumer. (Contrast push-based: a producer shoves elements, and a slow consumer forces an unbounded buffer or drops.)

### 2.6 Where fs2 boxes — the surface fuse beats

The cost ledger (verified), every item of which a statically-fused unboxed pipeline removes:

1. **Pull free-monad allocation, per combinator per step.** `flatMap`/`>>`/`map` each allocate a fresh `Bind`; `Stream.map`/`flatMap`/`evalMap` allocate a `FlatMapOutput` + `Function1`. `viewL` rebalances/reallocates `Bind` chains. A pipeline `s.map(f).filter(p).map(g)` is a tree of algebra objects the interpreter walks and re-allocates each step. **fuse collapses `f,p,g` into one loop body with zero algebra nodes.**
2. **Chunk boxing for primitive `O`.** `Chunk.map` → `Array[Any]` of boxed `Integer`; abstract `apply(i): O` erases to `Object`. **fuse keeps `int[]` end-to-end, applying `f: Int => Int` monomorphically.**
3. **Function1 / closures.** Every `map`/`filter`/`flatMap`/`evalMap` captures a `Function1` invoked per element through a virtual `apply` (megamorphic after a few stages). **fuse inlines the lambda body — no objects, no virtual dispatch.**
4. **Effect thunks.** `Pull.Eval(value: F[R])` wraps each effectful step; for pure transforms the whole `F`/`MonadError`/interruption machinery is pure overhead. **fuse's pure spans have no `F`.**
5. **Per-element residual.** Chunking amortizes (1)/(4) over N elements (fs2's whole perf story), but does **not** eliminate (2)/(3) — the inner `Chunk.map`/`foreach` still boxes and dispatches per element. fs2's residual per-element cost is **boxing + virtual closure call**; per-chunk cost is **Bind/FlatMapOutput alloc + scope check + effect bind**. fuse removes every line.

---

## 3. Concurrency — fs2 internals, then reimagined in ox

### 3.1 How fs2 keeps each concurrent combinator bounded

The two bounding primitives:

- **`Channel[F, A]`** (`fs2.concurrent.Channel`): `bounded(capacity)`, `unbounded` (= `bounded(Int.MaxValue)`), `synchronous` (= `bounded(0)`). A `Ref`-state machine; when full, `send` **semantically blocks** the producer fiber (parks it on a `Deferred`) until the consumer drains. `synchronous` = rendezvous: `send` completes only when a consumer takes that exact element. `close` then `stream` drains remaining elements and terminates.
- **`cats.effect.std.Queue[F,A]`**: `bounded(n)` (offer blocks when full — the backpressure primitive), `unbounded` (offer never blocks — unbounded memory), `synchronous` (= `bounded(0)`), `dropping(n)` (drop new when full — lossy, bounded), `circularBuffer(n)` (evict oldest — lossy, bounded).

**Universal pattern:** every bounded combinator backpressures by making the producer's effect (`Channel.send`/`Topic.publish1`/`Queue.offer`) semantically block the producer fiber until a consumer drains. "Block" = the fiber suspends; it does not peg a thread.

- **`merge`** — `Channel.synchronous` carrying *streams*, two producer fibers, a per-side `Semaphore(1)` "guard" so a side won't pull its next chunk until the previous was handed off. In-flight memory ≤ 2 chunks total.
- **`parJoin(maxOpen)`** — `Semaphore(maxOpen)` gates the number of *open* inner fibers (the outer fiber blocks in `available.acquire` when `maxOpen` are open → bounds topology); all inners share one **synchronous** output channel → element memory ≤ ~one chunk per open inner. `parJoinUnbounded` = `maxOpen = Int.MaxValue` (unbounded fibers, still bounded element memory).
- **`parEvalMap(n)`/`Unordered`** — `Semaphore(n)` bounds in-flight effects (source fiber blocks when permits exhausted) + `Channel.bounded(n+1)` for results (verified `Stream.scala:2311`). **Ordered** enqueues a placeholder `Deferred` in stream order, completed by the worker when `f(el)` finishes; downstream reads placeholders in order, blocking on `.get`. **Unordered** sends completed results in completion order.
- **`broadcastThrough(pipes*)`** — `Topic`, each pipe a `subscribeAwait(1)` (1-slot channel); `Topic.publish1` blocks on the **slowest** subscriber → source backpressured by the slowest pipe; `CountDownLatch` ensures no element published before all subscribe.
- **`concurrently(that)`** — no buffer at all: `that.compile.drain` discards output (O(1)); two `Deferred`s coordinate interruption when the foreground finishes or `that` errors.
- **`prefetchN(n)`** — `Channel.bounded[Chunk[O]](n)`, producer runs `n` chunks ahead in a background fiber, blocks on `send` once `n` buffered.
- **`Topic`** — per-subscriber bounded channel (`subscribe(maxQueued)`); lossless + backpressuring (slow subscriber blocks publisher).
- **`Signal`/`SignallingRef`** — a single mutable cell + version counter + one-shot `Deferred` listeners (notifications, *not* a value backlog). O(1) regardless of producer speed; `discrete` coalesces/skips intermediate values. **The odd one out: it stays bounded by dropping to the latest value, NOT by backpressuring the producer.**

### 3.2 Reimagined in ox / structured concurrency / virtual threads

ox is direct-style: a "computation" is a by-name block `=> T` on a virtual thread (Loom); capabilities (`Ox`, `OxUnsupervised`) are `using` parameters. Core vocabulary:

- **Scopes**: `supervised { (using Ox) ?=> ... }` — on exit, `herd.interruptAllAndJoinUntilCompleted()` (the structured guarantee: no fork outlives the scope), then finalizers run LIFO + uninterruptibly. One fork failing → the supervisor ends the scope and interrupts siblings (no `.join()` needed to observe failure).
- **Forks**: `fork` (daemon), `forkUser` (keeps scope alive), `forkUnsupervised`, `forkCancellable` (`.cancel()`/`.cancelNow()`); `Fork[T].join()`.
- **Combinators**: `par(a, b, …)` / `parLimit(n)(tasks)`; `race`/`raceSuccess`/`raceResult`/`timeout`.
- **Channels** (`ox.channels`): `Channel.rendezvous` (≈ fs2 synchronous — `send` blocks until a `receive` meets it), `Channel.buffered(n)` (≈ fs2 bounded), `Channel.unlimited` (≈ fs2 unbounded, no backpressure); `send`/`receive` (blocking); `select(...)` over multiple channels.
- **Resources**: `useInScope(acquire)(release)`, `useCloseableInScope`, `releaseAfterScope` (released after all forks finish, before scope completes, uninterruptibly, on any exit); single-resource `use`/`useCloseable`.
- **`Flow`** (`ox.flow`) — a lazy, cold stream. The core is `FlowStage.run(emit: FlowEmit[T])`: each stage **pushes** into the downstream `FlowEmit` synchronously on the calling thread — so it behaves like a cold pull stream (backpressure automatic: `emit` blocks the producer until the consumer returns). Concurrency operators (`mapPar`, `merge`, `buffer`, `groupedWithin`) open a nested scope internally.

**`Flow.mapPar` confirmed shape (the proof structured concurrency reproduces fs2's bounded concurrency):** opens a nested `unsupervised` scope; `Semaphore(parallelism)` caps concurrency; a **bounded** `Channel.withCapacity[Fork[…]](parallelism*4)` holds in-flight forks (backpressures the upstream-driving fork); one fork runs the upstream and per element spawns a `forkUnsupervised` (acquire permit → run `f` → release); a second fork drains the in-progress channel **in order**, `join()`-ing each and sending to a bounded results channel. On any error the scope exits → interrupts/joins all forks → propagates. `mapParUnordered` is the same but emits as forks finish (no ordering buffer).

**The mapping — what each fs2 combinator becomes without `IO`:**

| fs2 | ox idiom | what backpressures |
|---|---|---|
| `parEvalMap(n)(f)` | `Flow.mapPar(n)(f)` | bounded in-progress channel + `Semaphore(n)`; ordered. |
| `parEvalMapUnordered(n)(f)` | `Flow.mapParUnordered(n)(f)` | `Semaphore(n)` + bounded results channel; unordered. |
| `merge` | `Flow.merge(other)` — both sides `runToChannel()` into bounded channels, `select`ed | per-side bounded channel. |
| `prefetchN(n)` | `Flow.buffer()` with `BufferCapacity(n)` | bounded channel of size n. |
| `concurrently(bg)` | a background `fork { … }` in the enclosing `supervised` scope | scope auto-cancels the fork when the body finishes. |
| `broadcastThrough` | `fork` source + `send` each element to N `Channel.buffered` sinks (or `Flow.alsoTo`) | sender blocks on the slowest bounded sink (`alsoToTap` = drop-if-slow). |
| `balance(n)` (removed in fs2) | N consumer forks `receive()`-ing from ONE shared bounded `Channel` (work-stealing) | the single bounded channel. |
| bounded/synchronous/unbounded `Channel` | `Channel.buffered(n)`/`rendezvous`/`unlimited` | send blocks / rendezvous / never. |
| `bracket`/`Resource` | `supervised { useInScope(acquire)(release); … }` (or `Flow.onComplete`/`onError`/`onDone`) | release uninterruptibly after all forks, on any exit. |
| `Signal`/`hold` | a plain `AtomicReference` (+ `LockSupport`/`CompletableFuture` notify) — no scope | drop-to-latest, not backpressure. |

**What needs a runtime fiber system + dynamic topology:** **only `parJoin*`** — data-driven dynamic spawning of a new worker per inner stream, capped by a semaphore, each with its own scope/lease lifecycle. That maps to a `StructuredTaskScope`-style "open a subtask per inner, cap with a semaphore, share one synchronous output channel," but it needs real per-inner scope management, not a fixed pool. Everything else is *"open a fixed N of virtual threads, wire them with one (or one-per-consumer) bounded blocking queue, let `send`/`offer` block the producer thread."*

**The minimal concurrency vocabulary** that preserves constant memory and composes with statically-fused pure spans:
1. **A structured scope boundary** (`supervised`) opened by the *terminal* (or by the concurrent stage), inside which the whole pipeline runs.
2. **A bounded blocking channel/queue** — `ArrayBlockingQueue(k)` of `FArray[A]` chunks (chunk-granular handoff, so per-element overhead is amortized exactly like fs2). This is the single backpressure primitive; everything bounded is "N forks + one (or N) bounded chunk-queues."
3. **`fork` / `forkUser` + a `Semaphore(n)`** for the worker-pool stages (`parEvalMap`/`merge`/`prefetch`).
4. **`useInScope`/`releaseAfterScope`** for resource lifetime tied to the scope — the bracket equivalent.

That is the whole kit. Pure spans stay fused `while`-loops over `FArray` chunks; a concurrency stage is "a fused producer loop on a fork → bounded chunk-queue → a fused consumer loop on another fork."

---

## 4. The reimagining for fuse — concrete source & chunk-driver proposal

### 4.1 What maps cleanly onto a chunk-at-a-time fused loop with cross-chunk state

The decisive observation: **every fuse terminal is already `(init-state, per-element step, finalize)`.** Constant-memory streaming is the same shape with the loop fed by a *lazily pulled sequence of chunks* instead of one in-memory `FArray`, and the state living **above the chunk loop** so it survives across chunks. Concretely, these fs2 capabilities port to a fused chunk loop with cross-chunk state:

- **Stateless transforms** — `map`, `filter`, `filterNot`, `collect`, `mapChunks`, `intersperse`, `tapEach`: already fused; just run the loop body per chunk.
- **Carried-state transforms** — `scan`/`scanLeft` (var acc above the loop), `mapAccumulate` (var state), `zipWithIndex` (var counter), `zipWithNext`/`zipWithPrevious` (one carried element), `distinct` (a `seen` set above the loop): all are a `var` hoisted above the chunk loop, already fuse's "slot" model — just hoist it above the *chunk* loop instead of the single-array loop.
- **Positional / predicate short-circuit** — `take`/`takeWhile`/`drop`/`dropWhile`/`find`/`exists`/`forall`/`head`: the existing `done` flag; once set, **stop pulling the source** (and trigger resource release — §4.3).
- **All SINK-O(1) terminals** — `foldLeft`/`sum`/`reduce`/`min`/`count`/`last`/`groupBy`/`groupMapReduce`/`partition`/`grouped`: literally the existing terminals, finalized after the last chunk.
- **Bounded-buffer stages** — `chunkN(k)`/`sliding(k)`/`takeRight(k)`/`dropRight(k)`/`groupAdjacentByLimit(k)`: a fixed-size ring/window above the chunk loop.
- **`flatMap`/`flatten`** — nested chunk loops (fuse already does nested unboxed loops for `flatMap`). Over a *source* it becomes "for each element, pull the inner source to exhaustion" — still constant memory.

### 4.2 The incremental `Source[A]` abstraction (what the macro stages over)

The minimal interface mirrors `Pull.uncons` but unboxed and chunk-granular. The chunk type **is** `FArray` (= fs2's `Chunk`):

```scala
// Minimal pull-based source. One method; returns the next chunk or end-of-stream.
// Specialized per element-kind so primitive sources hand back int[]/long[]/double[]-backed FArrays.
trait Source[+A]:
  /** Pull the next chunk. Returns an empty/sentinel FArray (or null) to signal end-of-stream.
   *  Implementations are free to size chunks (e.g. 4096); the driver does not care. */
  def pullChunk(): FArray[A] | Source.End

object Source:
  sealed trait End          // end-of-stream sentinel (one allocation, shared)
  case object End extends End
```

Design notes:
- **Why a chunk, not an element.** Per-element pulling reintroduces fs2's `Function1`/virtual-call cost. Chunk granularity lets the macro emit a tight inner `while` over the chunk's backing array (unboxed `iaload`/`iadd`), amortizing the one virtual `pullChunk()` call over the whole chunk — exactly fs2's amortization, minus the boxing.
- **Specialization.** `Source[Int]` should hand back `FArray[Int]` whose leaf is an `int[]`; the macro reads `IntArr.arr` and loops unboxed. This is the whole unboxed advantage — fs2 *can* store `ArraySlice[Int]` but re-boxes through `Chunk.apply`/`Chunk.map`; fuse never does.
- **No `F[_]`.** Side-effecting sources (file/socket) just *do* the read inside `pullChunk()` on the current (virtual) thread. Direct style. Back-pressure is intrinsic: `pullChunk()` is called only when the driver wants more — a slow consumer slows the source exactly as in fs2.
- **Stateful sources are plain mutable objects.** `fromIterator`, `fromInputStream`, `unfold`, `iterate`, `repeat` are all "a mutable cursor with a `pullChunk()`":
  - `fromIterator(it, chunkSize)` — fill an `FArray` builder up to `chunkSize` from `it.next()`, return it; `End` when `!it.hasNext`. (Mirrors `Stream.fromIterator`.)
  - `fromInputStream(is, chunkSize)` / `lines(reader)` — read up to `chunkSize` bytes/lines into a primitive `byte[]`/ref `FArray`; `End` at EOF. The O(1)-working-set file/socket reader — the headline constant-memory use case.
  - `unfold(s)(f: S => Option[(A,S)])` — carry `var s`; fill a chunk by repeatedly applying `f` until `None`; `End` when exhausted. `unfoldChunk` returns whole `FArray`s.
  - `iterate(start)(f)` / `repeat(xs)` / `range(lo,hi,step)` — generate a chunk at a time from a counter; constant memory, possibly infinite.

### 4.3 The chunk-driver (carries fold/scan state across chunks in constant memory)

The macro lowers `src.fuse.<stages>.<terminal>` to ONE driver loop. Shape (what the macro emits, conceptually):

```scala
// init: terminal state + every stage's cross-chunk slots, ABOVE the loops
var acc       = <terminal seed>           // foldLeft/sum/reduce/count slot
var <slots>   = <stage state>             // scan acc, take counter, drop counter,
                                          //   distinct `seen`, zipWithIndex counter, window ring …
var done      = false                     // short-circuit flag (take/find/exists/…)
val res = <growable or exact FArray builder, only if the terminal produces an FArray>

// resource acquisition is LAZY — opened on first pull (§ below)
while (!done) {
  val chunk = src.pullChunk()
  if (chunk eq Source.End) { done = true }   // normal completion
  else {
    val arr = chunk.<kind>Backing            // int[]/long[]/double[]/Object[] leaf
    var i = 0; val n = arr.length
    while (i < n && !done) {
      val x = arr(i)                         // unboxed read, no Chunk.apply boxing
      <fused stage bodies — beta-reduced lambdas, DCE/sink/CSE as today>
      <terminal step — e.g. acc = op(acc, y)>
      i += 1
    }
  }
}
<finalize — e.g. res.toFArray, or acc, or Some(acc), or the grouped map>
```

Key properties:
- **Constant memory.** Live data at any instant: `acc` + the stage slots + **one chunk's backing array**. Independent of total stream length. A 100 GB file folds in O(chunkSize). This is fs2's guarantee, statically fused.
- **The state lives above the loop** — fuse already hoists `var acc`/counters/`seen`/scan-acc above its single-array loop; the only change is **the chunk loop wraps the element loop**, and slots are hoisted one level further out. The `scanLeft` "prologue runs once before the loop" trick already generalizes: run it before the chunk loop.
- **Short-circuit stops pulling.** When `done` is set (take limit reached, predicate satisfied), the outer loop exits → no further `pullChunk()` → the source is never read past what's needed (and resources release — below). This is fs2's `take(1)` short-circuit, statically.
- **Unboxed throughout.** The inner loop is fuse's existing specialized `while` over `int[]`/`long[]`/`double[]`/typed-ref arrays — zero `Chunk` boxing, zero `Function1`, zero `Pull` nodes.
- **A bounded-buffer stage** (e.g. `chunkN(k)`) is just a `k`-sized builder slot above the loop, flushed when full.

### 4.4 Resource safety — the ox-scope equivalent of `bracket`

A `Source` backed by a file/socket must acquire on first pull and release on **termination, short-circuit, OR exception** — fs2's three `ExitCase`s. The ox-native mechanism:

- **The terminal opens a `supervised` scope** (or, for pure in-memory sources, no scope at all — zero overhead; the macro can detect a side-effect-free source and skip the scope).
- **Acquire lazily on first pull**, register release with the scope: `useInScope(open())(close)` (or `useCloseableInScope` for `AutoCloseable`). The handle lives in a slot above the loop.
- **The driver loop runs inside the scope.** On any exit path — normal `End`, `done` short-circuit, or a thrown exception unwinding the loop — the scope's exit runs the finalizer **once, uninterruptibly, LIFO**. This is precisely fs2's "once and only once," delivered by ox's `scopedWithCapability` `finally` semantics rather than a `Scope` tree threaded through a free monad.
- **No interruption-during-release hazard.** ox runs finalizers `uninterruptible`, matching fs2's "never interrupted while releasing."

So the bracket story is: **`fuse` terminal ⇒ optional `supervised` scope ⇒ `useInScope(acquire)(release)` for stateful sources ⇒ the fused driver loop ⇒ guaranteed release on the scope's `finally`.** The macro emits the scope only when a source/stage declares it needs one (a `def needsScope: Boolean` on `Source`, or a marker on resource stages), so pure pipelines stay zero-overhead `while` loops.

---

## 5. What WON'T port — be honest

The static-fusion model hits a wall wherever the *topology* of the computation is decided at runtime, or where semantics need a first-class suspendable/interruptible effect. The pragmatic boundary: **fuse the static pure spans; drop to a dynamic/scoped driver at concurrency and dynamic-topology points.**

1. **`flatMap` producing a *runtime-chosen Stream* / dynamic stream-of-streams.** fuse's `flatMap` works because the inner is a *known* shape (an `FArray` or another fused source) whose loop the macro can emit inline. fs2's `flatMap(f: O => Stream[F,O2])` where `f` returns a *different stream per element* (one element opens a socket, the next reads a file) cannot be a statically emitted nested loop — the inner program isn't known at compile time. **Port:** require the inner to be a `Source[B]` value at the type level and run it through a *runtime* chunk-driver (a small interpreter over `Source`), not the fused loop. Fused when the inner shape is statically a `map/filter/...` chain; dynamic otherwise.

2. **`parJoin` / `parJoinUnbounded` — dynamic concurrent topology.** The number of concurrently-open inner streams is data-driven (the outer stream *emits* streams). No static loop spawns a data-dependent, semaphore-capped, dynamically-growing set of forks. **Port:** a runtime structured-concurrency driver — `supervised` scope, `Semaphore(maxOpen)`, a fork per inner pulled from the outer source, shared bounded output chunk-queue. The *inner* and *outer* pure spans can still be fused loops; the join itself is a scoped runtime stage.

3. **`merge` / `interleave` of two live effectful sources, and `parEvalMap`.** These are *boundaries*, not fusible spans: two (or N) independently-paced producers must run on separate threads with a bounded handoff. **Port:** as §3.2 — a scoped stage with forks + a bounded chunk-channel. The producer feeding the channel and the consumer draining it are each fused loops; the stage between them is not fusible (it's where the threads and the queue live). This is fine — it's the natural seam.

4. **Interruption semantics on *blocked* effects.** fs2 can interrupt a fiber parked on `Eval`/`Channel.send` mid-effect (`Interrupted`, `interruptWhen`, `ExitCase.Canceled`). fuse's pure loops have nothing to interrupt (they just stop pulling). But a fused loop blocked inside a `pullChunk()` that is doing a blocking socket read can only be interrupted by ox's **thread interruption** (`forkCancellable.cancel()` / scope cancellation → `InterruptedException`). **Port:** rely on Loom thread interruption at the scope boundary; the pure spans are uninterruptible-but-fast (they finish the current chunk). Fine-grained mid-element interruption of a pure loop is neither needed nor offered — and that's correct.

5. **`Signal`/`hold`/`debounce`/`Topic` — stateful concurrent pub-sub with drop semantics.** A `Signal` is a shared mutable cell with drop-to-latest, observed concurrently. This is not a fused-pipeline shape at all — it's a concurrency *primitive*. **Port:** provide it as a library primitive (an `AtomicReference` + notifications), usable *from* a scoped stage, not as a fused stage.

6. **Effect composition / `IO` interleaving (`evalMap` with arbitrary `F`, `Resource` algebra, `MonadCancel`).** Explicitly out of scope by priority. In direct style, `evalMap(f)` is just `map` where `f` happens to do I/O on the current virtual thread — no `F[_]`, no monadic sequencing. The whole `Pull.Eval`/`MonadError`/`interruptibleEval` layer simply doesn't exist; effects are direct calls. The only thing lost is *referential transparency of the effect description* — which, post-virtual-threads and by stated priority, we don't want.

**The wall, summarized:** static fusion owns everything with a **statically-known element-to-element dataflow** (every CONST and SINK-O(1) and bounded-buffer combinator over a known source/stage chain). It cannot own **runtime-decided topology** (dynamic stream-of-streams, data-driven fork counts) or **independently-paced concurrent producers** (merge/parEvalMap/parJoin). The boundary is clean and already implied by fuse's design: *a concurrency or dynamic-topology operator is a **scope-opening stage** that breaks the fused span into a fused-producer-loop + bounded-channel + fused-consumer-loop.* You fuse up to the seam, open a structured scope at the seam, fuse again on the other side.

---

## 6. Recommendations — prioritized, mapped to the streaming roadmap

> The roadmap doc (`docs/streaming-roadmap.md` §7) referenced in the brief is **not present in this checkout** — the streaming direction currently lives in the auto-memory notes (`fused-pipeline-phase1`) and this document. The sequence below is the recommended §7 ordering; create/align the roadmap to it.

**Phase S1 — the incremental `Source[A]` + chunk-driver (the keystone; unlocks constant memory).**
- Implement `trait Source[A] { def pullChunk(): FArray[A] | End }` (§4.2), specialized per element-kind so primitive sources hand back `int[]`/`long[]`/`double[]`-backed `FArray`s.
- Teach `FuseMacro` to stage over a `Source` exactly as it stages over an `FArray` today: hoist terminal state + stage slots above a **chunk loop** wrapping the existing **element loop**; finalize after `End`. The existing `(init, step, finalize)` terminals need *no change* — only the loop nesting and slot-hoisting level change.
- Sources to ship: `fromIterator(it, chunkSize)`, `fromInputStream`/`lines` (the O(1) file reader — the headline demo), `unfold`/`unfoldChunk`, `iterate`, `repeat`, `range`. **Subsumes** fs2 `Stream.{fromIterator, fromInputStream, unfold, unfoldChunk, iterate, repeat, range, emits, constant}` — unboxed and without `Pull`/`Chunk` allocation.
- This is the single highest-leverage piece: it turns every existing CONST/SINK-O(1) terminal into a constant-memory streaming operation for free.

**Phase S2 — resource safety via ox scope (`bracket` equivalent).**
- A `supervised`-scope wrapper emitted by the terminal *only* when the source/stage needs it; `useInScope(acquire)(release)` for the file/socket handle; guaranteed release on completion / short-circuit / exception (§4.4). **Subsumes** fs2 `bracket`/`Stream.resource`/`onFinalize`/`handleErrorWith` (the cleanup half). Validate with the fs2 short-circuit test: a `take(1)` (or `find`) over a file source must close the file even though the source isn't exhausted.

**Phase S3 — cross-chunk stateful & bounded-buffer stages.**
- Generalize the slot model across chunks for `scan`/`mapAccumulate`/`zipWithNext`/`zipWithPrevious`/`distinct` (already have the slots — just hoist above the chunk loop), and add bounded-buffer stages `chunkN(k)`/`sliding(k)`/`takeRight(k)`/`dropRight(k)`/`groupAdjacentByLimit(k)`/`grouped`. **Subsumes** fs2 `scan`, `mapAccumulate`, `zipWith{Next,Previous}`, `chunkN`, `sliding`, `takeRight`, `dropRight`, `groupAdjacentByLimit`, `groupWithin`(size half).

**Phase S4 — the structured-concurrency seam (the differentiator).**
- A `parMap(n)(f)` / `mapAsync(n)` stage that opens a `supervised` scope at the seam: a fused-producer loop feeds a bounded `ArrayBlockingQueue[FArray[A]]`, `n` worker forks (`Semaphore(n)`) run `f`, a fused-consumer loop drains in order (placeholder-future trick for ordering). **Subsumes** fs2 `parEvalMap(n)`/`parEvalMapUnordered(n)`/`mapAsync`.
- `merge` (two forks → one rendezvous/bounded channel), `prefetch(n)` (one fork + `Channel.buffered(n)`), `concurrently` (a background `fork` cancelled by the scope). **Subsumes** fs2 `merge`/`prefetchN`/`concurrently`.
- A runtime `Source`-driver for `flatMap`-of-runtime-`Source` and a `parJoin(maxOpen)` scoped stage for dynamic stream-of-streams (§5.1, §5.2). **Subsumes** fs2 dynamic `flatMap`/`parJoin`.
- Pub-sub primitives (`Signal`/`Topic` equivalents) as library objects callable from a scoped stage. **Subsumes** fs2 `hold`/`Signal`/`Topic`/`broadcastThrough`.

**Phase S5 — timing/rate (`Temporal` equivalents).**
- `metered`/`spaced`/`debounce`/`throttle` as scoped stages using `Thread.sleep`/ox `tick` on a fork. **Subsumes** fs2 `metered`/`spaced`/`debounce`/`fixedRate`/`awakeEvery`.

### Where the combination gives fuse a story no library has

1. **Unboxed constant-memory streaming.** fs2 gives constant memory but boxes (`Chunk.map` → `Array[Any]`, `apply: O` → Object, `Function1`, `Pull` nodes, `IO` thunks). No streaming library combines **O(1) working set** *and* **`int[]`/`long[]`/`double[]` end-to-end with zero per-element boxing and zero per-step allocation**. A 100 GB file `fold`ed through `map.filter.sum` runs as one unboxed `while` loop over reused primitive chunks — fs2's memory profile at array-loop speed.

2. **Statically fused, then a clean concurrency seam.** Pure spans are single `while` loops with no `Pull`/`Chunk`/`Function1`; the *only* allocations/threads appear exactly at concurrency seams (where they're inherent). fs2 pays free-monad + boxing overhead on *every* span, concurrent or not. fuse pays only at the seam.

3. **Direct-style, structured concurrency — no monadic runtime.** Post-virtual-threads, `evalMap` is just `map` doing I/O on a virtual thread; `parEvalMap(n)` is N forks + a bounded queue in a `supervised` scope; resource safety is `useInScope` + a `finally`. No `IO`, no `MonadCancel`, no `Compiler`, no `Scope`-tree threaded through a free monad — yet the same once-and-only-once cleanup and the same demand-driven back-pressure. The fs2 guarantees (constant memory, resource safety, back-pressure) **without** the fs2 tax (boxing, free-monad allocation, effect monad).

**Bottom line:** build the `Source` + chunk-driver first (S1) — it converts the entire existing fused-pipeline surface into a constant-memory streaming engine in one move — then layer scoped resource safety (S2), cross-chunk stages (S3), and the structured-concurrency seam (S4) on top. The result is fs2's constant-memory and resource-safety guarantees, statically fused and fully unboxed, with ox-style structured concurrency instead of an effect monad — a combination no existing Scala streaming library offers.
