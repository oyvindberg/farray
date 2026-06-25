# ox research — structured concurrency for the `fuse` streaming engine

**Question:** should the unboxed, compile-time-fused streaming engine (`fuse` over `FArray`) get its
structured concurrency from **ox** (SoftwareMill), from a **vendored subset** of ox, or **directly from
the JDK 25 primitives**?

**Bottom line up front:** **Build directly on the JDK, vendoring ~1 idea (a bounded virtual-thread fork
helper) rather than the library.** ox is genuinely well-engineered and the *concept work* it has done is
worth studying, but (a) its two streaming-boundary types — `Channel[T]` and `Flow[T]` — **box every
primitive element**, which directly erodes the one thing this engine exists to protect (unboxed
`int[]`/`long[]`/`double[]`); (b) the structured-concurrency core fuse actually needs is small (≈150–250
lines) and maps almost 1:1 onto `Thread.ofVirtual` + a bounded queue + `Semaphore`; (c) ox does *not* even
build on `StructuredTaskScope` (it has its own `ThreadHerd`), so depending on ox to "shield us from JEP
churn" is a non-reason — and `StructuredTaskScope` is **still preview in JDK 25** anyway, which we should
avoid as a hard runtime dependency. The right move is to **pass `FArray` chunks across the concurrency
boundary, not elements**, so the boxing question becomes moot (the boundary moves one `Object` reference
per *chunk*, while the chunk stays a primitive array).

Versions/status as researched (June 2026): **ox 1.0.5** (Apache-2.0, Scala 3.3 LTS, JDK 21+, single
runtime dep `com.softwaremill.jox:channels`); **JDK 25** has virtual threads + ScopedValue **final**, and
`StructuredTaskScope` **still preview (5th preview, JEP 505)**.

---

## 1. ox inventory (with thin-wrapper vs value-add flags)

ox is **direct-style** structured concurrency: blocking code on virtual threads, no `IO`/effect monad. It
is exactly the philosophy fuse wants (structured concurrency *instead of* an effect runtime). The crucial
architectural fact, verified from source (`core/src/main/scala/ox/internal/ThreadHerd.scala`):

> **ox does NOT use `java.util.concurrent.StructuredTaskScope`.** It ships its own scope engine,
> `private[ox] class ThreadHerd(threadFactory: ThreadFactory)`, whose header comment says it is *"a
> replacement for `StructuredTaskScope`"* until the JEP stabilises in an LTS release. Everything
> (supervision, daemon/user forks, typed errors, cancellation, finalizers) is layered on top of a
> `ThreadFactory` of Loom virtual threads plus leaf `java.util.concurrent` primitives
> (`CompletableFuture`, `Semaphore`, `ArrayBlockingQueue`, atomics).

So "thin wrapper vs value-add" is judged **against raw Loom virtual threads + j.u.c**, not against
`StructuredTaskScope`.

### Scopes

| Construct | Signature | Flag |
|---|---|---|
| `supervised` | `def supervised[T](f: Ox ?=> T): T` — body gets `Ox` capability; any `fork`/`forkUser` that throws ends the scope, **interrupts all siblings**, re-throws after all forks join (no orphans). | **Value-add** — the supervision policy is ox's, not the JDK's |
| `unsupervised` | `def unsupervised[T](f: OxUnsupervised ?=> T): T` — weaker capability; only `forkUnsupervised`/`forkCancellable`; failures surface only on `.join()`. | **Thin** (closest to a bare never-shutdown scope) |
| `supervisedError` | `def supervisedError[E, F[_], T](em: ErrorMode[E,F])(f: OxError[E,F] ?=> F[T]): F[T]` — a fork that *returns* an application-error value (shape `F[T]`, e.g. `Left`) also ends the scope. | **Strong value-add** — typed value-based error propagation, no JDK analogue |
| `scoped` (old) | removed/superseded by the supervised model | n/a |

Capability hierarchy: `OxUnsupervised` (holds `herd: ThreadHerd`, `finalizers`, `supervisor`) → `Ox` →
`OxError[E,F]` (a **scope-capability `case class`**, *not* an exception type). `OxApp` / `OxApp.Simple` /
`OxApp.WithEitherErrors[E]` give entry points that run on a virtual thread in a root supervised scope with
clean SIGINT/SIGTERM teardown (`Settings.shutdownTimeout` default 10s) — **value-add** for an application,
irrelevant for a library.

### Forks

```scala
def fork[T](f: => T)(using Ox): Fork[T]                                  // supervised daemon
def forkUser[T](f: => T)(using Ox): Fork[T]                              // supervised user (scope waits)
def forkError[E,F[_],T](using OxError[E,F])(f: => F[T]): Fork[T]
def forkUnsupervised[T](f: => T)(using OxUnsupervised): UnsupervisedFork[T]
def forkCancellable[T](f: => T)(using OxUnsupervised): CancellableFork[T]
trait Fork[T]:            def join(): T
trait CancellableFork[T]: def cancel(): Either[Throwable,T]; def cancelNow(): Unit
```

- `fork` = supervised **daemon** (scope may end once body + user forks succeed). **Value-add** (the
  daemon/user split + auto-propagation is ox's).
- `forkUser` = supervised **user** (scope waits for it). **Value-add.**
- `forkUnsupervised` = failure not propagated, surfaces on `.join()`. **Thin** (≈ raw fork + future).
- `forkCancellable` = unsupervised + cancellable; costs a nested scope + two virtual threads. Thin +
  small value-add.

Cancellation is **interrupt-based and cooperative**; all outstanding forks are interrupted when the scope
ends (normally or on failure). This is the same `Thread.interrupt()` model the JDK uses.

### Combinators

```scala
def par[T1,T2](t1: => T1, t2: => T2): (T1,T2)            // + arities, Seq, ErrorMode forms
def parLimit[T](parallelism: Int)(ts: Seq[() => T]): Seq[T]   // gated by a Semaphore
def raceSuccess[T](fs: Seq[() => T]): T                 // first SUCCESS wins; losers interrupted+awaited
def raceResult[T](fs: Seq[() => T]): T                  // first to finish (success or failure) wins
def timeout[T](d: FiniteDuration)(t: => T): T           // = raceResult(t, {sleep(d); throw})
def timeoutOption[T](d: FiniteDuration)(t: => T): Option[T]
```

`par`/`parLimit`/`race*`/`timeout` are **compositional conveniences** over `supervised { fork… }` + a
`Semaphore` + an `ArrayBlockingQueue`. The "interrupt the losers **and await them** before returning"
guarantee in `raceSuccess` is the part that is genuinely fiddly to get right by hand — modest **value-add**.

### Channels

```scala
trait Source[+T]: def receive(): T; def receiveOrClosed(): T | ChannelClosed
trait Sink[-T]:   def send(t: T): Unit; def done(): Unit; def error(reason: Throwable): Unit
class Channel[T] extends Source[T] with Sink[T]
Channel.rendezvous[T]           // capacity 0, synchronous hand-off
Channel.buffered[T](capacity)
Channel.bufferedDefault[T]      // BufferCapacity given, default 16
Channel.unlimited[T]
def select[T](clauses: Seq[SelectClause[T]]): SelectResult[T]   // Go-style select over send+receive
```

- Closure is first-class (`ChannelClosed.Done` / `ChannelClosed.Error`); `*OrClosed` methods return
  `T | ChannelClosed`, plain methods throw.
- **Backing engine (verified):** NOT `java.util.concurrent` queues. ox's channels delegate to the
  separate Java library **`com.softwaremill.jox:channels`**, a **custom lock-free segment-based channel**
  implementing the Koval/Alistarh/Elizarov algorithm (arXiv:2211.04986 — the same design as Kotlin
  coroutine `Channel`), retargeted to virtual-thread `LockSupport.park`/`unpark`. Segments hold
  `Object[] data` (default size 32) manipulated via `VarHandle` CAS. **Real value-add** (unified
  rendezvous/buffered/unlimited + `select` over heterogeneous clauses + closure semantics; rendezvous
  ≈176 ns/op).
- **⚠ Boxing: `Channel[Int]` boxes.** Two erasure points: ox's `Channel[T]` delegates to jox `Channel<Object>`,
  and jox stores values in `Object[]`. No `@specialized`, no primitive channel. Every `Int` →
  `java.lang.Integer`. **This is the central conflict with our unboxed goal.**

### Flow (`ox.flow.Flow`) — the direct precedent; studied carefully

`class Flow[+T](protected val last: FlowStage[T])`. Core model (verified from source):

```scala
trait FlowStage[+T]: def run(emit: FlowEmit[T]): Unit
trait FlowEmit[-T]:  def apply(t: T): Unit
```

- **Cold / re-runnable**: a `Flow` is a lazy description; each `run*` re-invokes the pipeline from scratch.
- **Push-based, direct style**: stages are *driven* by `run(emit)` and push each element by calling the
  downstream callback `emit(t)`. This is the **inverse of fuse's pull/iterator** model. `map(f)` is a
  stage whose `run(emit) = last.run(t => emit(f(t)))`.
- **Constant memory — two mechanisms:** (1) in a synchronous segment, `emit(t)` blocks until the element
  is fully processed downstream, so only one element is on the call stack at a time → O(1), no
  intermediate collection; (2) at async boundaries (`mapPar`/`merge`/`buffer`/`runToChannel`) stages are
  joined by **bounded jox channels** sized by `BufferCapacity` (default 16); a full channel blocks the
  producer → backpressure → still constant memory. **This is a real, well-engineered constant-memory
  guarantee** — directly comparable to fs2/Pekko Streams, but in direct style on Loom.
- **⚠ Boxing: `Flow[+T]` boxes every primitive.** Generic erased `T`; `FlowEmit.apply(t: T)` erases to
  `Object`; channels at async boundaries hold `Object`. No `@specialized`. The *only* unboxed bulk path
  is byte IO, which produces `Flow[Chunk[Byte]]` — note the *element* is the `Chunk` (wrapping a primitive
  `Array[Byte]`), **not** the byte. **This is the model to emulate but with `FArray` chunks** — see §5.
- **`mapPar(n)` bounding (verified from the body):**

  ```scala
  def mapPar[U](parallelism: Int)(f: T => U)(using BufferCapacity): Flow[U] = Flow.usingEmitInline: emit =>
    val s          = new Semaphore(parallelism)
    val inProgress = Channel.withCapacity[Fork[Option[U]]](parallelism * 4)
    val results    = BufferCapacity.newChannel[U]
    // per element t: forkUnsupervised { s.acquire(); val u = f(t); s.release(); Some(u) }
    //                inProgress.send(thatFork)
    // a collector fork pulls fork-handles off `inProgress` IN ORDER and join()s them → results
  ```

  The throttle is a **`java.util.concurrent.Semaphore(parallelism)`**: one fork per element, but each fork
  `acquire()`s before `f(t)` and `release()`s after, so at most `n` invocations run concurrently;
  bounded channels (`inProgress` size `n*4`, `results`) keep the producer from racing ahead.
  **`mapPar` is order-preserving** (collector joins handles in upstream order). **`mapParUnordered`** uses
  the same `Semaphore` but emits each result the moment it finishes. **Value-add** (genuinely engineered);
  the leaf throttle is a raw JDK `Semaphore`.
- **Operators** (selected, from `FlowOps.scala`): `map filter collect mapConcat tap`; `scan mapStateful
  mapWithResource(create,close)(f) mapWithCloseableResource`; `mapPar mapParUnordered flattenPar
  groupBy`; `take takeWhile drop throttle sample debounce`; `buffer batch conflate grouped groupedWithin
  sliding split`; `flatMap flatten merge concat/++ prepend interleave(other,segmentSize,eagerComplete)`;
  `zip zipAll zipWithIndex intersperse`; `alsoTo alsoToTap drain`; resiliency `onComplete onError orElse
  recover recoverWith retry onErrorComplete`. **No `broadcast` on Flow** — closest is `alsoTo`/`alsoToTap`
  (tee to a `Sink`); true fan-out/`broadcast`/`balance` lives at the **channels** layer, not Flow.
- **Terminals** (`FlowRunOps.scala`): `runForeach runToList runToSet runToMap runFold runReduce
  runLast runDrain`, and crucially **`runToChannel()(using OxUnsupervised): Source[T]`** — forks the flow
  into a bounded channel and returns the pullable `Source` (bridges push-flow back to a pull channel).

**Flow verdict:** a legitimate, well-engineered, cold, push-based, constant-memory streaming engine. Its
differentiation is **direct-style structured concurrency** (ordered/unordered semaphore-bounded `mapPar`,
scope cleanup, select). Its weakness *for us* is that it pays the **generic-erasure boxing tax on every
primitive element**, and it is **push** where fuse is **pull/fused**. We are not going to adopt `Flow`'s
type; we are going to learn from its async-boundary design and apply it to chunks.

### Resources

```scala
def useInScope[T](acquire: => T)(release: T => Unit)(using OxUnsupervised): T   // finalizer, LIFO, uninterruptible
def useCloseableInScope[T <: AutoCloseable](c: => T)(using OxUnsupervised): T
def releaseAfterScope(release: => Unit)(using OxUnsupervised): Unit
inline def use[R,T](inline acquire: R, inline release: R => Unit)(inline f: R => T): T  // block-bound try/finally
```

Scope-bound `useInScope` registers a finalizer that runs **after all forks complete, before the scope
returns, LIFO, uninterruptibly** — **value-add** (no JDK analogue beyond hand-rolled try/finally).
Block-bound `use`/`useCloseable` are inline try/finally with an uninterruptible-release guarantee — thin.
(Note: there is **no** `useSupervised` in current ox; the multi-resource pattern is `useInScope` *inside*
a `supervised {}` block.)

### Application errors

- **`either {}` + `.ok()`** (`ox/either.scala`): `inline def either[E,A](inline body: Label[Either[E,A]]
  ?=> A): Either[E,A] = boundary(Right(body))`; `extension (e: Either[E,A]) transparent inline def ok(): A`
  short-circuits `Left` to the enclosing `either`. Built on **Scala 3 stdlib `scala.util.boundary`/`break`**
  — not JDK at all; the error type is inferred as the *union* of unwrapped error types. **Thin** over the
  stdlib; value-add = union inference + compile-time nesting guards.
- **Error-aware forks** (`supervisedError`/`forkError` + `ErrorMode` — `NoErrorMode`/`EitherMode`/`UnionMode`):
  an error *value* ends the scope like a thrown exception. **Strong value-add**, no JDK equivalent.

### Resilience & scheduling

- `ox.resilience.RateLimiter` (token/leaky bucket, fixed/sliding window; needs an `Ox` scope) — **value-add**.
- `ox.resilience.retry` + `Schedule` DSL (`exponentialBackoff`, `fibonacciBackoff`, `decorrelatedJitter`,
  `.jitter`, `.maxRetries`…) + **`AdaptiveRetry`** (token-bucket retry budget à la AWS SDK v2) — **strong
  value-add** (resilience4j-class, direct-style).
- `ox.scheduling.repeat` (poll/heartbeat on a schedule) — value-add.
- `ox.channels.Actor` (`ask`/`tell` over a serial mailbox on an ox channel) — value-add but **boxed**
  (rides on `Channel`).

### Metadata (verified)

- **Version 1.0.5** (2026-06-08), **not pre-1.0** (crossed 1.0 on 2025-08-20); MiMa binary-compat gating
  on `core` ⇒ a deliberately stable API surface.
- **Scala 3 only** (`3.3.8`, the 3.3 LTS); no Scala 2.13, no JS/Native.
- **JDK 21+** (Loom). **No `--enable-preview`** — ox deliberately avoids the preview `StructuredTaskScope`
  by using its own `ThreadHerd`.
- **core is lean**: single runtime dep `com.softwaremill.jox:channels:1.1.2`. **No cats-effect, no ZIO,
  no slf4j in core.** (slf4j/logback/otel/cron only in the optional `kafka`/`mdc-logback`/`otel-context`/
  `cron` modules.)
- **License Apache-2.0.**

---

## 2. JDK-thinness analysis — ox vs raw JDK 25, per primitive fuse needs

For each boundary fuse needs, here is (a) the ox way and (b) the raw-JDK way, with a line-count estimate of
what ox actually saves.

### 2a. `parEvalMap(n)` — bounded-concurrency `f: A => B`, order-preserving + unordered

- **ox:** `flow.mapPar(n)(f)` / `flow.mapParUnordered(n)(f)`, or `parLimit(n)(tasks)` for a fixed batch.
- **raw JDK 25 (chunked, the design we want):**

  ```scala
  // ordered parEvalMap over a producing iterator of FArray chunks, bound = n in-flight chunks
  def parEvalMapChunks[A,B](n: Int, src: Iterator[FArray[A]])(f: FArray[A] => FArray[B]): Iterator[FArray[B]] =
    val sem     = new java.util.concurrent.Semaphore(n)
    val results = new java.util.concurrent.ArrayBlockingQueue[java.util.concurrent.Future[FArray[B]]](n)
    val pool    = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
    // a feeder (one virtual thread) submits each chunk after sem.acquire(); results.put(future)
    // the returned Iterator does results.take().get() then sem.release()  — order preserved by FIFO queue
    // unordered variant: a CompletionService / completion callback that enqueues finished chunks
  ```

  This is ~40–60 lines for *both* ordered and unordered, plus careful interrupt/close handling. The hard
  parts the JDK leaves to you: making the feeder stop and `pool.shutdownNow()` (interrupt) when the
  consumer short-circuits, and not leaking a half-full queue. **ox saves ~40 lines here and gets the
  cancel-on-short-circuit right** — this is one of the two places ox earns real keep.

### 2b. `merge` / `interleave` — N producers → 1 consumer

- **ox:** `flowA.merge(flowB)`; at the channel layer, fork each producer into one `Channel`, consumer
  `receive`s; `select(srcA, srcB)` for fair interleave; `flowA.interleave(flowB, segmentSize, eagerComplete)`.
- **raw JDK 25:** one bounded `ArrayBlockingQueue[Chunk]`, one virtual thread per producer doing
  `queue.put(chunk)` (blocks → backpressure), consumer does `queue.take()`. A sentinel or `AtomicInteger`
  of live producers signals completion. ~25–40 lines. Fair `select` over *multiple* queues with
  backpressure is where the JDK gets awkward (no built-in multi-queue select) — **ox's `select` is real
  value-add**, but plain merge-into-one-queue is trivial without it.

### 2c. `broadcast` / `balance` — 1 producer → N consumers (fan-out / work-steal)

- **ox:** *not* on `Flow` (only `alsoTo` tee). At the channels layer you fork the source into N channels
  (broadcast = copy to each) or 1 shared channel that N consumers `take` from (balance = natural
  work-stealing). ox gives you the channel; you still write the fan-out fork yourself.
- **raw JDK 25:** **balance** is *free* — one bounded queue, N consumer virtual threads all calling
  `take()`; the queue is the work-stealing distributor (~10 lines). **broadcast** = one producer writing
  to N bounded queues, each with its own consumer; a slow consumer backpressures the producer (~20 lines).

  Here **ox barely helps**: balance/broadcast over `BlockingQueue` is almost nothing, and ox has no
  first-class `broadcast` combinator anyway.

### 2d. Scoped resource lifecycle (open on first demand, close on completion / short-circuit / error)

- **ox:** `supervised { val r = useCloseableInScope(open()); … }` — guaranteed `r.close()` after forks
  finish, LIFO, uninterruptible, on every exit path. Or Flow's `mapWithResource(create, close)`.
- **raw JDK 25:** plain `try/finally` in the stream driver, *or* `StructuredTaskScope.close()` semantics
  if you adopt it. ~5–15 lines per resource, but the subtle part (release runs *after* all forks, and
  *uninterruptibly*) you must remember to do. ox's finalizer-LIFO-uninterruptible discipline is a genuine
  correctness aid — modest **value-add** (~15–30 lines saved across a few call sites).

### 2e. Cancellation & error propagation (one fork fails / consumer stops → cancel all siblings)

- **ox:** automatic. `supervised`/`par`/`race` interrupt all siblings on first failure and await them
  before re-throwing; `forkCancellable.cancel()` for explicit cancel.
- **raw JDK 25:** two options. (1) `Executors.newVirtualThreadPerTaskExecutor()` + manual
  `shutdownNow()` (interrupts all) on first failure — you must catch, shut down, await termination,
  and rethrow yourself (~30–50 lines, easy to get wrong: lost exceptions, orphan threads, no "await the
  interrupted siblings"). (2) `StructuredTaskScope` does exactly this — but it's **preview** (§3).

  **This is the second place ox earns real keep.** Deterministic cancel-on-first-error with sibling-await
  is the classic thing hand-rolled code gets subtly wrong. ox (or `StructuredTaskScope`, once final)
  saves ~50 lines *and* a class of bugs.

### 2f. Bounded channel with backpressure (queue between producer fork and consumer)

- **ox:** `Channel.buffered[T](cap)` / `Channel.rendezvous[T]`; `send` blocks when full.
- **raw JDK 25:** `new ArrayBlockingQueue[T](cap)`; `put` blocks when full, `take` blocks when empty, both
  throw `InterruptedException` on cancel, both park-release the virtual-thread carrier. `SynchronousQueue`
  for rendezvous; `LinkedBlockingQueue(cap)` for a linked bounded variant. **~0 lines** — this is a
  drop-in. ox's jox channel adds `select` + first-class closure + a faster lock-free impl, but for a
  single producer→consumer queue **the JDK queue is exactly what we want**, and (critically) it lets us
  make a `Queue[FArray[A]]` whose payload stays unboxed.

### Gap summary

| Need | ox saves vs raw JDK | JDK awkward/unsafe enough to justify a wrapper? |
|---|---|---|
| Bounded channel | ~0 lines | No — `ArrayBlockingQueue` is a drop-in |
| merge (→1 queue) | ~5 lines | No — trivial |
| balance (work-steal) | ~5 lines | No — one queue + N takers is free |
| broadcast (fan-out) | ~15 lines | Marginal — ox has no first-class broadcast anyway |
| scoped resource | ~15–30 lines | Modest — finalizer discipline is a correctness aid |
| **parEvalMap(n) ordered+unordered** | **~40 lines** | **Yes** — collector ordering + cancel-on-stop |
| **cancel-on-first-error + sibling await** | **~50 lines + a bug class** | **Yes** — the classic footgun |
| `select` over N channels with backpressure | N/A (no JDK equivalent) | Yes — but fuse may not need it |

**Net:** for the *queue/merge/balance/broadcast* half, ox saves us **tens of lines** of glue we'd happily
write ourselves (and which we *want* to write ourselves so the payload stays `FArray`). For the
*cancellation correctness* and *ordered parEvalMap* half, ox saves us **~100 lines and a couple of genuine
footguns** — but `StructuredTaskScope` (once final) covers most of that same ground from the JDK. So the
honest gap is ~2–300 lines of careful concurrency code, **not** 2000, and the part worth borrowing is the
*design* of those ~300 lines, not a dependency.

---

## 3. `StructuredTaskScope` reality check (JDK 25)

**It is STILL PREVIEW in JDK 25** — the **5th preview, JEP 505** — and requires `--enable-preview` to
compile and run. The JDK 25 javadoc carries the verbatim note *"StructuredTaskScope is a preview API …
Programs can only use StructuredTaskScope when preview features are enabled."* Package `java.util.concurrent`.

Evolution (corrected from the brief's numbering): JEP 428 incubator (JDK 19) → 453 preview-1 (JDK 21) →
462 preview-2 (**JDK 22**) → 480 preview-3 (**JDK 23**) → 499 preview-4 (**JDK 24**, API unchanged) →
**505 preview-5 (JDK 25), a major redesign**.

**The JEP 505 redesign — what changed and the current shape:**

1. **No public constructors** — you `open()` via static factories.
2. **`ShutdownOnFailure` / `ShutdownOnSuccess` subclasses REMOVED.** Policy is now a **`Joiner`** passed
   to `open()`. You no longer subclass.
3. **`Config` renamed `Configuration`**, applied via a `Function<Configuration,Configuration>`.

```java
// Factories
static <T>   StructuredTaskScope<T,Void> open()                          // default Joiner.awaitAllSuccessfulOrThrow()
static <T,R> StructuredTaskScope<T,R>    open(Joiner<? super T,? extends R> joiner)
static <T,R> StructuredTaskScope<T,R>    open(Joiner<? super T,? extends R> joiner,
                                              Function<Configuration,Configuration> configFn)
// Forking / joining
<U extends T> Subtask<U> fork(Callable<? extends U> task)
<U extends T> Subtask<U> fork(Runnable task)
R    join() throws InterruptedException     // also FailedException, TimeoutException (both preview, unchecked)
void close()                                // cancels scope + interrupts unfinished subtasks + waits

// Built-in Joiners
Joiner.awaitAllSuccessfulOrThrow()   // fail-fast, result Void  (default; ≈ old ShutdownOnFailure)
Joiner.allSuccessfulOrThrow()        // fail-fast, result Stream<Subtask<T>>
Joiner.anySuccessfulResultOrThrow()  // first success wins, cancels rest  (≈ old ShutdownOnSuccess)
Joiner.awaitAll()                    // wait for all, never short-circuit, Void
Joiner.allUntil(Predicate<Subtask<? extends T>>)

// Subtask
Subtask.State state()   // UNAVAILABLE | SUCCESS | FAILED
T         get()         // valid only after join() and state==SUCCESS, else IllegalStateException
Throwable exception()
// Configuration: withThreadFactory(tf) / withTimeout(Duration) / withName(String)
```

**Cancellation/interruption (verbatim semantics):** when a `Joiner` short-circuits, the scope *"prevents
new threads from being started … interrupts the threads executing subtasks that have not completed, and
causes `join` to wakeup."* `close()` *"first cancels the scope … interrupts the threads executing
unfinished subtasks … then waits for all threads to finish."* So it does exactly the deterministic
cancel-on-first-error + sibling-await that fuse needs, via `Thread.interrupt()` (cooperative).

```java
// JDK 25, --enable-preview: fork–join with shutdown-on-first-failure + 5s timeout
try (var scope = StructuredTaskScope.open(
        StructuredTaskScope.Joiner.<Object>awaitAllSuccessfulOrThrow(),
        cfg -> cfg.withTimeout(Duration.ofSeconds(5)))) {
    Subtask<User>       u = scope.fork(() -> findUser(id));
    Subtask<List<Repo>> r = scope.fork(() -> findRepos(id));
    scope.join();                       // throws FailedException on first failure
    return new Result(u.get(), r.get());
}
```

**Does ox shield us from this churn, or mirror it?** **Neither — ox sidesteps it entirely.** ox does not
build on `StructuredTaskScope` at all (it uses its own `ThreadHerd`), precisely *because* the JDK API kept
churning and is still preview. So:

- "Depend on ox to be insulated from JEP churn" is a **valid** but narrow reason — ox gives you a stable
  1.0 API today over plain Loom, with zero preview flags.
- But we can get the *same* insulation by **not using `StructuredTaskScope` ourselves** and instead writing
  our small core directly on `Thread.ofVirtual()` + `ArrayBlockingQueue` + `Semaphore` — all **final since
  JDK 21**, no preview flag. That is the same bet ox made (`ThreadHerd`), just much smaller because our
  needs are narrower.

**Recommendation on `StructuredTaskScope`:** **do not take a hard runtime dependency on it while it is
preview** (preview APIs can change between JDK 25 and 26, and shipping `--enable-preview` to downstream
users of a *library* is hostile). Build the core on the *final* Loom primitives now; adopt
`StructuredTaskScope` later, behind our own tiny interface, once it goes final (likely JDK 26).

---

## 4. Is ox a suitable dependency?

| Axis | Assessment |
|---|---|
| **Maturity / version** | **1.0.5**, post-1.0 since Aug 2025, MiMa-gated stable API. Genuinely mature for a young library. |
| **API churn risk** | Low *now* (1.x, binary-compat gating). It churned a lot pre-1.0; that's behind it. |
| **Transitive footprint** | **Excellent** for core: one dep (`jox`). No cats/ZIO/slf4j in core. Direct-style, matches our no-`IO`-monad stance perfectly. |
| **License** | Apache-2.0 — fine, and friendly for vendoring a subset. |
| **Maintenance** | SoftwareMill's flagship direct-style/Loom library; actively developed, clearly committed. |
| **Scala 3 compat** | Scala 3 only, 3.3 LTS — aligns with this repo. |
| **JDK** | 21+, no preview flags — good. |
| **Fit with a fused, unboxed engine** | **Poor at the boundary.** See below. |

**The fit problem (the decisive axis).** ox's two streaming-boundary types **box every primitive
element**: `Channel[Int]` stores `java.lang.Integer` in jox's `Object[]` segments; `Flow[Int]` boxes
through the erased `FlowEmit.apply(t: T)` callback and at every async channel. The entire point of this
engine is to keep `int[]`/`long[]`/`double[]` unboxed through a fused `while` loop. If we let `Flow[Int]`
or `Channel[Int]` mediate the concurrency boundary, **we box exactly where we were trying to stay unboxed**,
and a per-element async boundary also dwarfs the fusion win with channel/park overhead (~176 ns/op
rendezvous vs a few ns per fused element).

Worse, ox's `Flow` is **push** while fuse is **pull/fused** — adopting `Flow` as the engine type would mean
abandoning the macro-fused `while` loop in favour of `emit` callbacks (re-introducing the `Function1`
indirection fuse was built to eliminate). We would be **fighting the library**: wrapping `FArray` in
`Flow[FArray[A]]` just to get back a pull `Source` via `runToChannel`, paying for jox segments and callback
dispatch we don't need.

**Conclusion:** ox is a *fine dependency in the abstract* (lean, stable, Apache-2.0, direct-style) and an
*excellent study object*, but **its element-level boxed/push channel+flow model undercuts our unboxed,
fused advantage at precisely the boundary we care about.** We would adopt at most its *channel-layer*
ideas, not its `Flow` engine.

---

## 5. How much to steal — concrete recommendation

**Build directly on the JDK (final Loom primitives), keeping the concurrency boundary chunked and
unboxed. Do not depend on ox; do not depend on preview `StructuredTaskScope`. Borrow ox's *designs* for
two pieces (ordered parEvalMap, cancel-on-first-error) — ~250 lines total of our own code.**

### Keep the boundary unboxed: pass `FArray` chunks, not elements

This is the key insight that dissolves the boxing problem. **Every cross-thread queue carries
`FArray[A]` (one `Object` reference per *chunk*), and the chunk stays a primitive array.** A
`BlockingQueue[FArray[Int]]` moves a pointer to an `int[]`-backed `FArray` per chunk — **zero per-element
boxing**, and the per-chunk queue/park cost is amortised over (say) 1k–8k elements. Inside each stage the
fused `while` loop runs unboxed over the chunk exactly as today. This is the same trick ox's *only*
unboxed path uses (`Flow[Chunk[Byte]]`) and the same trick fs2 uses (`Chunk`) — we just make the chunk an
`FArray` and fuse within it. **A `Channel[Int]` would erode our edge; a `Queue[FArray[Int]]` does not.**

### The minimal structured-concurrency core (where each piece comes from)

| Piece | Source | Notes |
|---|---|---|
| **Scope / fork pool** | **JDK** `Executors.newVirtualThreadPerTaskExecutor()` or `Thread.ofVirtual().start(…)` — final since 21. | Wrap in a tiny `scope { fork{…} }` helper (~40 lines) modelled on ox `supervised`/`fork`, with `shutdownNow()` (interrupt-all) + await-termination on exit. |
| **Cancel-on-first-error + sibling await** | **Our helper**, design borrowed from ox `supervised` / `StructuredTaskScope.Joiner.awaitAllSuccessfulOrThrow`. | First fork to throw → `shutdownNow()` → await → rethrow (suppressed-attach secondaries). ~50 lines. The one footgun-prone piece — get it right once, test it hard. |
| **Bounded channel / backpressure** | **JDK** `ArrayBlockingQueue[FArray[A]](cap)` (or `SynchronousQueue` for rendezvous, `LinkedTransferQueue.transfer` for hand-off). | Drop-in. `put`/`take` block + throw `InterruptedException` on cancel + park-release the carrier. Payload `FArray[A]` ⇒ **unboxed**. ~0 lines. |
| **parEvalMap(n) ordered** | **Our helper**, design borrowed from ox `Flow.mapPar`. | `Semaphore(n)` + a FIFO `ArrayBlockingQueue[Future[FArray[B]]](n)`; feeder fork submits, consumer `take().get()`. ~50 lines incl. unordered variant (completion-order enqueue). |
| **merge / balance** | **JDK**, trivial. | merge = N producer forks → 1 `ArrayBlockingQueue`; balance = 1 queue, N taker forks (free work-steal). ~20 lines each. |
| **broadcast** | **JDK**, our helper. | 1 producer → N bounded queues; slow consumer backpressures. ~25 lines. ox has no first-class broadcast anyway. |
| **scoped resource** | **Our helper**, design borrowed from ox `useCloseableInScope`. | LIFO, uninterruptible release after forks join, on every exit (normal / short-circuit / error). ~30 lines. |
| **select over channels** | **Defer.** | Only needed if we do fair multi-source merge; the JDK has no equivalent and ox's `select` is real value-add. If we need it, that's the one place to revisit vendoring jox's algorithm — but most streaming merges don't need fair select. |

**Total: ~250 lines of our own, well-tested concurrency code**, all on **final** JDK 21+ primitives, with
the boundary **unboxed** because it carries `FArray` chunks. No preview flags. No third-party runtime dep.
We own the cancellation semantics (the part that matters), and we keep the fused `while` loop intact
*inside* each chunk.

### Why not just depend on ox for these ~250 lines?

1. **Boxing at the boundary** — its `Channel`/`Flow` box every primitive; using them would defeat the
   engine's reason to exist (mitigated only by *not* using ox's channel and instead our
   `Queue[FArray]` — at which point ox's channel adds nothing).
2. **Push vs pull mismatch** — `Flow` is push/callback; fuse is pull/fused. Adopting `Flow` re-introduces
   the `Function1` indirection we eliminated.
3. **It's a small amount of code** — ~250 lines vs a library + its `jox` dep + a model we'd fight.
4. **No churn shield needed** — ox doesn't use `StructuredTaskScope` either; we get the same stability by
   building on final Loom primitives directly.

### Where ox stays useful

- As a **reference implementation** to copy semantics from (especially `mapPar`'s semaphore+collector and
  `supervised`'s interrupt-and-await-siblings).
- If we ever want **rich resilience** (adaptive retry, rate limiting) or a **fair multi-channel `select`**,
  those are genuine value-add we could pull in *à la carte* (depend on `ox-core` then) rather than
  reimplement — but they are not on the streaming critical path.

---

## Appendix — corrections to assumptions in the brief

- **`StructuredTaskScope` is still PREVIEW in JDK 25** (5th preview, JEP 505), not final. `ScopedValue`
  *is* final (JEP 506).
- The preview→JDK mapping in the brief was off by one: JEP 462 = JDK 22, 480 = JDK 23, 499 = JDK 24.
- **ox does not wrap `StructuredTaskScope`** — it has its own `ThreadHerd` over a virtual-thread
  `ThreadFactory`. So "ox shields us from `StructuredTaskScope` churn" is true only in the sense that ox
  ignores that API entirely.
- ox channels are **not** backed by `java.util.concurrent` queues — they delegate to the custom lock-free
  `jox` library. `ArrayBlockingQueue` etc. appear only as *leaf* primitives in a few ox helpers.
- `OxError[E,F]` is a **scope-capability `case class`**, not an exception/error-value type.
- There is **no `useSupervised`** in ox; resources use `useInScope`/`useCloseableInScope` inside a
  `supervised {}` block.
- `Flow` has **no `broadcast`** combinator (only `alsoTo`/`alsoToTap` tee); broadcast/balance live at the
  channels layer. `mapParUnordered` **does** exist.

## Sources

- ox docs: ox.softwaremill.com/latest/ (structured-concurrency, high-level-concurrency, streaming/flows,
  streaming/channels, utils/{oxapp,resources,rate-limiter,actors}, scheduling/{retries,repeat}).
- ox source (master): `ox/internal/ThreadHerd.scala`, `Ox.scala`, `fork.scala`, `supervised.scala`,
  `par.scala`, `race.scala`, `resource.scala`, `either.scala`, `ErrorMode.scala`, `flow/Flow.scala`,
  `flow/FlowOps.scala`, `flow/FlowRunOps.scala`, `channels/Channel.scala`, `channels/select.scala`,
  `build.sbt` — github.com/softwaremill/ox. Version via `gh release list` (1.0.5).
- jox: github.com/softwaremill/jox; paper arXiv:2211.04986 (Koval/Alistarh/Elizarov, Kotlin channels).
- JDK: JEP 444 (virtual threads, final JDK 21), JEP 491 (no-pin synchronized, JDK 24), **JEP 505
  (StructuredTaskScope, 5th preview, JDK 25)**, JEP 506 (ScopedValue, final JDK 25); JDK 25 javadoc for
  `StructuredTaskScope`/`Joiner`/`Subtask`/`Configuration`/`ScopedValue`/`BlockingQueue`
  (docs.oracle.com/en/java/javase/25/docs/api).
