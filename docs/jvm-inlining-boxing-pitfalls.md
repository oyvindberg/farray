# The JVM performance cliffs behind FArray: inlining, boxing, escape analysis, vectorization

*A sourced, expert-level reference on how HotSpot C2 and the GraalVM JIT actually behave around
method size, megamorphic dispatch, primitive boxing, scalar replacement, and auto-vectorization —
and why an `inline`, per-kind-specialized, array-backed Scala collection beats erased generic
collections in realistic (big-method) code.*

Every numeric default and mechanism below was checked against primary sources (OpenJDK/Graal source,
HotSpot-committer writeups, the Scala runtime). Source URLs are inline; a consolidated list is at the
end. Defaults are **JDK 17–25, 64-bit** unless noted, and were confirmed against OpenJDK `master`.

---

## 0. The one-paragraph thesis

A Scala generic collection's "unboxed" fast path is **not a property of the bytecode** — it is a
*reward the JIT grants* only when a chain of optimizations all succeed at a particular call site:
the higher-order method (`List.foldLeft`, `ArrayOps.map`) must be **inlined**, its `Function`
argument must **devirtualize** to the specialized `apply$mcIII$sp`, and the resulting loop must stay
**call-free and bounds-check-free** so it can vectorize. Each of those steps has a hard cliff —
inlining stops when the *caller* grows past a budget, devirtualization fails when a *shared* call
site sees ≥3 lambda types, scalar replacement bails on a variable array index, vectorization bails on
any call in the loop body. Cross any cliff and the collection silently reverts to a generic
`Object`-typed, boxing, non-vectorized path. FArray moves that fast path from "JIT reward" to
"compile-time fact" by emitting per-primitive-kind, unboxed-signature methods and splicing the user's
lambda with Scala `inline` — so it does not depend on the JIT winning any of those gambles.

---

## 1. C2 inlining: the heuristics and the two ways a *big caller* stops inlining

C2's parse-time inlining policy lives in
[`src/hotspot/share/opto/bytecodeInfo.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/bytecodeInfo.cpp),
in three functions evaluated per call site: `InlineTree::try_to_inline()` (driver),
`should_inline()` (positive filter), `should_not_inline()` (negative filter). Flag defaults are in
[`opto/c2_globals.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/c2_globals.hpp),
[`compiler/compiler_globals.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/compiler/compiler_globals.hpp),
and the per-arch `cpu/.../c2_globals_*.hpp`.

### 1.1 The callee-size gates (hot vs cold)

| Flag | Default | What it gates |
|---|---|---|
| `MaxInlineSize` | **35** bytecodes | Callee bytecode limit at a **cold** call site. (Was 20 on JDK 8.) |
| `FreqInlineSize` | **325** bytecodes (`product_pd`) | Callee bytecode limit at a **hot** call site. 325 on x86-64, aarch64, and in fact all current 64-bit arches. |
| `MaxTrivialSize` | **6** bytecodes | "Always small enough" — trivial callees skip the frequency checks. |
| `InlineSmallCode` | **1000** bytes (`product_pd`; *not* 2000) | A cap on the callee's already-**compiled native** code size when reusing an existing nmethod. Distinct from the bytecode gates. The "2000" in old Oracle docs/blogs is stale. |
| `MaxInlineLevel` | **15** (was **9** on JDK ≤ 13) | Nested inlining depth. Raised 9→15 in **JDK 14** ([JDK-8234863](https://bugs.openjdk.org/browse/JDK-8234863); Renaissance/scala-kmeans motivated it). |
| `MaxRecursiveInlineLevel` | **1** | Recursive (same-method) inlining depth. |

The hot/cold decision is frequency-driven. In `should_inline()`, C2 computes
`freq = call_site_count / caller_invocation_count`; if `freq >= InlineFrequencyRatio` (a *diagnostic*
flag, default **0.25**) it raises the size budget from `MaxInlineSize` (35) to `FreqInlineSize` (325).
The `-XX:+PrintInlining` reason strings map straight to these constants:

- **`"too big"`** — callee bytecode > `MaxInlineSize` (35) at a cold site.
- **`"hot method too big"`** — callee bytecode > `FreqInlineSize` (325) at a hot site.
- **`"already compiled into a big method"`** — callee's *compiled* size > `InlineSmallCode`.
- **`"callee is too large"`** — this is the **C1** (client) limit's message, not C2's; many blogs
  conflate them. C2's are the two above. (Reason-string catalog:
  [JITWatch `SuggestionWalker`](https://github.com/AdoptOpenJDK/jitwatch/blob/master/core/src/main/java/org/adoptopenjdk/jitwatch/report/suggestion/SuggestionWalker.java).)

### 1.2 The two caller-size cliffs (the heart of the "big method" hypothesis)

Neither of these looks at the callee's size at all. They look at how big the *current compilation*
has already grown — which is exactly why **a big hot method stops inlining even trivial callees**:

1. **Accumulated-bytecode cliff — `DesiredMethodLimit` = 8000 bytecodes.** In `try_to_inline()`:
   `if (ClipInlining && (int)count_inline_bcs() + size >= DesiredMethodLimit) set_msg("size > DesiredMethodLimit")`.
   `count_inline_bcs()` is the **caller's running total of already-inlined bytecodes**; once it
   reaches 8000, further inlining is clipped. `ClipInlining` defaults to **true**.
   (`DesiredMethodLimit` is in `compiler_globals.hpp`.)

2. **IR node-count cliff — `NodeCountInliningCutoff` = 18000, `LiveNodeCountInliningCutoff` = 40000.**
   `Compile::over_inlining_cutoff()` returns true once the growing sea-of-nodes graph passes
   `NodeCountInliningCutoff` (≈19 800 with breathing room during incremental inlining); late-inlining
   then bails per-candidate with `"live nodes > LiveNodeCountInliningCutoff"`. `LiveNodeCountInliningCutoff`
   (40000) is the hard ceiling on total live nodes in one method; `MaxNodeLimit` = 80000 is the
   absolute graph cap. (All in `c2_globals.hpp` / `compile.cpp`.)

**Takeaway.** The hypothesis "big methods lose the unboxed fast path" is *mechanically real* and has
two independent triggers — bytecode accumulation (8000) and IR node count (18000) in the caller. As a
generated/inlined method grows, C2 stops folding in the very collection operations whose unboxing
*depends on* being inlined (§3). The cliff is gradual: each additional fold/map that doesn't get
inlined reverts to a boxed call, so allocation climbs monotonically with method size before throughput
falls off — the deterministic `gc.alloc.rate.norm` (B/op) signal is the cleanest way to see it.

> Verification note: all defaults and the two-cutoff structure were confirmed against OpenJDK `master`
> source. `MinInliningThreshold` / `InlineFrequencyCount`, still cited by the OpenJDK wiki and JITWatch,
> have been **removed** from modern HotSpot; the live frequency knobs are `InlineFrequencyRatio` (0.25)
> and `MinInlineFrequencyRatio` (0.0085).

---

## 2. `HugeMethodLimit`: a method too big to compile *at all*

Separate, blunter cliff. `HugeMethodLimit` = **8000** bytecodes (`compiler_globals.hpp`), enforced by
`DontCompileHugeMethods` = **true**. The check is in
[`compilationPolicy.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/compiler/compilationPolicy.cpp)
`CompilationPolicy::can_be_compiled()`:

```cpp
if (DontCompileHugeMethods && m->code_size() > HugeMethodLimit) return false;
```

- `m->code_size()` is the method's **raw bytecode length** (not source lines, not native size). Only
  `> 8000` is rejected; exactly 8000 is fine.
- **It is level-agnostic.** This runs in the generic `can_be_compiled()` before any tier selection,
  so it blocks **C1 *and* C2** — the method is never JIT-compiled at any tier and **runs in the
  interpreter forever**. (Common phrasing "not compiled by C2" undersells it; there is no C1 fallback.)

**How it differs from §1.2.** `DesiredMethodLimit`/`NodeCountInliningCutoff` are *inlining* cliffs —
the method still compiles, it just stops folding callees in. `HugeMethodLimit` is a *compile* cliff —
the whole method drops to the interpreter and loses everything (inlining, EA, vectorization). Same
8000 number, totally different severity.

**Is it hit in practice?** Mostly by **generated code**: huge `switch` statements
([JDK-8366118](https://bugs.openjdk.org/browse/JDK-8366118): a >38 KiB switch even crashed the C2
parser with >1000 `Parse::jump_switch_ranges` frames — illustrating *why* HotSpot refuses huge
methods) and "inline everything into one method" codegen. Spark's whole-stage codegen famously hits it
and ships a `spark.sql.codegen.hugeMethodLimit` guard, recommending **8000** to stay under the JIT cap
([waitingforcode](https://www.waitingforcode.com/apache-spark-sql/generated-method-too-long-be-jit-compiled/read)).

**Direct relevance to FArray.** This is the cliff behind the project's abandoned "inlined-walk"
strategy: splicing a full traversal into every call site emitted ~640 bytecodes/op, so ~8–13 such ops
in one user method blew past 8000 → the whole hot method ran interpreted → catastrophic
(measured 0.27–0.37× of IArray at scale). The hybrid redesign (surface inlines only Empty/One/leaf;
trees call a shared Java traverser) keeps each op compact (~30–60 bytecodes) precisely to stay far
under this limit. **The fast path that beats competitors and the cliff that would destroy FArray are
the same 8000-byte number** — which is why "measure the per-op bytecode footprint" is a load-bearing
discipline here.

---

## 3. Why erased Scala collections box — and why it's conditional on inlining

### 3.1 The double-erasure indirection

A Scala higher-order collection op erases **two** boundaries to `Object`:

1. The element type — `List[A].foldLeft`, `ArrayOps.map` operate on `Object`; signature is roughly
   `foldLeft(Object, Function2): Object`.
2. The operation itself — passed as a `Function1`/`Function2` SAM whose generic `apply(Object…)` also
   takes and returns `Object`.

Java's `int[]` + `IntStream` primitive path has no SAM-erasure layer for the common case; Scala's does,
which is why **Scala generics box more readily than Java**.

### 3.2 The specialized SAM, and the exact boxing site

Scala generates specialized function forms `JFunction2$mcIII$sp` (naming: `mc<Ret><Arg1><Arg2>$sp`,
JVM type codes I/J/D/Z/…). The Scala 2.13 runtime source
([`JFunction2$mcIII$sp.scala`](https://github.com/scala/scala/blob/v2.13.15/src/library/scala/runtime/java8/JFunction2%24mcIII%24sp.scala))
is literally:

```scala
def apply$mcIII$sp(v1: Int, v2: Int): Int          // unboxed — no allocation
override def apply(v1: Any, v2: Any): Any =          // the boxing bridge
  scala.runtime.BoxesRunTime.boxToInteger(
    apply$mcIII$sp(BoxesRunTime.unboxToInt(v1), BoxesRunTime.unboxToInt(v2)))
```

So the primitive stays unboxed **only when the caller invokes `apply$mcIII$sp` directly.** That
happens only when the collection method is **inlined/devirtualized** at the site so the *static* type
at the call is the specialized SAM. If the call goes through the erased `Function2.apply(Object,Object)`
— the un-inlined or megamorphic case (§5) — it routes through that boxing bridge, allocating a boxed
`Integer`/`Long`/… **per element**.

> This is the crux: **the same lambda is unboxed or boxed depending entirely on whether the call site
> was inlined.** Unboxing is not a property of the bytecode; it's a property of the JIT's success.

### 3.3 Why the standard library can't just `@specialize` its way out

`@specialized` generates one class per primitive (~10 variants incl. the erased reference case), so
for **N** type parameters it explodes **~10^N**: `Tuple3` would emit **1000** classes vs **8** with
miniboxing (Ureche et al., [miniboxing](http://scala-miniboxing.org/intro.html);
[paper](https://infoscience.epfl.ch/record/200245)). That blowup is *why* `scala.collection` stays
erased/boxed and `@specialized` is reserved for a tiny whitelist (`Function0/1/2`, `Tuple1/2`,
`Product1/2`). It's also why the ecosystem ships hand-specialized primitive collections (debox, spire,
breeze) — and the motivation for FArray's per-kind code generation.

**The cost of boxing** (Li Haoyi, [Benchmarking Scala Collections](https://www.lihaoyi.com/post/BenchmarkingScalaCollections.html)):
a boxed `Integer` is a 16-byte header + 4-byte payload reached via a separate pointer; `Array[Int]` uses
~**4–5×** less memory than `Array[java.lang.Integer]`; `List`-of-primitive is ~**2×** a boxed
`Array[Object]` on top of that. Deboxing generics is measured at **1.5×–22×** faster (miniboxing; 22× on
Int).

---

## 4. Escape analysis & scalar replacement: what it can't eliminate

HotSpot doesn't stack-allocate; it does **Scalar Replacement of Aggregates (SRA)** — a non-escaping
object's fields become synthetic local scalars and the allocation vanishes
([Shipilev, JVM Anatomy Quark #18](https://shipilev.net/jvm/anatomy-quarks/18-scalar-replacement/)).
Flags: `-XX:+DoEscapeAnalysis`, `-XX:+EliminateAllocations` (both default on); `-XX:+PrintEliminateAllocations`
is **debug-build only**. Logic in
[`escape.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/escape.cpp) /
[`macro.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/macro.cpp).

### 4.1 The escape lattice, and why inlining is a *precondition* for EA

From [`escape.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/escape.hpp):

- `NoEscape` — "does not escape … not passed to call. **Could be replaced with scalar.**" (only this is eliminable)
- `ArgEscape` — passed as an argument to a call (but doesn't escape *during* the call).
- `GlobalEscape` — escapes the method/thread.

A fresh object **passed as an argument to a call is at best `ArgEscape` → not scalar-replaceable** —
*unless that callee is inlined*, which collapses the argument-pass and re-classifies it `NoEscape`.
EA needs the allocation and all its uses visible in **one compilation unit** (the method + its inlined
callees). Therefore **a non-inlined or megamorphic call defeats EA**: the object escapes into the
opaque call and stays heap-allocated (Chris Seaton,
[Seeing Escape Analysis Working](https://chrisseaton.com/truffleruby/seeing-escape-analysis/);
GraalVM blog, [Under the hood of GraalVM JIT optimizations](https://medium.com/graalvm/under-the-hood-of-graalvm-jit-optimizations-d6e931394797)
— "an inliner cannot proceed … the call is not direct … the inliner does not know what to inline,"
so the object stays escaped). C2 EA is additionally **all-or-nothing and flow-insensitive**: a single
control-flow merge where the object reference is ambiguous defeats the whole elimination
(Shipilev #18's `split()` = 16 B/op vs `single()` = 0 B/op).

### 4.2 The killer for array-backed code: variable array index ⇒ not scalar-replaceable

**HotSpot cannot scalar-replace an array allocation accessed with a non-constant (variable) index —
even when the array is provably `NoEscape`.** Confirmed in `macro.cpp`: during `can_eliminate_allocation`,
a non-constant access offset yields `Type::OffsetBot`, which trips the
`"Undefined field reference"` bailout (there's also a separate `"Array's size is not constant"` bailout).
Scalar replacement maps each *statically known* slot to a local; a loop index isn't statically known,
so it can't. Demonstrated empirically by
[jpbempel, "When Escape Analysis fails you?"](https://jpbempel.github.io/2020/08/02/when-escape-analysis-fails-you.html):
the *same* small array is fully eliminated when accessed by constant index `a[0], a[1]` but **not**
when iterated in a loop — *"there are no scalar replaceable candidates"* even though *"all objects are
NoEscape!"* The only escape hatch is **full loop unrolling** turning indices into constants (small/fixed
sizes; improved JDK 15+ via JDK-8231291). The Microsoft stack-allocation JEP confirms the same for
non-constant array *size*.

> **Why this matters for FArray's `flatMap`.** A `flatMap(x => FArray(x, x+1))` builds a fresh 2-element
> `IntArr` (a wrapper + a backing `int[2]`) per element. EA scalar-replaces the **wrapper** but **not the
> backing array** (read by a variable loop index), so each call leaks one `int[]` per element. We measured
> this precisely with `-XX:-DoEscapeAnalysis`: EA-on B/op equalled *exactly* the array bytes (Int: 70 000
> arrays × 24 B = 1.68 MB/op), EA-off was double (both objects). The fix — splatting a literal
> `FArray(e0, e1)` flatMap body to `body(e0); body(e1)` with **no array at all** — took it to 0 B/op and
> ~3× throughput. This is §4.2 applied directly.

### 4.3 Graal partial escape analysis (PEA): flow-sensitive, strictly stronger

C2's "one escaping branch kills it" is the limitation PEA removes. Graal keeps objects **virtual on
non-escaping paths** and **materializes** them only on the branch that escapes
(Stadler, Würthinger, Mössenböck,
[Partial Escape Analysis and Scalar Replacement for Java](https://ssw.jku.at/Research/Papers/Stadler14/Stadler2014-CGO-PEA.pdf),
CGO 2014 — measured **up to −58.5% allocated memory, +33% performance**, *"especially well … code
generated by the Scala compiler"*). PEA is the documented reason Graal dissolves boxing inside larger
inlined methods that C2 leaves allocated, and the main contributor to Graal's 5× on the GraalVM blog's
example.

---

## 5. Call-site profiling: monomorphic → bimorphic → megamorphic

The number that fixes the cliff at **3 types**: `TypeProfileWidth` = **2**
([`globals.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/runtime/globals.hpp),
range 0–8 — "Number of receiver types to record"). With `UseBimorphicInlining` = **true**
(`c2_globals.hpp`), C2 can guard-inline **up to 2** receiver types; a **3rd distinct type saturates the
profile → megamorphic → real vtable/itable call, not inlined** (Shipilev,
[Black Magic of (Java) Method Dispatch](https://shipilev.net/blog/2015/black-magic-method-dispatch/);
Richard Warburton, "Too Fast, Too Megamorphic").

- **Monomorphic** (1 type): inlined behind one type guard; virtual ≈ static when monomorphic
  (29.17 vs 30.17 µs/op in Black Magic).
- **Bimorphic** (2 types): two guarded inlines; cost dominated by **branch misprediction**, not dispatch
  (worst at 50/50 bias).
- **Megamorphic** (≥3): vtable stub, no inlining. **≈3.3× slower** than monomorphic — 1070.3 vs 325.5
  ns/op ([Shipilev, JVM Anatomy Quark #16](https://shipilev.net/jvm/anatomy-quarks/16-megamorphic-virtual-calls/)).
  The bigger real cost is the **lost inlining** (and everything downstream: EA, vectorization, unboxing),
  not the indirect jump itself.

**This is the mechanism that makes one shared `Array.map` box.** A single internal `f.apply(x)` site
inside one shared `ArrayOps.map` that sees **many distinct lambda classes** across a program saturates
its 2-wide profile → megamorphic → generic boxed `Function.apply(Object)`. **Inlining `map` per call
site** gives each lambda its **own monomorphic `apply` site** → devirtualized to `apply$mcIII$sp` →
unboxed, and (§6) the loop stays call-free and vectorizable. Ionut Balosin measured exactly this in
Kotlin: marking the wrapper `inline` turns one 4-type megamorphic site into 4 monomorphic direct calls,
**~2× faster** (10.78 → 5.23 ns/op,
[link](https://ionutbalosin.com/2019/03/kotlin-explicit-inlining-at-megamorphic-call-sites-pays-off-in-performance/)).
**FArray's "every lambda-taking op is `inline`" is precisely this trick** — each user lambda gets a
private monomorphic site instead of crushing one shared site megamorphic.

---

## 6. Auto-vectorization (SuperWord): the loop must stay clean

SuperWord (C2 only, `-XX:+UseSuperWord` default on;
[`superword.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/superword.cpp))
packs adjacent isomorphic scalar ops into SIMD, working on **basic blocks after loop unrolling**
([Emanuel Peter — current SuperWord maintainer — "SuperWord: An Introduction"](https://eme64.github.io/blog/2023/02/23/SuperWord-Introduction.html)).
It vectorizes **counted loops over primitive arrays with straight-line bodies**. It **bails** on:
control flow / `if` in the body, **any method call in the body**, non-counted loops, loop-carried
dependencies, gather/scatter access. Flags: `MaxVectorSize` (64), `LoopMaxUnroll` (16), `UseAVX`.

So the §5 megamorphic `Function.apply` is a *double* blocker: it's a call (kills SuperWord) **and**
boxes (§3). And boxing's per-element heap writes are themselves un-vectorizable. They compound.

**Bounds checks and why `a.length` matters.** Every array access is range-checked. To vectorize, that
check must be hoisted out of the body. HotSpot does **range-check elimination** when the array length
and index bounds are **loop-invariant**, splitting into pre/main/post loops where the **main loop is
check-free** and is the only SuperWord candidate (Red Hat,
[Range check elimination in loops](https://developers.redhat.com/articles/2022/03/16/range-check-elimination-loops-openjdks-hotspot-jvm)).
Iterating against the **backing array's own `a.length`** lets the JIT prove invariance and hoist the
check; an **opaque, separately-stored length field** that the JIT can't prove equals the array's true
length defeats the proof, leaving a per-element bounds check in the body that **blocks vectorization**.
(This is the mechanism behind FArray's generated leaf loops bounding on `a.length`, not `leaf.length`.)

---

## 7. GraalVM vs C2: why Graal holds the unboxed path in bigger methods

GraalVM's JIT (JVMCI; option prefix `-Djdk.graal.*`, formerly `-Dgraal.*`; inspect with
`-XX:+JVMCIPrintProperties`) differs from C2 on every axis above. Defaults from
[`GraalOptions.java`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.graal.compiler/src/jdk/graal/compiler/core/common/GraalOptions.java):

- **Budgets in IR graph nodes, not bytecode bytes.** `MaximumInliningSize` = **300** nodes/call-site
  (vs C2's 35/325 *bytes*), `MaximumDesiredSize` = **20000** nodes total, `TrivialInliningSize` = **10**,
  `SmallCompiledLowLevelGraphSize` = **330**, `MaximumRecursiveInlining` = **5**. The inliner is an
  exploratory cost/benefit model that inlines deeply *to expose* cross-method optimization
  (Prokopec, GraalVM blog) — the opposite posture to C2's conservative byte gates.
- **Polymorphic (duplication-based) inlining.** Graal keeps a receiver-type profile for indirect call
  sites and emits a type-switch over the profiled receivers, inlining *through* virtual/megamorphic
  calls that C2 leaves as a call.
- **Partial escape analysis** (`PartialEscapeAnalysis` = true) — §4.3. Flow-sensitive; dissolves boxing
  C2 leaves allocated.
- Oracle GraalVM adds `UsePriorityInlining` (default true) and `TuneInlinerExploration` ∈ [−1, 1].

**Consequence, which we measured.** Graal keeps the unboxed/specialized path inlined inside **larger**
generated methods than C2, and its PEA mops up residual boxing — so the "big-method boxing cliff"
appears **later and gentler on Graal than on C2**. In our per-primitive pipeline benchmark, eager FArray
beat the best competitor **1.4–2.6× on Graal but 3.6–7.9× on C2** — same code, harsher cliff on the
non-JVMCI JIT most people run. **Lesson: benchmark both.** A C2 "boxing loss" can simply vanish under
Graal PEA, and a Graal win can overstate the margin a C2 user sees.

---

## 8. How FArray sidesteps each cliff (the design, mapped to §§1–7)

| JVM cliff (§) | What it does to erased generics | FArray's structural answer |
|---|---|---|
| Inlining stops in big caller (§1.2) | Un-inlined `foldLeft`/`map` → generic boxed call | Op signature is already unboxed (`reduceLeafFwdIntInt(FBase, int, IntToIntFold): int`); no inlining needed to be unboxed |
| `HugeMethodLimit` (§2) | Inlined-everything codegen → method interpreted forever | Compact per-op footprint (~30–60 bytecodes): surface inlines Empty/One/leaf, trees call a shared Java traverser — stays far under 8000 |
| Erased SAM boxes (§3) | `Function2.apply(Object,Object)` + `BoxesRunTime` per element | Per-kind specialized SAM (`int apply(int,int)`); the unboxed `$sp`-equivalent is the *only* signature |
| Megamorphic shared site (§5) | One shared `map`'s `f.apply` sees N lambdas → megamorphic → box | Every lambda-taking op is Scala `inline`: each user lambda gets a private monomorphic site |
| Variable-index array EA (§4.2) | n/a for them; bites FArray's own `flatMap` | Literal-`FArray` flatMap splat emits `body(e0); body(e1)` — no inner array to fail to eliminate |
| Vectorization blockers (§6) | Boxed/megamorphic body → no SIMD | Leaf loop bounds on backing `a.length` (range-check elimination) with a call-free body |

The throughline: **competitors' unboxed speed is a JIT reward contingent on inlining; FArray makes it a
compile-time fact.** That's why the gap is largest exactly where the JIT's gambles fail — big, realistic,
method-heavy code — and on the harsher (C2) JIT.

---

## Sources

**OpenJDK / HotSpot source (primary).**
[`opto/bytecodeInfo.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/bytecodeInfo.cpp) ·
[`opto/c2_globals.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/c2_globals.hpp) ·
[`compiler/compiler_globals.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/compiler/compiler_globals.hpp) ·
[`compiler/compilationPolicy.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/compiler/compilationPolicy.cpp) ·
[`opto/escape.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/escape.hpp) ·
[`opto/macro.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/macro.cpp) ·
[`opto/superword.cpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/superword.cpp) ·
[`runtime/globals.hpp`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/runtime/globals.hpp) ·
[JDK-8234863 (MaxInlineLevel 9→15)](https://bugs.openjdk.org/browse/JDK-8234863) ·
[JDK-8366118 (huge-switch / DontCompileHugeMethods)](https://bugs.openjdk.org/browse/JDK-8366118).

**Aleksey Shipilëv (HotSpot committer).**
[Black Magic of (Java) Method Dispatch](https://shipilev.net/blog/2015/black-magic-method-dispatch/) ·
[JVM Anatomy Quark #16: Megamorphic Virtual Calls](https://shipilev.net/jvm/anatomy-quarks/16-megamorphic-virtual-calls/) ·
[#18: Scalar Replacement](https://shipilev.net/jvm/anatomy-quarks/18-scalar-replacement/).

**Escape analysis / vectorization (practitioner + vendor).**
[jpbempel — When Escape Analysis fails you](https://jpbempel.github.io/2020/08/02/when-escape-analysis-fails-you.html) ·
[Chris Seaton — Seeing Escape Analysis Working](https://chrisseaton.com/truffleruby/seeing-escape-analysis/) ·
[Emanuel Peter — SuperWord: An Introduction](https://eme64.github.io/blog/2023/02/23/SuperWord-Introduction.html) ·
[Red Hat — Range check elimination](https://developers.redhat.com/articles/2022/03/16/range-check-elimination-loops-openjdks-hotspot-jvm) ·
[Ionut Balosin — Kotlin explicit inlining at megamorphic call-sites](https://ionutbalosin.com/2019/03/kotlin-explicit-inlining-at-megamorphic-call-sites-pays-off-in-performance/).

**GraalVM.**
[GraalOptions.java (defaults)](https://github.com/oracle/graal/blob/master/compiler/src/jdk.graal.compiler/src/jdk/graal/compiler/core/common/GraalOptions.java) ·
[Java HotSpot options reference](https://www.graalvm.org/latest/reference-manual/java/options/) ·
[Prokopec — Under the hood of GraalVM JIT optimizations](https://medium.com/graalvm/under-the-hood-of-graalvm-jit-optimizations-d6e931394797) ·
[Stadler et al. — Partial Escape Analysis (CGO 2014)](https://ssw.jku.at/Research/Papers/Stadler14/Stadler2014-CGO-PEA.pdf).

**Scala boxing / specialization.**
[`JFunction2$mcIII$sp.scala`](https://github.com/scala/scala/blob/v2.13.15/src/library/scala/runtime/java8/JFunction2%24mcIII%24sp.scala) ·
[Scala optimizer docs](https://docs.scala-lang.org/overviews/compiler-options/optimizer.html) ·
[Miniboxing (Ureche et al.)](http://scala-miniboxing.org/) ·
[Improving Scala Collections with Miniboxing (paper)](https://infoscience.epfl.ch/record/200245) ·
[Li Haoyi — Benchmarking Scala Collections](https://www.lihaoyi.com/post/BenchmarkingScalaCollections.html).

**Spark (HugeMethodLimit in the wild).**
[waitingforcode — Generated method too long to be JIT compiled](https://www.waitingforcode.com/apache-spark-sql/generated-method-too-long-be-jit-compiled/read).

---

*Confidence: the numeric defaults and source-file locations in §§1–7 were each confirmed against
primary source and survived an adversarial fact-check (6/7 load-bearing claims CONFIRMED; the 7th —
HugeMethodLimit — corrected from "no C2" to "no JIT at any tier, C1 included"). The FArray-specific
measurements in the call-out boxes are from this repo's own benchmarks, not the literature.*
