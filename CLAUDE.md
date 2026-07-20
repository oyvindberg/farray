# FArray

> **⛔ NO DESTRUCTIVE GIT OPERATIONS.** Claude is **NOT allowed** to run destructive git commands
> (`git checkout -- <file>`, `git checkout .`, `git reset --hard`, `git clean -f`, `git restore`,
> `git stash drop`/`git stash clear`, force-push, branch deletion). These throw away work
> irrecoverably. **To revert / set work aside, use `git stash` — stash is free and unlimited; keep as
> many stashes as you want.** Want a clean tree to measure a baseline? `git stash push` it, do the
> measurement, then `git stash pop` to bring the change back. Never `git checkout` a file to discard
> edits, and never drop/clear a stash.

`FArray[+A]` — an immutable, `Array`-backed sequence whose goal is to **beat every competitor**
(`IArray`, `List`, `Vector`, `fs2.Chunk`, `zio.Chunk`, `kyo.Chunk`) on as many operations as possible while
keeping the full `IndexedSeq` API.

## Goals & principles

- **No boxing of primitives.** `opaque type FArray[+A] <: AnyRef = FBase` (zero wrapper allocation);
  per-element-kind specialization (Int/Long/Double/Ref); every lambda-taking op is `inline` with an
  `inline` function parameter, so the user's lambda inlines and primitives stay unboxed — the same
  machinery as `map`/`foldLeft`. Surface ops that call into specialized impls must be `inline` so the
  kind dispatch resolves at the concrete call site.
- **Hybrid representation.** Flat primitive-array leaves (`${K}Arr`) **plus** lazy structural tree
  nodes (`Concat`, `Append`, `Prepend`, `SliceNode`, `ReverseNode`, `Pad`, `Updated`, `One`, `Empty`,
  `RangeNode`) so structural ops (`++`, `take`/`drop`, `reverse`, `:+`, …) are O(1). Traversal is **one
  direction-aware DFS** — forward/backward drivers are mutual mirrors that flip at each `ReverseNode`.
- **Beat Chunk at minimum.** Losing to `IArray` on inherently-allocating ops (e.g. construction must
  allocate more than a raw array) is acceptable; **losing to `fs2.Chunk`/`zio.Chunk`/`kyo.Chunk` is not.** Treat
  `IArray` ≈ `Array` as the same raw-array baseline.
- **Measure, don't assume.** Performance claims are validated by benchmarks, never by reasoning about
  the JIT. Several "obvious" optimizations here were committed then reverted once measurement disproved
  the premise — the real bottleneck is often not what it looks like (e.g. a short-circuit gap that was
  the per-element *loop shape*, not virtual dispatch). Hard-won findings live in the project memory and
  the git log; check there before re-deriving.
- **Correctness = parity with `List`.** Every op is tested against the equivalent `List` operation in
  `tests/` (`FListTest`). Keep it green: `bleep test tests`.

## Benchmark-driven workflow

The scorecard **is the checked-in JSON** under `docs/`: `bench-results.json` (farray suite) and
`set-bench-results.json` (fset suite) — raw JMH numbers, rendered by the site (`site/`, a Docusaurus
app; pages `/benchmarks/farray` and `/benchmarks/fset`: W/T/L per op × size, ≥1.05× win, 0.95–1.05×
tie, <0.95× loss).
There is NO generated HTML report anymore. Treat the data like code: **re-measure and commit it
alongside the change that moved the numbers.**

**Full guide: `docs/benchmarking.md`** — the one-liner for a new machine (`bash scripts/bench-all.sh`,
sequential, both suites, all competitors), every knob, and the traps (shard contention ≈1.26× median,
FArray's longer warmup, the fresh-checkout preflight).

**Fast round-trip (the common loop).** Because `docs/bench-results.json` already exists, the runner
auto-selects **farray-only patch mode**: it re-measures *only* the `farray_*` methods and patches them
into the cached results, keeping every competitor entry untouched. So iterating on FArray never pays to
re-run the competitors — edit `GenCores.scala`, run the sweep, read the refreshed report, repeat.

```
caffeinate -i bash scripts/bench-run.sh [warmup-iters] [measure-iters] [forks] [max-shards]
#   typical:  caffeinate -i bash scripts/bench-run.sh 3 5 1 6
```
- **NEVER `bleep compile`/`bleep test` while a JMH run is live.** Recompilation swaps class files
  under the running forks' classpath and JMH silently truncates the run (exits 0 with a partial
  matrix — diagnosed 2026-07-04, two runs lost). Queue compiles behind the lock, or wait.
- **One JMH consumer per box — `scripts/bench-lock.sh`.** Multiple agents/sessions may work here
  concurrently; two JMH runs contend and both measure garbage (and invite pkill collateral). The sweep
  runners (`bench-run.sh` / `setbench-run.sh`) take the mutex automatically; **wrap ad-hoc runs
  yourself**: `scripts/bench-lock.sh bleep run setbenchmarks-runner -- '<regex>' -f 1 ...`. The lock
  waits (never fails), and steals a lock whose holder died. NEVER `pkill` JMH — the lock makes it
  unnecessary.
- Defaults: 3 warmup / 5 measure / 0 forks / 6 concurrent shards. `0` forks = fast & noisier
  (in-shard JVM); `1` fork = more stable. `max-shards` caps peak memory (~2g per `-f1` fork).
- Drives `org.openjdk.jmh.Main` as N parallel java processes (not `bleep run`, which serializes on the
  build server); captures the real classpath from one live `bleep run`.
- Ends at the refreshed `docs/bench-results.json` — view it via the site (`cd site && npm run dev`),
  and always check the refreshed JSON back in.
- Delete `docs/bench-results.json` to force a full-suite run (re-measures competitors too).
- `BENCH_VARIANT=<methodPrefix>` generalizes the patch to any one implementation — how a NEWLY ADDED
  competitor gets folded into an existing scorecard without re-measuring the field:
  `BENCH_VARIANT=kyochunk caffeinate -i bash scripts/bench-run.sh 3 5 1 6`. Its cells were measured
  under different contention than the entries beside them, so confirm close calls with `-f 2`.
- Beware under-warmed-up artifacts in a fast sweep; confirm a surprising number with `-f 2`/more iters
  before acting on it.

**Single op, ad-hoc:**
```
bleep run benchmarks-runner -- '<regex>' -f 1 -wi 3 -i 5 -p size=100000
```

## Codegen + the bleep build

FArray's implementation is **generated** — do NOT edit generated sources; edit the generator.

- `codegen/src/scala/farray/GenCores.scala` is a `BleepCodegenScript` that emits (a) the sealed Java
  `FBase` core hierarchy and (b) `FArrayOps` — every per-kind-specialized op. In `bleep.yaml` the
  `farray` project declares it as a generate step (`generate: { project: codegen, main: farray.GenCores }`),
  so `bleep` runs `GenCores` before compiling `farray`; output lands under `.bleep/generated-*/farray/`.
- **To change or add an op: edit `GenCores.scala`, then build** — the codegen re-runs and regenerates.
  Specialization is driven by `opKinds` (Int/Long/Double/Ref); `dispatchA`/`dispatchB` pick the kind via
  `summonFrom` on `${K}Repr[A]`.
- `farray/src/scala/farray/FArray.scala` (the opaque-type surface + `FArraySeq`) is hand-written and
  thin; the heavy lifting is the generated `FArrayOps`.

### Project layout (`bleep.yaml`)
- `codegen` — the `GenCores` generator.
- `farray` — the library (generated `FBase`/`FArrayOps` + hand-written `FArray.scala`).
- `example-json-decoder` (dependsOn `farray`) — the NDJSON record decoder as a downstream module.
  Pipelines are shape-indexed (`Fuse[A, S]`); terminal SYNTAX lives once in farray's capability traits
  (`AggTerminals`/`SearchTerminals`/`GroupTerminals`/`PlanTerminals`/`MaterializeTerminals`,
  bundle `StandardTerminals`), every method funneling into the shape lowering's single abstract
  `inline def lower(self, t: Terminal[A, R])` hook. A module = shape + given + hook whose macro calls
  `FuseMacro.lower(self, t, itsRecordDecoder)` — see `example-json-decoder/…/Integration.scala` (the
  whole plug-in, one file; design: docs/fusion-integration-design.md). No registry, no reflection.
- `tests` (dependsOn `farray`, `example-json-decoder`) — `FListTest`, parity vs `List`.
- `benchmarks` / `benchmarks-runner` — JMH suites, driven by `scripts/bench-run.sh`.

### Build notes
- `bleep test tests` — compile + run the parity tests.
- The MCP `bleep` builds the main checkout (`/Users/oyvind/pr/farray`) only; build a git worktree with
  the CLI inside it (`cd <worktree> && bleep …`).
