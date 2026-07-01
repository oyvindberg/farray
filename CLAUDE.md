# FArray

> **⛔ NO DESTRUCTIVE GIT OPERATIONS.** Claude is **NOT allowed** to run destructive git commands
> (`git checkout -- <file>`, `git checkout .`, `git reset --hard`, `git clean -f`, `git restore`,
> `git stash drop`/`git stash clear`, force-push, branch deletion). These throw away work
> irrecoverably. **To revert / set work aside, use `git stash` — stash is free and unlimited; keep as
> many stashes as you want.** Want a clean tree to measure a baseline? `git stash push` it, do the
> measurement, then `git stash pop` to bring the change back. Never `git checkout` a file to discard
> edits, and never drop/clear a stash.

`FArray[+A]` — an immutable, `Array`-backed sequence whose goal is to **beat every competitor**
(`IArray`, `List`, `Vector`, `fs2.Chunk`, `zio.Chunk`) on as many operations as possible while
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
  allocate more than a raw array) is acceptable; **losing to `fs2.Chunk`/`zio.Chunk` is not.** Treat
  `IArray` ≈ `Array` as the same raw-array baseline.
- **Measure, don't assume.** Performance claims are validated by benchmarks, never by reasoning about
  the JIT. Several "obvious" optimizations here were committed then reverted once measurement disproved
  the premise — the real bottleneck is often not what it looks like (e.g. a short-circuit gap that was
  the per-element *loop shape*, not virtual dispatch). Hard-won findings live in the project memory and
  the git log; check there before re-deriving.
- **Correctness = parity with `List`.** Every op is tested against the equivalent `List` operation in
  `tests/` (`FListTest`). Keep it green: `bleep test tests`.

## Benchmark-driven workflow

The benchmark report **is the scorecard and is checked in** under `docs/`:
`bench-results.json` (raw JMH numbers) + `index.html` (rendered W/T/L charts — farray vs every
competitor, per op × size: ≥1.05× win, 0.95–1.05× tie, <0.95× loss). Treat the report like code:
**regenerate and commit it alongside the change that moved the numbers.**

**Fast round-trip (the common loop).** Because `docs/bench-results.json` already exists, the runner
auto-selects **farray-only patch mode**: it re-measures *only* the `farray_*` methods and patches them
into the cached results, keeping every competitor entry untouched. So iterating on FArray never pays to
re-run the competitors — edit `GenCores.scala`, run the sweep, read the refreshed report, repeat.

```
caffeinate -i bash scripts/bench-run.sh [warmup-iters] [measure-iters] [forks] [max-shards]
#   typical:  caffeinate -i bash scripts/bench-run.sh 3 5 1 6
```
- Defaults: 3 warmup / 5 measure / 0 forks / 6 concurrent shards. `0` forks = fast & noisier
  (in-shard JVM); `1` fork = more stable. `max-shards` caps peak memory (~2g per `-f1` fork).
- Drives `org.openjdk.jmh.Main` as N parallel java processes (not `bleep run`, which serializes on the
  build server); captures the real classpath from one live `bleep run`.
- **Always regenerates `docs/index.html`** (`scripts/bench_report.py`) at the end — always check the
  refreshed report back in.
- Delete `docs/bench-results.json` to force a full-suite run (re-measures competitors too).
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
- `tests` (dependsOn `farray`) — `FListTest`, parity vs `List`.
- `benchmarks` / `benchmarks-runner` — JMH suites, driven by `scripts/bench-run.sh`.

### Build notes
- `bleep test tests` — compile + run the parity tests.
- The MCP `bleep` builds the main checkout (`/Users/oyvind/pr/farray`) only; build a git worktree with
  the CLI inside it (`cd <worktree> && bleep …`).
