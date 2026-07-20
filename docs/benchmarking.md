# Benchmarking FArray

The scorecard **is** the checked-in JSON. `docs/bench-results.json` (the farray suite) and
`docs/set-bench-results.json` (the fset suite) are raw JMH output, rendered by the site at
`/benchmarks/farray` and `/benchmarks/fset`. There is no generated HTML report. Treat the data like
code: re-measure and commit it alongside the change that moved the numbers.

## Re-measure everything on a new machine

```
git clone <repo> && cd farray
bash scripts/bench-all.sh
```

That is the whole thing — both suites, every competitor, one JMH process at a time, ending at the two
result files. It self-wraps in `caffeinate -is` on macOS, moves any existing scorecards aside into
`docs/.full-run-backup-<timestamp>/` (never deletes them), and prints an ETA for your settings.

Prerequisites: a JDK (the build fetches GraalVM CE 25 itself), the `bleep` CLI, and — if you want to
look at the results — Node for the site. **Plug the machine in and leave it alone**; anything else
using the CPU perturbs the measurement.

To view the results:

```
cd site && npm install && npm run data && npm run dev    # → http://localhost:3000/farray/
```

## Knobs

All are environment variables on `scripts/bench-all.sh`:

| knob | default | what it does |
|---|---|---|
| `WI` | `5` | warmup iterations per trial. **Do not lower for a scorecard run** — see below. |
| `MI` | `8` | measurement iterations per trial |
| `FORKS` | `1` | JVM forks per benchmark. `1` = clean JVM each (what the checked-in numbers use). `0` = reuse the shard JVM: faster, noisier, cross-benchmark JIT pollution. `2+` = confirming a surprising cell. |
| `SHARDS` | `1` | max concurrent JMH processes. `1` = sequential. Raising it invalidates comparability — see below. |
| `SUITES` | `both` | `both` \| `farray` \| `fset` |
| `XMX` | `1g` | heap per JMH process; raise on OOM at the 100k sizes |
| `TIME_PER_IT` | `0.3` | seconds per iteration (`-r`/`-w`) |

```
SUITES=farray bash scripts/bench-all.sh              # one suite
WI=10 MI=10 FORKS=2 bash scripts/bench-all.sh        # highest confidence, slowest
SHARDS=6 FORKS=0 bash scripts/bench-all.sh           # fast + noisy; never commit these
```

## The two rules that make numbers comparable

**Never mix shard counts.** Parallel shards contend, and the effect is big and one-directional.
Measured 2026-07-20 on identical code, 6 shards vs 1: the same benchmarks moved by a **median 1.26×**
(p90 1.66×, worst 2.85×) in 115 of 119 cells. That dwarfs most margins the scorecard reports, so a
contended cell cannot sit in a table next to an uncontended one. Parallel runs are for exploration
only, and are comparable solely against other runs at the same shard count.

**Give FArray enough warmup.** FArray reaches peak later than its rivals. At `WI=3` with 300 ms
iterations, some FArray cells measured up to **10× low** while competitors in the same run were
already at steady state — a bias that works *against* FArray and looks like a regression. `WI=5` is
the floor for anything you intend to commit; confirm surprising cells with `FORKS=2 WI=10`.

Corollary: when a number surprises you, re-measure that one cell with more warmup and more forks
before believing it. Several "findings" in this project's history were artifacts that dissolved under
a longer run.

## Fast iteration while developing

The common loop does **not** re-measure competitors. If `docs/bench-results.json` already exists,
`bench-run.sh` re-measures only the `farray_*` methods and patches them into the cached results:

```
caffeinate -i bash scripts/bench-run.sh [warmup] [measure] [forks] [max-shards]
caffeinate -i bash scripts/bench-run.sh 3 5 1 6      # typical
```

Same for the fset suite via `scripts/setbench-run.sh` (subject = `fset`).

Because this measures FArray under contention against uncontended cached competitor numbers, the
resulting **win/loss ratios are not trustworthy** — use it to see whether *your own* change moved
FArray relative to FArray, not to settle a comparison.

### Adding a new competitor

`BENCH_VARIANT` generalizes the patch to any method-name prefix, so a newly added rival can be folded
into an existing scorecard without re-measuring the field:

```
BENCH_VARIANT=kyochunk bash scripts/bench-run.sh 5 8 1 1
```

Run it at `SHARDS=1` (as above) so the new competitor's cells are measured under the same
uncontended conditions as everything already in the file. This is how `kyo.Chunk` was added.

### One JMH consumer per box

`scripts/bench-lock.sh` is a mutex — several agents/sessions may share this machine, and two JMH runs
both measure garbage. The sweep runners take it automatically; wrap ad-hoc runs yourself:

```
bash scripts/bench-lock.sh bleep run benchmarks-runner -- '<regex>' -f 1 -wi 5 -i 8 -p size=100000
```

It waits rather than failing, and steals a lock whose holder died. Never `pkill` JMH.

**Never `bleep compile` or `bleep test` while a sweep is live.** Recompilation swaps class files under
the running forks and JMH silently truncates — it exits 0 with a partial matrix. Two runs were lost
to this on 2026-07-04.

## Known limitations

**`*Bench`-suffixed classes are never swept.** `bench-run.sh` lists classes with a regex requiring a
`Benchmark` suffix, so `IntDceBench`, `StrDceBench`, `IntLongPipelineBench` and `StrLongPipelineBench`
carry cached numbers no sweep refreshes — stale for every variant — and the `*AllOpsBench` family has
never entered the scorecard at all. Fixing the regex widens the scorecard's scope, so it needs a
deliberate full re-measure rather than a quiet change.

**Empty-input cells are absent for some rivals.** At `size=0` several competitors throw
(`empty.max`, `head`, `updated(0, …)`), so those cells simply have no entry. This is parity across
rivals, not a defect in any one of them.

**A fresh clone or git worktree needs a preflight.** `GenJmh` reads compiled classes from a path the
newer bleep CLI no longer writes to; where both layouts exist the old one is a symlink to the new.
Without it the generator finds zero classes, writes an **empty** `META-INF/BenchmarkList`, and exits
0 — after which *every* JMH pattern answers `No matching benchmarks. Miss-spelled regexp?`, including
benchmarks that obviously exist. `scripts/bench-preflight.sh` creates the link and warns on an empty
list; both sweep runners now call it automatically. If you ever see that error for a benchmark you
can read in the source, this is why.
