#!/usr/bin/env bash
# Truly-parallel JMH sweep. Drives org.openjdk.jmh.Main directly (N independent java
# processes) instead of N `bleep run` (which serialize on the bloop build server).
# Each shard gets its own java.io.tmpdir + -Djmh.ignoreLock=true so the JMH lock file
# doesn't make them collide. Wrap in `caffeinate -i` to keep the machine awake.
#
#   caffeinate -i bash scripts/bench-run.sh [warmup-iters] [measure-iters] [forks] [shards]
#
# Defaults: 3 warmup + 5 measurement iters, 0 forks (in-shard JVM, fast/noisier), shards = cores-1.
# Sizes come from each benchmark's @Param (0..100000); overridden only where needed
# (head/last/apply skip 0; O(n^2) chains capped at 10000). Env XMX (default 1g) per shard.
set -uo pipefail
cd "$(dirname "$0")/.."

# Serialize all benchmarking on this box through the shared mutex (see scripts/bench-lock.sh):
# concurrent JMH runs contend and both measure garbage. Re-exec the whole sweep under the lock.
if [ -z "${BENCH_LOCK_HELD:-}" ]; then
  exec env BENCH_LOCK_HELD=1 bash scripts/bench-lock.sh bash "$0" "$@"
fi

WI="${1:-3}"; MI="${2:-5}"; FORKS="${3:-0}"
MAXJ="${4:-6}"                                            # max shards running AT ONCE — caps peak memory (each -f1 fork ~2g)
CORES="$(sysctl -n hw.ncpu 2>/dev/null || nproc)"
SHARDS=$(( CORES - 1 )); [ "$SHARDS" -lt 1 ] && SHARDS=1  # shard GROUPS (work distribution); concurrency is throttled to MAXJ
throttle() { while [ "$(jobs -rp | grep -c .)" -ge "$MAXJ" ]; do sleep 0.5; done; }
XMX="${XMX:-1g}"
MAIN="org.openjdk.jmh.Main"

# Fast-iteration mode: if results already exist, re-measure ONLY the farray methods and
# patch them into the cached json (keeping every competitor entry). Otherwise full suite.
#
# BENCH_VARIANT generalises that patch to ANY method-name prefix — the mechanism used to fold a
# newly-added competitor into an existing scorecard without re-measuring everyone else:
#   BENCH_VARIANT=kyochunk caffeinate -i bash scripts/bench-run.sh 3 5 1 6
# measures only the `kyochunk*` methods and patches them in, leaving farray and every other
# competitor's cached numbers untouched. Caveat: numbers patched in this way were measured under a
# DIFFERENT sweep's contention than the entries they sit beside, so a fresh competitor's cells are
# only as trustworthy as that match — confirm close calls with an isolated -f 2 run.
RESULTS="${BENCH_RESULTS:-docs/bench-results.json}"
VARIANT="${BENCH_VARIANT:-farray}"
if [ -f "$RESULTS" ]; then
  MODE="patch"; echo "▶ Mode: ${VARIANT}-only patch ($RESULTS exists)"
else
  MODE="full";  echo "▶ Mode: full suite (no $RESULTS yet)"
fi

echo "▶ Compiling…"
bleep compile benchmarks-runner >/tmp/bench-compile.log 2>&1 || { echo "compile failed:"; tail -20 /tmp/bench-compile.log; exit 1; }

# A fresh clone/worktree cannot generate the JMH wrappers until the classes-dir link exists — without
# it every pattern reports "No matching benchmarks". Fix it, then re-run the build so the generator
# actually sees the classes. See scripts/bench-preflight.sh.
bash scripts/bench-preflight.sh benchmarks
bleep compile benchmarks-runner >>/tmp/bench-compile.log 2>&1 || true

# bleep's on-disk bloop configs are stale, so grab the real java+classpath from a live run.
echo "▶ Capturing runtime java + classpath…"
# </dev/null: a BACKGROUNDED bleep run from an interactive terminal hard-fails trying to attach its
# input reader to the TTY ("IO error: Failed to initialize input reader") — killed a full-run at the
# capture step on 2026-07-06. Non-interactive contexts were unaffected (stdin already null there).
bleep run benchmarks-runner -- "MapStrBenchmark.farray" -p size=10 -wi 5 -i 120 -f 0 -r 1s </dev/null >/tmp/cp-cap.log 2>&1 &
CAPPID=$!
JAVA=""; CP=""
for _ in $(seq 1 90); do
  CMD=$(ps -Ao command 2>/dev/null | grep "$MAIN" | grep -v grep | head -1)
  if [ -n "$CMD" ]; then
    JAVA=$(printf '%s' "$CMD" | awk '{print $1}')
    CP=$(printf '%s' "$CMD" | grep -oE '(-cp|-classpath)[[:space:]]+[^[:space:]]+' | head -1 | awk '{print $2}')
    [ -n "$CP" ] && case "$CP" in *fs2*) break;; esac
  fi
  sleep 1
done
kill "$CAPPID" 2>/dev/null; pkill -f "$MAIN" 2>/dev/null
{ [ -z "$CP" ] || [ -z "$JAVA" ]; } && { echo "failed to capture classpath; see /tmp/cp-cap.log"; exit 1; }
# BENCH_JAVA: run the suite on a different JVM than the build's (e.g. the Temurin/C2 scorecard:
#   BENCH_JAVA="$(coursier java-home --jvm temurin:25)/bin/java" BENCH_RESULTS=docs/bench-results-c2.json ...)
# The classpath capture above still uses the build JVM; only the measuring JVM is swapped.
[ -n "${BENCH_JAVA:-}" ] && { JAVA="$BENCH_JAVA"; echo "  BENCH_JAVA override: $JAVA"; }
echo "  java=$(basename "$(dirname "$(dirname "$JAVA")")") · classpath $(printf '%s' "$CP" | tr ':' '\n' | grep -c .) entries"

echo "▶ Listing benchmarks…"
ALL=$("$JAVA" -cp "$CP" "$MAIN" -l 2>/dev/null | grep -oE 'farray\.[A-Za-z0-9]+Benchmark\.' | sed -E 's/farray\.([A-Za-z0-9]+Benchmark)\./\1/' | sort -u)
[ -z "$ALL" ] && { echo "no benchmarks listed"; exit 1; }

EMPTY='StrHeadBenchmark|StrLastBenchmark|StrTailBenchmark|StrInitBenchmark|StrApplyBenchmark'
CHAINS='IntAppendChainBenchmark|IntPrependChainBenchmark|IntUpdateChainBenchmark'
UPDATED='StrUpdatedBenchmark|IntUpdatedMapBenchmark|IntUpdated4MapBenchmark'  # updated(fixed index) needs non-empty
DIAG='ArrLenIterBenchmark|ArrLenFoldBenchmark|IndexedApplyPollutedBenchmark|IndexedApplyStrBenchmark'  # loop-shape / megamorphic-refAt validators: A/B only, not a scorecard comparison
NORMAL=$(echo "$ALL" | grep -vE "^($EMPTY|$CHAINS|$UPDATED|$DIAG)$" || true)

rm -rf docs/parts && mkdir -p docs/parts
echo "  $(echo "$ALL" | grep -c .) classes → $SHARDS shards, ≤$MAXJ at once (XMX=$XMX, ${WI}w/${MI}m/${FORKS}f)"

run_shard() {  # name  regex  extra-jmh-args...
  local name="$1" rx="$2"; shift 2
  local td="/tmp/jmh-$name"; rm -rf "$td"; mkdir -p "$td"
  # In patch mode, constrain the class regex to the `.$VARIANT*` methods so each shard measures
  # just that one implementation; full mode runs all impls in each class.
  local filter
  if [ "$MODE" = "patch" ]; then filter="(farray\.($rx)\.${VARIANT}[A-Za-z]*_?)"; else filter="($rx)"; fi
  "$JAVA" -Xmx"$XMX" -Djava.io.tmpdir="$td" -Djmh.ignoreLock=true -cp "$CP" "$MAIN" \
    "$filter" -wi "$WI" -i "$MI" -f "$FORKS" -r 300ms -w 300ms \
    -rf json -rff "docs/parts/part-$name.json" "$@" >"docs/parts/log-$name.txt" 2>&1 &
}

declare -a G; i=0
for c in $NORMAL; do idx=$(( i % SHARDS )); G[$idx]="${G[$idx]:-}$c|"; i=$(( i + 1 )); done
for idx in "${!G[@]}"; do throttle; run_shard "n$idx" "${G[$idx]%|}"; done
echo "$ALL" | grep -qE "^($EMPTY)$"  && { throttle; run_shard "edge"  "$EMPTY"  -p size=1,10,100,1000,10000,100000; }
echo "$ALL" | grep -qE "^($CHAINS)$" && { throttle; run_shard "chain" "$CHAINS" -p size=0,1,10,100,1000,10000; }
echo "$ALL" | grep -qE "^($UPDATED)$" && { throttle; run_shard "upd" "$UPDATED" -p size=10,100,1000,10000,100000; }

echo "▶ $(jobs -rp 2>/dev/null | grep -c .) shards running in parallel across $CORES cores…"
wait
echo "▶ Merging + rendering…"
MODE="$MODE" RESULTS="$RESULTS" VARIANT="$VARIANT" python3 - <<'PY'
import json, glob, os
mode, variant = os.environ["MODE"], os.environ["VARIANT"]
new, files = [], sorted(glob.glob("docs/parts/part-*.json"))
for f in files:
    try: new += json.load(open(f))
    except Exception as e: print("  skip", f, e)

def is_variant(b):  # kyochunk_map, farray_scanLeft, bare farray, plus farrayMat_*/farrayTree_* variants
    return b["benchmark"].split(".")[-1].split("_")[0].startswith(variant)

dst = os.environ.get("RESULTS", "docs/bench-results.json")
if mode == "patch":
    old = json.load(open(dst))
    kept = [b for b in old if not is_variant(b)]
    out = kept + new
    json.dump(out, open(dst, "w"))
    print(f"  {variant}-only patch: refreshed {len(new)} {variant} entries, kept {len(kept)} other entries")
else:
    json.dump(new, open(dst, "w"))
    print(f"  full suite: merged {len(new)} results from {len(files)} shards")
PY
# The report is the site: site/ renders docs/bench-results.json (see site/scripts/build-data.mjs).
echo "✔ Done → $RESULTS  (view: cd site && npm run dev → #/reference; publish: git add docs/ && git commit && git push)"
