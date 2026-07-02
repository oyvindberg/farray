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
RESULTS="docs/bench-results.json"
if [ -f "$RESULTS" ]; then
  MODE="farray"; echo "▶ Mode: farray-only patch ($RESULTS exists)"
else
  MODE="full";   echo "▶ Mode: full suite (no $RESULTS yet)"
fi

echo "▶ Compiling…"
bleep compile benchmarks-runner >/tmp/bench-compile.log 2>&1 || { echo "compile failed:"; tail -20 /tmp/bench-compile.log; exit 1; }

# bleep's on-disk bloop configs are stale, so grab the real java+classpath from a live run.
echo "▶ Capturing runtime java + classpath…"
bleep run benchmarks-runner -- "MapStrBenchmark.farray" -p size=10 -wi 5 -i 120 -f 0 -r 1s >/tmp/cp-cap.log 2>&1 &
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
echo "  java=$(basename "$(dirname "$(dirname "$JAVA")")") · classpath $(printf '%s' "$CP" | tr ':' '\n' | grep -c .) entries"

echo "▶ Listing benchmarks…"
ALL=$("$JAVA" -cp "$CP" "$MAIN" -l 2>/dev/null | grep -oE 'farray\.[A-Za-z0-9]+Benchmark\.' | sed -E 's/farray\.([A-Za-z0-9]+Benchmark)\./\1/' | sort -u)
[ -z "$ALL" ] && { echo "no benchmarks listed"; exit 1; }

EMPTY='StrHeadBenchmark|StrLastBenchmark|StrTailBenchmark|StrInitBenchmark|StrApplyBenchmark'
CHAINS='IntAppendChainBenchmark|IntPrependChainBenchmark|IntUpdateChainBenchmark'
UPDATED='StrUpdatedBenchmark|IntUpdatedMapBenchmark|IntUpdated4MapBenchmark'  # updated(fixed index) needs non-empty
DIAG='ArrLenIterBenchmark|ArrLenFoldBenchmark'  # loop-shape validators: no competitor methods, not a scorecard comparison
NORMAL=$(echo "$ALL" | grep -vE "^($EMPTY|$CHAINS|$UPDATED|$DIAG)$" || true)

rm -rf docs/parts && mkdir -p docs/parts
echo "  $(echo "$ALL" | grep -c .) classes → $SHARDS shards, ≤$MAXJ at once (XMX=$XMX, ${WI}w/${MI}m/${FORKS}f)"

run_shard() {  # name  regex  extra-jmh-args...
  local name="$1" rx="$2"; shift 2
  local td="/tmp/jmh-$name"; rm -rf "$td"; mkdir -p "$td"
  # In farray-only mode, constrain the class regex to `.farray*` methods so each shard
  # measures just the FArray implementation; full mode runs all impls in each class.
  local filter
  if [ "$MODE" = "farray" ]; then filter="(farray\.($rx)\.farray[A-Za-z]*_?)"; else filter="($rx)"; fi
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
MODE="$MODE" python3 - <<'PY'
import json, glob, os
mode = os.environ["MODE"]
new, files = [], sorted(glob.glob("docs/parts/part-*.json"))
for f in files:
    try: new += json.load(open(f))
    except Exception as e: print("  skip", f, e)

def is_farray(b):  # farray_map, farray_scanLeft, bare farray, plus farrayMat_*/farrayTree_* variants
    return b["benchmark"].split(".")[-1].split("_")[0].startswith("farray")

dst = "docs/bench-results.json"
if mode == "farray":
    old = json.load(open(dst))
    kept = [b for b in old if not is_farray(b)]
    out = kept + new
    json.dump(out, open(dst, "w"))
    print(f"  farray-only patch: refreshed {len(new)} farray entries, kept {len(kept)} competitor entries")
else:
    json.dump(new, open(dst, "w"))
    print(f"  full suite: merged {len(new)} results from {len(files)} shards")
PY
python3 scripts/bench_report.py docs/bench-results.json docs/report.html
echo "✔ Done → docs/report.html  (publish: git add docs/ && git commit && git push)"
