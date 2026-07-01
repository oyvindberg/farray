#!/usr/bin/env bash
# Truly-parallel JMH sweep for the FSet suite — a parallel of scripts/bench-run.sh, fully split:
# its own runner (setbenchmarks-runner) and its own output (docs/set-bench-results.json — rendered by
# the site at #/setbench). Drives org.openjdk.jmh.Main directly (N independent java processes).
#
#   caffeinate -i bash scripts/setbench-run.sh [warmup-iters] [measure-iters] [forks] [shards]
#
# Defaults: 3 warmup + 5 measurement iters, 0 forks (in-shard JVM, fast/noisier), shards = cores-1.
# If docs/set-bench-results.json exists, re-measures ONLY the `fset` methods and patches them in
# (keeping every competitor entry); otherwise runs the full suite. Env XMX (default 1g) per shard.
set -uo pipefail
cd "$(dirname "$0")/.."

# Serialize all benchmarking on this box through the shared mutex (see scripts/bench-lock.sh):
# concurrent JMH runs contend and both measure garbage. Re-exec the whole sweep under the lock.
if [ -z "${BENCH_LOCK_HELD:-}" ]; then
  exec env BENCH_LOCK_HELD=1 bash scripts/bench-lock.sh bash "$0" "$@"
fi

WI="${1:-3}"; MI="${2:-5}"; FORKS="${3:-0}"
MAXJ="${4:-6}"
CORES="$(sysctl -n hw.ncpu 2>/dev/null || nproc)"
SHARDS=$(( CORES - 1 )); [ "$SHARDS" -lt 1 ] && SHARDS=1
throttle() { while [ "$(jobs -rp | grep -c .)" -ge "$MAXJ" ]; do sleep 0.5; done; }
XMX="${XMX:-1g}"
MAIN="org.openjdk.jmh.Main"
RUNNER="setbenchmarks-runner"

RESULTS="docs/set-bench-results.json"
if [ -f "$RESULTS" ]; then
  MODE="fset"; echo "▶ Mode: fset-only patch ($RESULTS exists)"
else
  MODE="full"; echo "▶ Mode: full suite (no $RESULTS yet)"
fi

echo "▶ Compiling…"
bleep compile "$RUNNER" >/tmp/setbench-compile.log 2>&1 || { echo "compile failed:"; tail -20 /tmp/setbench-compile.log; exit 1; }

# bleep's on-disk bloop configs are stale, so grab the real java+classpath from a live run.
echo "▶ Capturing runtime java + classpath…"
bleep run "$RUNNER" -- "IntSetContainsHitBenchmark.fset" -p size=16 -wi 5 -i 120 -f 0 -r 1s >/tmp/setcp-cap.log 2>&1 &
CAPPID=$!
JAVA=""; CP=""
for _ in $(seq 1 90); do
  CMD=$(ps -Ao command 2>/dev/null | grep "$MAIN" | grep -v grep | head -1)
  if [ -n "$CMD" ]; then
    JAVA=$(printf '%s' "$CMD" | awk '{print $1}')
    CP=$(printf '%s' "$CMD" | grep -oE '(-cp|-classpath)[[:space:]]+[^[:space:]]+' | head -1 | awk '{print $2}')
    [ -n "$CP" ] && case "$CP" in *fastutil*) break;; esac
  fi
  sleep 1
done
kill "$CAPPID" 2>/dev/null; pkill -f "$MAIN" 2>/dev/null
{ [ -z "$CP" ] || [ -z "$JAVA" ]; } && { echo "failed to capture classpath; see /tmp/setcp-cap.log"; exit 1; }
echo "  java=$(basename "$(dirname "$(dirname "$JAVA")")") · classpath $(printf '%s' "$CP" | tr ':' '\n' | grep -c .) entries"

echo "▶ Listing benchmarks…"
ALL=$("$JAVA" -cp "$CP" "$MAIN" -l 2>/dev/null | grep -oE 'farray\.[A-Za-z0-9]+Benchmark\.' | sed -E 's/farray\.([A-Za-z0-9]+Benchmark)\./\1/' | sort -u)
[ -z "$ALL" ] && { echo "no benchmarks listed"; exit 1; }

rm -rf docs/setparts && mkdir -p docs/setparts
echo "  $(echo "$ALL" | grep -c .) classes → $SHARDS shards, ≤$MAXJ at once (XMX=$XMX, ${WI}w/${MI}m/${FORKS}f)"

run_shard() {  # name  regex
  local name="$1" rx="$2"; shift 2
  local td="/tmp/setjmh-$name"; rm -rf "$td"; mkdir -p "$td"
  # In fset-only mode, constrain the class regex to the `.fset` method so each shard re-measures just FSet.
  local filter
  if [ "$MODE" = "fset" ]; then filter="(farray\.($rx)\.fset$)"; else filter="($rx)"; fi
  "$JAVA" -Xmx"$XMX" -Djava.io.tmpdir="$td" -Djmh.ignoreLock=true -cp "$CP" "$MAIN" \
    "$filter" -wi "$WI" -i "$MI" -f "$FORKS" -r 300ms -w 300ms \
    -rf json -rff "docs/setparts/part-$name.json" "$@" >"docs/setparts/log-$name.txt" 2>&1 &
}

declare -a G; i=0
for c in $ALL; do idx=$(( i % SHARDS )); G[$idx]="${G[$idx]:-}$c|"; i=$(( i + 1 )); done
for idx in "${!G[@]}"; do throttle; run_shard "n$idx" "${G[$idx]%|}"; done

echo "▶ $(jobs -rp 2>/dev/null | grep -c .) shards running in parallel across $CORES cores…"
wait
echo "▶ Merging + rendering…"
MODE="$MODE" python3 - <<'PY'
import json, glob, os
mode = os.environ["MODE"]
new, files = [], sorted(glob.glob("docs/setparts/part-*.json"))
for f in files:
    try: new += json.load(open(f))
    except Exception as e: print("  skip", f, e)

def is_fset(b):
    return b["benchmark"].split(".")[-1].split("_")[0] == "fset"

dst = "docs/set-bench-results.json"
if mode == "fset":
    old = json.load(open(dst))
    kept = [b for b in old if not is_fset(b)]
    out = kept + new
    json.dump(out, open(dst, "w"))
    print(f"  fset-only patch: refreshed {len(new)} fset entries, kept {len(kept)} competitor entries")
else:
    json.dump(new, open(dst, "w"))
    print(f"  full suite: merged {len(new)} results from {len(files)} shards")
PY
# The report is the site: site/ renders docs/set-bench-results.json (see site/scripts/build-data.mjs).
echo "✔ Done → docs/set-bench-results.json  (view: cd site && npm run dev → #/setbench; publish: git add docs/ && git commit && git push)"
