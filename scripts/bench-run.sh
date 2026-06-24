#!/usr/bin/env bash
# Parallel JMH sweep: shard benchmark classes across cores, run concurrently, merge, render.
# Trades a little cross-process noise for a big wall-clock win. Wrap in `caffeinate -i` to avoid sleep.
#
#   caffeinate -i bash scripts/bench-run.sh [warmup-iters] [measure-iters] [forks] [shards]
#
# Defaults: 2 warmup + 3 measurement iters, 0 forks (run in-shard JVM = fast, noisier), shards = cores-1.
# Sizes come from each benchmark's @Param (0..100000); this script only overrides where needed:
#   - head/last/tail/init/apply are undefined on empty  -> skip size 0
#   - O(n^2) builder chains                             -> cap at 10000
# Output: docs/bench-results.json (merged) + docs/index.html (report).
set -uo pipefail
cd "$(dirname "$0")/.."

WI="${1:-2}"; MI="${2:-3}"; FORKS="${3:-0}"
CORES="$(sysctl -n hw.ncpu 2>/dev/null || nproc)"
SHARDS="${4:-$(( CORES > 2 ? CORES - 1 : 1 ))}"
COMMON=(-wi "$WI" -i "$MI" -f "$FORKS" -r 300ms -w 300ms)

EMPTY='StrHeadBenchmark|StrLastBenchmark|StrTailBenchmark|StrInitBenchmark|StrApplyBenchmark'
CHAINS='IntAppendChainBenchmark|IntPrependChainBenchmark|IntUpdateChainBenchmark'

rm -rf docs/parts && mkdir -p docs/parts
echo "▶ Listing benchmarks…"
ALL=$(bleep run benchmarks-runner -- -l 2>/dev/null \
        | grep -oE 'farray\.[A-Za-z0-9]+Benchmark\.' | sed -E 's/farray\.([A-Za-z0-9]+Benchmark)\./\1/' | sort -u)
[ -z "$ALL" ] && { echo "no benchmarks listed — aborting"; exit 1; }
NORMAL=$(echo "$ALL" | grep -vE "^($EMPTY|$CHAINS)$" || true)
echo "  $(echo "$ALL" | wc -l | tr -d ' ') classes; sharding NORMAL into $SHARDS, +edge +chain shards"

run_shard() {  # name  class-regex  extra-args...
  local name="$1" rx="$2"; shift 2
  bleep run benchmarks-runner -- "($rx)" "${COMMON[@]}" "$@" \
    -rf json -rff "docs/parts/part-$name.json" >"docs/parts/log-$name.txt" 2>&1 &
}

# round-robin NORMAL classes into SHARDS regex groups
declare -a G; i=0
for c in $NORMAL; do idx=$(( i % SHARDS )); G[$idx]="${G[$idx]:-}$c|"; i=$(( i + 1 )); done
for idx in "${!G[@]}"; do run_shard "n$idx" "${G[$idx]%|}"; done
[ -n "$(echo "$ALL" | grep -E "^($EMPTY)$" || true)" ]  && run_shard "edge"  "$EMPTY"  -p size=1,10,100,1000,10000,100000
[ -n "$(echo "$ALL" | grep -E "^($CHAINS)$" || true)" ] && run_shard "chain" "$CHAINS" -p size=0,1,10,100,1000,10000

echo "▶ Running $(jobs -p | wc -l | tr -d ' ') shards in parallel…"
wait
echo "▶ Merging + rendering…"
python3 - <<'PY'
import json, glob
out = []
for f in sorted(glob.glob("docs/parts/part-*.json")):
    try: out += json.load(open(f))
    except Exception as e: print(f"  skip {f}: {e}")
json.dump(out, open("docs/bench-results.json", "w"))
print(f"  merged {len(out)} results from {len(glob.glob('docs/parts/part-*.json'))} shards")
PY
python3 scripts/bench_report.py docs/bench-results.json docs/index.html
echo "✔ Done → docs/index.html  (publish: git add docs/ && git commit && git push)"
