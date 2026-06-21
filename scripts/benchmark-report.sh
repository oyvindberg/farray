#!/usr/bin/env bash
# Run the full JMH benchmark suite and render a self-contained HTML report.
#
#   scripts/benchmark-report.sh [size] [warmup-iters] [measure-iters]
#
# Defaults: size=1000, 3 warmup + 4 measurement iterations, 1 fork.
# Outputs: bench-results.json (raw JMH, also loadable at https://jmh.morethan.io)
#          bench-report.html  (open in a browser)
set -euo pipefail
cd "$(dirname "$0")/.."

SIZE="${1:-1000}"
WI="${2:-3}"
MI="${3:-4}"

echo "▶ Running benchmarks (size=$SIZE, ${WI} warmup + ${MI} measurement iterations)…"
bleep run benchmarks-runner -- ".*Benchmark" \
  -p size="$SIZE" -wi "$WI" -i "$MI" -f 1 -r 400ms -w 400ms \
  -rf json -rff bench-results.json

echo "▶ Rendering HTML…"
python3 scripts/bench_report.py bench-results.json bench-report.html

echo "✔ Done. Open bench-report.html"
