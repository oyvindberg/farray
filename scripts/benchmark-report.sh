#!/usr/bin/env bash
# Run the full JMH benchmark suite and render a self-contained HTML report into docs/ (GitHub Pages).
#
#   scripts/benchmark-report.sh [size] [warmup-iters] [measure-iters] [forks]
#
# Defaults: size=1000, 5 warmup + 8 measurement iterations, 2 forks — enough for results that matter.
# Outputs: docs/bench-results.json (raw JMH, also loadable at https://jmh.morethan.io)
#          docs/report.html         (the GitHub Pages report)
set -euo pipefail
cd "$(dirname "$0")/.."

SIZE="${1:-1000}"
WI="${2:-5}"
MI="${3:-8}"
FORKS="${4:-2}"

mkdir -p docs
echo "▶ Running benchmarks (size=$SIZE, ${WI} warmup + ${MI} measurement iters, ${FORKS} forks)…"
bleep run benchmarks-runner -- ".*Benchmark" \
  -p size="$SIZE" -wi "$WI" -i "$MI" -f "$FORKS" -r 400ms -w 400ms \
  -rf json -rff docs/bench-results.json

echo "▶ Rendering HTML…"
python3 scripts/bench_report.py docs/bench-results.json docs/report.html

echo "✔ Done. docs/report.html (served at the GitHub Pages URL)"
