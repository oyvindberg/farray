#!/usr/bin/env bash
# ONE COMMAND to re-measure the entire scorecard on any machine, fully sequentially.
#
#   bash scripts/bench-all.sh
#
# That is the whole thing. It runs both suites (farray + fset), every implementation (not just the
# fast-iteration patch subset), one JMH process at a time, and ends at the two checked-in result
# files that the site renders:
#
#   docs/bench-results.json       the farray suite   → site page /benchmarks/farray
#   docs/set-bench-results.json   the fset suite     → site page /benchmarks/fset
#
# Existing scorecards are MOVED ASIDE into docs/.full-run-backup-<timestamp>/ (never deleted), which
# is also what forces full-suite mode: both sweeps re-measure every competitor when no result file
# is present, and re-measure only their own subject when one is.
#
# ── Why sequential ────────────────────────────────────────────────────────────────────────────────
# Parallel shards finish far sooner but every number is contended. Measured on this project
# 2026-07-20: identical code, 6 shards vs 1, moved the SAME benchmark by a median 1.26x (p90 1.66x,
# worst 2.85x), one-directional across 115 of 119 cells. That is larger than most of the margins the
# scorecard reports, so a contended sweep cannot be compared against an uncontended one. If you run
# this parallel to save time, the results are only comparable with OTHER runs at the same shard
# count — never mix them into an existing scorecard.
#
# ── Knobs (environment variables) ─────────────────────────────────────────────────────────────────
#   WI=5        warmup iterations per trial. FArray needs MORE warmup than its rivals to reach peak
#               — at WI=3 / 300ms some FArray cells measured up to 10x low while the competitors in
#               the same run were already at steady state, a bias that silently works AGAINST us.
#               Do not lower this for a scorecard run.
#   MI=8        measurement iterations per trial.
#   FORKS=1     JVM forks per benchmark. 1 = each benchmark gets a clean JVM (correct, and what the
#               checked-in numbers use). 0 = reuse the shard's JVM: much faster, noisier, and lets
#               one benchmark's JIT profile pollute the next. 2+ = for confirming a surprising cell.
#   SHARDS=1    max JMH processes at once. 1 = sequential (see above). Raise ONLY for a throwaway
#               exploratory run.
#   SUITES=both which suites to run: both | farray | fset.
#   XMX=1g      heap per JMH process. Raise if you see OOM on the 100k-element cases.
#   TIME_PER_IT=0.3   seconds per iteration (both -r and -w), used for the ETA and passed through.
#
# Examples:
#   bash scripts/bench-all.sh                          # the scorecard run, sequential, both suites
#   SUITES=farray bash scripts/bench-all.sh            # farray suite only
#   WI=10 MI=10 FORKS=2 bash scripts/bench-all.sh      # slower, highest confidence
#   SHARDS=6 FORKS=0 bash scripts/bench-all.sh         # fast + noisy; do NOT commit these numbers
#
# ── Before you start ──────────────────────────────────────────────────────────────────────────────
#   * Plug the machine in. On macOS this self-wraps in `caffeinate -is`, which only prevents sleep
#     on AC power; a mid-run sleep or a low-power-mode throttle corrupts whole shards.
#   * Close anything CPU-hungry. A running dev server / compile / browser perturbs the measurement —
#     that is the same contention effect as parallel shards, just from another process.
#   * Expect hours. The ETA is printed below from a calibration measured on an M-series Mac; a
#     different machine scales roughly with single-core speed.
#
# Related: scripts/bench-run.sh (farray sweep + its own knobs, incl. BENCH_VARIANT for folding in a
# single new competitor), scripts/setbench-run.sh (fset sweep), scripts/bench-lock.sh (the one-JMH-
# at-a-time mutex), docs/benchmarking.md (the full guide).
set -euo pipefail
cd "$(dirname "$0")/.."

WI="${WI:-5}"; MI="${MI:-8}"; FORKS="${FORKS:-1}"; SHARDS="${SHARDS:-1}"
SUITES="${SUITES:-both}"; TIME_PER_IT="${TIME_PER_IT:-0.3}"
export XMX="${XMX:-1g}"

# Keep the box awake for the whole run (macOS). Re-exec under caffeinate once.
# -is: -i alone does NOT survive lid-close — a mid-run sleep produced garbage across whole shards
# (2026-07-05). -s prevents system sleep while on AC power, so run this PLUGGED IN.
if [ -z "${BENCH_CAFFEINATED:-}" ] && command -v caffeinate >/dev/null 2>&1; then
  exec env BENCH_CAFFEINATED=1 caffeinate -is bash "$0" "$@"
fi

if command -v pmset >/dev/null 2>&1 && pmset -g batt 2>/dev/null | grep -q "Battery Power"; then
  echo "⚠ On battery power — plug in before a multi-hour serialized run (caffeinate -s is AC-only)." >&2
fi

# ETA from the 2026-07-03 calibration: 0.47s fork startup + TIME_PER_IT per iteration, times the
# known trial counts of the two suites. Rough by construction — it is a heads-up, not a promise.
TRIALS_FARRAY=5622; TRIALS_FSET=957
case "$SUITES" in
  farray) TRIALS=$TRIALS_FARRAY ;;
  fset)   TRIALS=$TRIALS_FSET ;;
  both)   TRIALS=$(( TRIALS_FARRAY + TRIALS_FSET )) ;;
  *) echo "SUITES must be one of: both | farray | fset (got '$SUITES')" >&2; exit 2 ;;
esac
ETA=$(awk -v t="$TRIALS" -v wi="$WI" -v mi="$MI" -v f="$FORKS" -v s="$SHARDS" -v tpi="$TIME_PER_IT" \
  'BEGIN{ forks=(f<1?1:f); printf "%.1f", (t*forks*(0.47+(wi+mi)*tpi))/(s<1?1:s)/3600 }')

echo "▶ Sequential full re-measure — suites=$SUITES  ${WI}w/${MI}m/${FORKS}f  shards=$SHARDS  XMX=$XMX"
echo "▶ ~$TRIALS trials → ETA ≈ ${ETA}h (calibrated on an M-series Mac; scales with single-core speed)"
[ "$SHARDS" -gt 1 ] && echo "⚠ SHARDS=$SHARDS — these numbers are CONTENDED and must not be mixed into the scorecard." >&2

# Force full-suite mode: set the existing scorecards aside (backup, not delete).
TS=$(date +%Y%m%d-%H%M%S)
BACKUP="docs/.full-run-backup-$TS"
mkdir -p "$BACKUP"
for f in docs/bench-results.json docs/set-bench-results.json; do
  [ -f "$f" ] && mv "$f" "$BACKUP/"
done
echo "▶ Existing scorecards moved to $BACKUP (full-suite mode forced; nothing is deleted)"

echo "▶ Started $(date '+%F %T')"
# case, not `[ … ] && …`: under `set -e` a false test at the end of a && list aborts the script, which
# would skip the summary below whenever only one suite is selected.
case "$SUITES" in both|farray) bash scripts/bench-run.sh    "$WI" "$MI" "$FORKS" "$SHARDS" ;; esac
case "$SUITES" in both|fset)   bash scripts/setbench-run.sh "$WI" "$MI" "$FORKS" "$SHARDS" ;; esac
echo "✔ Finished $(date '+%F %T')"
echo
echo "Next: inspect, then commit."
echo "  cd site && npm install && npm run data && npm run dev     # → http://localhost:3000/farray/"
echo "  git add docs/bench-results.json docs/set-bench-results.json && git commit"
