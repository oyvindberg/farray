#!/usr/bin/env bash
# Full-suite, SEQUENTIAL re-measure of BOTH benchmark suites (farray + fset), sized to ~8 hours.
#
#   bash scripts/bench-full-8h.sh          # (self-wraps in caffeinate on macOS)
#
# Config (why these numbers): 1 fork × (5 warmup + 8 measure) × 300ms ≈ 4.4 s/trial, and the two
# full suites together are ~6,579 trials (5,622 farray + 957 fset) → ≈ 8 hours wall time. Measured
# calibration 2026-07-03: 0.47s fork startup + 0.30s/iteration. Same 5w/8m iteration scheme as the
# checked-in scorecard runs. max-shards=1 → exactly one JMH process at a time (fully serialized,
# lowest noise); the bench mutex (scripts/bench-lock.sh) additionally serializes against other
# sessions.
#
# Full mode (re-measuring every competitor, not just the farray_*/fset patch subset) is triggered
# by the result JSONs being absent, so this script moves them aside first — into a timestamped
# backup dir, never deleted (they are also checked into git). The sweeps regenerate both files.
set -euo pipefail
cd "$(dirname "$0")/.."

# Keep the box awake for the whole run (macOS). Re-exec under caffeinate once.
if [ -z "${BENCH_CAFFEINATED:-}" ] && command -v caffeinate >/dev/null 2>&1; then
  exec env BENCH_CAFFEINATED=1 caffeinate -i bash "$0" "$@"
fi

WI=5; MI=8; FORKS=1; MAXJ=1

# Force full-suite mode: set the existing scorecards aside (backup, not delete).
TS=$(date +%Y%m%d-%H%M%S)
BACKUP="docs/.full-run-backup-$TS"
mkdir -p "$BACKUP"
for f in docs/bench-results.json docs/set-bench-results.json; do
  [ -f "$f" ] && mv "$f" "$BACKUP/"
done
echo "▶ Existing scorecards moved to $BACKUP (full-suite mode forced)"

echo "▶ Started $(date '+%F %T') — expect ~8h (farray ~6.8h, then fset ~1.2h)"
bash scripts/bench-run.sh    "$WI" "$MI" "$FORKS" "$MAXJ"
bash scripts/setbench-run.sh "$WI" "$MI" "$FORKS" "$MAXJ"
echo "✔ Finished $(date '+%F %T') — review + commit docs/bench-results.json docs/set-bench-results.json"
