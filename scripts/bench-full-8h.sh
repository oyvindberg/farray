#!/usr/bin/env bash
# Kept for muscle memory — the full sequential re-measure now lives in scripts/bench-all.sh, which
# takes the same settings as knobs and works on a machine other than the one this was calibrated on.
# The old name hard-coded an ~8h estimate that was only ever true for one box.
#
#   bash scripts/bench-all.sh          # equivalent, and prints an ETA for the current settings
exec bash "$(dirname "$0")/bench-all.sh" "$@"
