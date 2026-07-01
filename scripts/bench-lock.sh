#!/usr/bin/env bash
# Cooperative benchmarking MUTEX for this box — exactly one JMH consumer at a time, across every
# session/agent working in any checkout of this repo. Two concurrent JMH runs both produce garbage
# (contended numbers) and invite pkill collateral; serialize instead.
#
#   scripts/bench-lock.sh <command...>     wait for the lock, run <command>, release on exit/kill
#
# The sweep runners (bench-run.sh / setbench-run.sh) re-exec themselves under this lock automatically.
# For AD-HOC JMH runs, wrap them yourself:
#   scripts/bench-lock.sh bleep run setbenchmarks-runner -- '<regex>' -f 1 ...
#
# Lock = a directory in /tmp (mkdir is atomic on the same filesystem); the holder's PID is stored
# inside, and a lock whose holder is dead is stolen — so a pkill'd run never wedges the box.
set -uo pipefail
LOCK="${BENCH_LOCK_DIR:-/tmp/farray-bench.lock.d}"

while true; do
  if mkdir "$LOCK" 2>/dev/null; then
    echo "$$" > "$LOCK/pid"
    printf '%s\n' "$*" > "$LOCK/cmd" 2>/dev/null || true
    trap 'rm -rf "$LOCK"' EXIT INT TERM
    break
  fi
  holder="$(cat "$LOCK/pid" 2>/dev/null || true)"
  if [ -n "$holder" ] && ! kill -0 "$holder" 2>/dev/null; then
    echo "bench-lock: stealing stale lock (holder pid $holder is dead)" >&2
    rm -rf "$LOCK"
    continue
  fi
  echo "bench-lock: held by pid ${holder:-unknown} ($(cat "$LOCK/cmd" 2>/dev/null || echo '?')) — waiting…" >&2
  sleep 5
done

"$@"
