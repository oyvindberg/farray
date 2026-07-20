#!/usr/bin/env bash
# Preflight for the JMH sweeps — makes a FRESH clone / git worktree actually able to run benchmarks.
#
#   bash scripts/bench-preflight.sh [project...]     # default: benchmarks setbenchmarks
#
# The problem it fixes: GenJmh/GenSetJmh generate the JMH wrapper classes by reading the compiled
# benchmark classes from `started.projectPaths(<proj>).classes`, which the bleep-core version the
# codegen links against resolves to `.bleep/builds/normal/.bloop/<proj>/classes`. Newer bleep CLIs
# compile to `.bleep/projects/<proj>/builds/normal/classes` instead. Where both exist the former is
# a symlink to the latter — but a fresh checkout has never had one created, so the generator reads
# an absent directory, finds ZERO classes, writes an EMPTY META-INF/BenchmarkList, and exits 0.
#
# The failure is silent and looks like a typo: every JMH pattern answers
#   "No matching benchmarks. Miss-spelled regexp?"
# even for benchmarks that plainly exist. The tell is the sourcegen line `Processing 0 classes …`.
#
# So: create the symlink when it is missing and the real classes dir exists. Idempotent, and it
# never touches a path that is already present (if a future bleep makes it a real directory, that
# directory wins and this is a no-op).
set -uo pipefail
cd "$(dirname "$0")/.."

for proj in "${@:-benchmarks setbenchmarks}"; do
  real="$PWD/.bleep/projects/$proj/builds/normal/classes"
  link="$PWD/.bleep/builds/normal/.bloop/$proj/classes"
  if [ ! -e "$link" ] && [ -d "$real" ]; then
    mkdir -p "$(dirname "$link")"
    ln -sfn "$real" "$link"
    echo "▶ preflight: linked $proj classes for the JMH generator"
  fi
done

# Verify the generated benchmark list is non-empty, so an empty sweep is caught HERE rather than
# after hours of "0 matching benchmarks". Only warns: the list is generated during the build, so on
# a truly cold checkout it legitimately does not exist yet.
for pair in "benchmarks-runner:farray.GenJmh" "setbenchmarks-runner:farray.GenSetJmh"; do
  runner="${pair%%:*}"; gen="${pair##*:}"
  list=".bleep/generated-resources/$runner/$gen/META-INF/BenchmarkList"
  if [ -f "$list" ] && [ ! -s "$list" ]; then
    echo "⚠ preflight: $list is EMPTY — the JMH generator saw no classes." >&2
    echo "  Re-run after a successful \`bleep compile $runner\`; if it stays empty, the classes-dir" >&2
    echo "  link above is pointing somewhere the generator cannot read." >&2
  fi
done
