#!/usr/bin/env python3
"""Measurement-consistency report over JMH result JSONs.

For every benchmark cell, JMH reports `score` ± `scoreError` (99.9% CI half-width) plus the raw
per-iteration samples. This aggregates the RELATIVE error (scoreError/score) and the raw
coefficient of variation (stddev/mean over all iterations) per implementation, answering: whose
numbers wobble, and whose hold still?

Usage:
    python3 scripts/bench-variance.py docs/bench-results.json
    python3 scripts/bench-variance.py docs/parts/part-*.json
"""

import json
import statistics
import sys
from collections import defaultdict

VARIANTS = ("farray", "iarray", "list", "vector", "fs2chunk", "ziochunk")


def variant_of(method: str) -> str:
    base = method.split("_", 1)[0]
    return base if base in VARIANTS else ("farray" if base.startswith("farray") else base)


def main(paths: list[str]) -> None:
    rel_err = defaultdict(list)  # variant -> [scoreError/score]
    cv = defaultdict(list)  # variant -> [stddev/mean of raw iterations]
    cells = defaultdict(int)
    for path in paths:
        try:
            entries = json.load(open(path))
        except Exception as ex:
            print(f"skip {path}: {ex}", file=sys.stderr)
            continue
        for e in entries:
            v = variant_of(e["benchmark"].split(".")[-1])
            pm = e["primaryMetric"]
            score, err = pm["score"], pm.get("scoreError")
            if not score or score <= 0:
                continue
            cells[v] += 1
            if err is not None and err == err:  # NaN-safe (single-iteration cells report NaN)
                rel_err[v].append(err / score)
            raw = [x for fork in pm.get("rawData", []) for x in fork]
            if len(raw) >= 3:
                m = statistics.fmean(raw)
                if m > 0:
                    cv[v].append(statistics.stdev(raw) / m)

    def fmt(xs, q):
        return f"{100 * statistics.quantiles(xs, n=100)[q - 1]:6.2f}%" if len(xs) >= 10 else "     —"

    print(f"{'impl':<10} {'cells':>6}  {'relCI med':>9} {'relCI p90':>9}  {'rawCV med':>9} {'rawCV p90':>9}")
    order = sorted(cells, key=lambda v: statistics.median(rel_err[v]) if rel_err[v] else 9)
    for v in order:
        re_, cv_ = rel_err[v], cv[v]
        med = f"{100 * statistics.median(re_):6.2f}%" if re_ else "     —"
        cmed = f"{100 * statistics.median(cv_):6.2f}%" if cv_ else "     —"
        print(f"{v:<10} {cells[v]:>6}  {med:>9} {fmt(re_, 90):>9}  {cmed:>9} {fmt(cv_, 90):>9}")


if __name__ == "__main__":
    main(sys.argv[1:] or ["docs/bench-results.json"])
