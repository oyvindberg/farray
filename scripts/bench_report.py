#!/usr/bin/env python3
"""Turn a JMH JSON result file into a self-contained HTML report.

Usage: python3 scripts/bench_report.py <results.json> <out.html>
"""
import json, sys

VARIANTS = ("farray", "array", "iarray", "list", "vector", "fs2chunk", "ziochunk")
LABELS = {"farray": "FArray", "array": "Array", "iarray": "IArray", "list": "List",
          "vector": "Vector", "fs2chunk": "fs2.Chunk", "ziochunk": "zio.Chunk"}
COLORS = {"farray": "#2e9e5b", "array": "#888", "iarray": "#c08a2e", "list": "#7a7ad0",
          "vector": "#5aa0c0", "fs2chunk": "#d0874e", "ziochunk": "#b06fb0"}
ORDER = list(VARIANTS)

def fmt(x):
    if x >= 1e9: return f"{x/1e9:.2f}B"
    if x >= 1e6: return f"{x/1e6:.1f}M"
    if x >= 1e3: return f"{x/1e3:.1f}k"
    return f"{x:.0f}"

def main(json_path, out_path):
    data = json.load(open(json_path))
    scen = {}
    for b in data:
        parts = b["benchmark"].split(".")
        cls, meth = parts[-2], parts[-1]
        variant = meth.split("_", 1)[0]
        op = meth.split("_", 1)[1] if "_" in meth else ""
        if variant not in VARIANTS:
            continue
        key = cls + (" · " + op if op else "")
        scen.setdefault(key, {})[variant] = b["primaryMetric"]["score"]
    scen = {k: v for k, v in scen.items() if "farray" in v}

    # score FArray vs the BEST competitor in each benchmark (goal: beat everyone everywhere)
    wins = ties = losses = 0
    for v in scen.values():
        comps = [s for k, s in v.items() if k != "farray"]
        if comps:
            r = v["farray"] / max(comps)
            if r >= 1.05: wins += 1
            elif r >= 0.95: ties += 1
            else: losses += 1

    cards = []
    for k in sorted(scen):
        v = scen[k]
        mx = max(v.values())
        best_comp = max((s for var, s in v.items() if var != "farray"), default=0)
        rows = ""
        for var in ORDER:
            if var not in v: continue
            s = v[var]
            w = max(1.5, s / mx * 100)
            ratio = v["farray"] / s
            rtxt = f"FA {ratio:.2f}×" if var != "farray" else ""
            # red bar tip for farray when it loses to the best competitor
            col = COLORS[var]
            if var == "farray" and best_comp and v["farray"] < best_comp * 0.95:
                col = "#c0392b"
            rows += (f'<div class="row"><span class="lbl">{LABELS[var]}</span>'
                     f'<span class="track"><span class="bar" style="width:{w:.1f}%;background:{col}"></span></span>'
                     f'<span class="val">{fmt(s)}<span class="r">{rtxt}</span></span></div>')
        cards.append(f'<div class="card"><h3>{k}</h3>{rows}</div>')

    total = wins + ties + losses
    html = f"""<!doctype html><meta charset="utf8"><title>FArray benchmarks</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
body{{font:14px -apple-system,system-ui,sans-serif;margin:24px;background:#f6f7f9;color:#222}}
h1{{margin:0 0 4px}} .sub{{color:#666;margin-bottom:18px}}
.summary{{background:#fff;border:1px solid #e2e5ea;border-radius:8px;padding:12px 16px;margin-bottom:18px;display:inline-block}}
.summary b{{font-size:20px}}
.grid{{display:grid;grid-template-columns:repeat(auto-fill,minmax(360px,1fr));gap:14px}}
.card{{background:#fff;border:1px solid #e2e5ea;border-radius:8px;padding:12px 14px}}
.card h3{{margin:0 0 8px;font-size:13px;font-weight:600;color:#333}}
.row{{display:flex;align-items:center;margin:3px 0;font-size:12px}}
.lbl{{width:64px;color:#666;flex:none}}
.track{{flex:1;background:#eef0f3;border-radius:3px;height:14px;overflow:hidden}}
.bar{{display:block;height:14px;border-radius:3px}}
.val{{width:104px;text-align:right;flex:none;font-variant-numeric:tabular-nums}}
.val .r{{color:#999;margin-left:6px;font-size:11px}}
</style>
<h1>FArray — benchmark suite</h1>
<div class="sub">throughput ops/s (higher = better) · bars normalised per chart · ratio = FArray ÷ that competitor · vs Array, IArray, List, Vector, fs2.Chunk, zio.Chunk</div>
<div class="summary">vs the best competitor in each — FArray: <b style="color:#2e9e5b">{wins} win</b> · {ties} tie · <b style="color:#c0392b">{losses} loss</b> (of {total} scored)</div>
<div class="grid">{''.join(cards)}</div>"""
    open(out_path, "w").write(html)
    print(f"wrote {out_path}: {len(cards)} charts · vs best competitor {wins}W {ties}T {losses}L")

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
