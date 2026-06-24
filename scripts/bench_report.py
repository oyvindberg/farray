#!/usr/bin/env python3
"""Render a JMH JSON result file into a self-contained HTML report.

Layout: a "Primitive" section (Int/Long benchmarks) on top, then "String", then
"Diagnostics" (everything not prefixed Int*/Str*/Long*). Each benchmark+op is one
chart; when a benchmark was swept across multiple sizes the chart is a log-scale
line per impl (size on x), otherwise a bar chart.

Usage: python3 scripts/bench_report.py <results.json> <out.html>
"""
import json, sys, math

# known impl variants -> (label, colour). Unknown prefixes fall back to a default.
KNOWN = {
    "farray": ("FArray", "#2e9e5b"), "array": ("Array", "#888888"),
    "iarray": ("IArray", "#c08a2e"), "list": ("List", "#7a7ad0"),
    "vector": ("Vector", "#5aa0c0"), "fs2chunk": ("fs2.Chunk", "#d0874e"),
    "ziochunk": ("zio.Chunk", "#b06fb0"),
    # MixedTree tree-vs-materialized variants
    "farrayTree": ("FArray(tree)", "#2e9e5b"), "farrayMat": ("FArray(flat)", "#7fd0a0"),
    "ziochunkTree": ("zio.Chunk(tree)", "#b06fb0"), "ziochunkMat": ("zio.Chunk(flat)", "#d8b0d8"),
}
ORDER = list(KNOWN.keys())
DEFAULT_COL = "#bbbbbb"

# priority for which param is the x-axis when a benchmark has several
XPRIORITY = ("size", "numChunks", "chunkCount", "numLeaves", "n", "innerSize", "chunkSize", "leafSize")

def label_col(variant):
    return KNOWN.get(variant, (variant, DEFAULT_COL))

def is_ours(variant):
    return variant.startswith("farray")

def section_of(cls):
    if cls.startswith("Str"):  return "String"
    if cls.startswith("Int") or cls.startswith("Long"): return "Primitive"
    return "Diagnostics"

def fmt(x):
    if x >= 1e9: return f"{x/1e9:.2f}B"
    if x >= 1e6: return f"{x/1e6:.1f}M"
    if x >= 1e3: return f"{x/1e3:.1f}k"
    if x >= 1:   return f"{x:.0f}"
    return f"{x:.2g}"

def main(json_path, out_path):
    data = json.load(open(json_path))
    # charts[(section, cls, op_label)][variant] = {xval: score}
    charts, skipped = {}, 0
    for b in data:
        if b["primaryMetric"]["scoreUnit"] != "ops/s":
            skipped += 1
            continue
        full = b["benchmark"]; cls = full.split(".")[-2]; meth = full.split(".")[-1]
        # method is either "<impl>" (single-op class; op implicit in class name) or "<impl>_<op>"
        variant, _, op = meth.partition("_")
        params = {k: v for k, v in (b.get("params") or {}).items()}
        xkey = next((k for k in XPRIORITY if k in params), (sorted(params)[0] if params else None))
        extra = " · ".join(f"{k}={params[k]}" for k in sorted(params) if k != xkey)
        op_label = op + (f"  [{extra}]" if extra else "")
        if xkey is not None and str(params[xkey]).lstrip("-").isdigit():
            xval = int(params[xkey])
        else:
            xval = params.get(xkey, 0) if xkey else 0
        key = (section_of(cls), cls, op_label)
        charts.setdefault(key, {}).setdefault(variant, {})[xval] = b["primaryMetric"]["score"]

    # win / tie / loss: ours (best farray*) vs best competitor, per (chart, x) point
    wins = ties = losses = 0
    for series in charts.values():
        xs = set()
        for d in series.values(): xs |= set(d.keys())
        for x in xs:
            ours = [d[x] for v, d in series.items() if is_ours(v) and x in d]
            comp = [d[x] for v, d in series.items() if not is_ours(v) and x in d]
            if ours and comp:
                r = max(ours) / max(comp)
                if r >= 1.05: wins += 1
                elif r >= 0.95: ties += 1
                else: losses += 1

    # ---- rendering helpers ----
    def line_chart(series, xs):
        W, H, pad_l, pad_b, pad_t, pad_r = 320, 150, 34, 22, 8, 8
        xs = sorted(xs)
        xpos = {x: pad_l + (W - pad_l - pad_r) * (i / max(1, len(xs) - 1)) for i, x in enumerate(xs)}
        allv = [s for d in series.values() for s in d.values() if s > 0]
        lo, hi = math.log10(min(allv)), math.log10(max(allv))
        if hi - lo < 0.5: lo, hi = lo - 0.5, hi + 0.5
        def ypos(v): return pad_t + (H - pad_t - pad_b) * (1 - (math.log10(v) - lo) / (hi - lo))
        out = [f'<svg viewBox="0 0 {W} {H}" class="chart">']
        # y gridlines at each log decade
        d0, d1 = math.ceil(lo), math.floor(hi)
        for dec in range(d0, d1 + 1):
            y = ypos(10 ** dec)
            out.append(f'<line x1="{pad_l}" y1="{y:.1f}" x2="{W-pad_r}" y2="{y:.1f}" class="grid"/>')
            out.append(f'<text x="2" y="{y+3:.1f}" class="ax">{fmt(10.0**dec)}</text>')
        # x ticks
        for x in xs:
            out.append(f'<text x="{xpos[x]:.1f}" y="{H-6}" class="ax" text-anchor="middle">{x}</text>')
        for v in ORDER + [k for k in series if k not in ORDER]:
            if v not in series: continue
            pts = sorted((x, s) for x, s in series[v].items() if s > 0)
            if not pts: continue
            col = label_col(v)[1]
            wid = 2.4 if is_ours(v) else 1.3
            if len(pts) > 1:
                poly = " ".join(f"{xpos[x]:.1f},{ypos(s):.1f}" for x, s in pts)
                out.append(f'<polyline points="{poly}" fill="none" stroke="{col}" stroke-width="{wid}"/>')
            for x, s in pts:
                out.append(f'<circle cx="{xpos[x]:.1f}" cy="{ypos(s):.1f}" r="{2.2 if is_ours(v) else 1.7}" fill="{col}"/>')
        out.append("</svg>")
        return "".join(out)

    def variants_in(series):
        return [v for v in ORDER if v in series] + [v for v in series if v not in ORDER]

    def legend(series):
        items = "".join(
            f'<span class="lg"><i style="background:{label_col(v)[1]}"></i>{label_col(v)[0]}</span>'
            for v in variants_in(series))
        return f'<div class="legend">{items}</div>'

    # ---- build sections ----
    section_html = {}
    for sec in ("Primitive", "String", "Diagnostics"):
        keys = sorted(k for k in charts if k[0] == sec)
        cards = []
        for (_, cls, op) in keys:
            series = charts[(sec, cls, op)]
            xs = set(x for d in series.values() for x in d.keys())
            body = line_chart(series, xs)
            nice = cls.replace("Benchmark", "")
            for p in ("Int", "Str", "Long"):
                if nice.startswith(p): nice = nice[len(p):]; break
            title = nice + (f" · {op}" if op else "")
            cards.append(f'<div class="card"><h3>{title}</h3>{legend(series)}{body}</div>')
        if cards:
            section_html[sec] = f'<h2>{sec}</h2><div class="grid">{"".join(cards)}</div>'

    total = wins + ties + losses
    skip_note = f' · {skipped} non-ops/s results skipped' if skipped else ""
    html = f"""<!doctype html><meta charset="utf8"><title>FArray benchmarks</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
body{{font:13px -apple-system,system-ui,sans-serif;margin:22px;background:#f6f7f9;color:#222}}
h1{{margin:0 0 4px}} h2{{margin:26px 0 10px;padding-bottom:4px;border-bottom:2px solid #d8dbe0;font-size:18px}}
.sub{{color:#666;margin-bottom:16px;font-size:12px}}
.summary{{background:#fff;border:1px solid #e2e5ea;border-radius:8px;padding:10px 14px;margin-bottom:8px;display:inline-block}}
.summary b{{font-size:19px}}
.grid{{display:grid;grid-template-columns:repeat(auto-fill,minmax(330px,1fr));gap:13px}}
.card{{background:#fff;border:1px solid #e2e5ea;border-radius:8px;padding:10px 12px}}
.card h3{{margin:0 0 5px;font-size:12px;font-weight:600;color:#333}}
.legend{{margin:0 0 4px;font-size:10px;color:#555;line-height:1.6}}
.lg{{margin-right:8px;white-space:nowrap}} .lg i{{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:3px;vertical-align:middle}}
.chart{{width:100%;height:auto}}
.grid line.grid{{stroke:#eef0f3;stroke-width:1}} text.ax{{fill:#999;font-size:8px}}
</style>
<h1>FArray — benchmark suite</h1>
<div class="sub">throughput ops/s, log y · x = size (or the swept parameter) · one line per impl · FArray is the bold green line{skip_note}</div>
<div class="summary">FArray vs best competitor (per size point): <b style="color:#2e9e5b">{wins} win</b> · {ties} tie · <b style="color:#c0392b">{losses} loss</b> of {total}</div>
{section_html.get("Primitive","")}
{section_html.get("String","")}
{section_html.get("Diagnostics","")}"""
    open(out_path, "w").write(html)
    print(f"wrote {out_path}: {len(charts)} charts across "
          f"{sum(1 for k in charts if k[0]=='Primitive')} prim / "
          f"{sum(1 for k in charts if k[0]=='String')} str / "
          f"{sum(1 for k in charts if k[0]=='Diagnostics')} diag · "
          f"vs best competitor {wins}W {ties}T {losses}L · {skipped} skipped")

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
