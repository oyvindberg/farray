#!/usr/bin/env python3
"""Render a JMH JSON result file into a self-contained, interactive HTML report.

Sections: Primitive (Int/Long) · String · Diagnostics, by class prefix. Each benchmark+op is
a log-scale line chart (size on x, one line per impl). Hover a chart for a crosshair + tooltip
comparing every impl's throughput (and its ratio vs FArray) at that size.

Usage: python3 scripts/bench_report.py <results.json> <out.html>
"""
import json, sys, math, re

KNOWN = {
    "farray": ("FArray", "#16a34a"), "array": ("Array", "#94a3b8"),
    "iarray": ("IArray", "#f59e0b"), "list": ("List", "#8b5cf6"),
    "vector": ("Vector", "#0ea5e9"), "fs2chunk": ("fs2.Chunk", "#f97316"),
    "ziochunk": ("zio.Chunk", "#ec4899"), "scalaRange": ("Range", "#64748b"),
    "farrayTree": ("FArray·tree", "#16a34a"), "farrayMat": ("FArray·flat", "#4ade80"),
    "ziochunkTree": ("zio·tree", "#ec4899"), "ziochunkMat": ("zio·flat", "#f9a8d4"),
}
ORDER = list(KNOWN.keys())
XPRIORITY = ("size", "numChunks", "chunkCount", "numLeaves", "n", "innerSize", "chunkSize", "leafSize")

def lc(v):    return KNOWN.get(v, (v, "#cbd5e1"))
def ours(v):  return v.startswith("farray")
def section(c):
    if c.startswith("ListLike"): return "ListLike"
    if c.startswith("Str"): return "String"
    if c.startswith(("Int", "Long")): return "Primitive"
    return "Diagnostics"

def main(json_path, out_path):
    data = json.load(open(json_path))
    charts, skipped = {}, 0
    for b in data:
        if b["primaryMetric"]["scoreUnit"] != "ops/s":
            skipped += 1; continue
        full = b["benchmark"]; cls = full.split(".")[-2]; meth = full.split(".")[-1]
        variant, _, op = meth.partition("_")
        params = dict(b.get("params") or {})
        xkey = next((k for k in XPRIORITY if k in params), (sorted(params)[0] if params else None))
        m = re.match(r"^([A-Za-z]+)(\d+)$", op) if not params else None  # size baked into method name (e.g. create02) -> one swept chart
        if m:
            op_label = m.group(1); xval = int(m.group(2))
        else:
            extra = " · ".join(f"{k}={params[k]}" for k in sorted(params) if k != xkey)
            op_label = op + (f"  [{extra}]" if extra else "")
            xval = int(params[xkey]) if xkey and str(params[xkey]).lstrip("-").isdigit() else 0
        charts.setdefault((section(cls), cls, op_label), {}).setdefault(variant, {})[xval] = b["primaryMetric"]["score"]

    def tally(series):
        w = t = l = 0
        xs = set().union(*[set(d) for d in series.values()]) if series else set()
        for x in xs:
            o = [d[x] for v, d in series.items() if ours(v) and x in d]
            c = [d[x] for v, d in series.items() if not ours(v) and x in d]
            if o and c:
                r = max(o) / max(c)
                w, t, l = (w + (r >= 1.05), t + (0.95 <= r < 1.05), l + (r < 0.95))
        return w, t, l

    W, H, PL, PB, PT, PR = 300, 158, 30, 30, 10, 8
    VC = {"w": "#16a34a", "t": "#cbd5e1", "l": "#ef4444"}
    cdata, cards = {}, {sec: [] for sec in ("Primitive", "String", "ListLike", "Diagnostics")}
    sec_wtl = {sec: [0, 0, 0] for sec in cards}
    cid = 0
    for (sec, cls, op) in sorted(charts):
        series = charts[(sec, cls, op)]
        w, t, l = tally(series)
        sec_wtl[sec] = [sec_wtl[sec][i] + v for i, v in enumerate((w, t, l))]
        xs = sorted(set(x for d in series.values() for x in d))
        xpx = [PL + (W - PL - PR) * (i / max(1, len(xs) - 1)) for i in range(len(xs))]
        allv = [s for d in series.values() for s in d.values() if s > 0]
        lo, hi = math.log10(min(allv)), math.log10(max(allv))
        if hi - lo < 0.4: lo, hi = lo - 0.4, hi + 0.4
        ypx = lambda v: PT + (H - PT - PB) * (1 - (math.log10(v) - lo) / (hi - lo))
        svg = [f'<svg viewBox="0 0 {W} {H}" id="s{cid}">']
        for dec in range(math.ceil(lo), math.floor(hi) + 1):
            y = ypx(10 ** dec)
            svg.append(f'<line x1="{PL}" y1="{y:.1f}" x2="{W-PR}" y2="{y:.1f}" class="g"/>')
            svg.append(f'<text x="3" y="{y+3:.1f}" class="ax">{nf(10.0**dec)}</text>')
        for i, x in enumerate(xs):
            svg.append(f'<text x="{xpx[i]:.1f}" y="{H-7}" class="ax" text-anchor="middle">{x}</text>')
            o = [d[x] for v, d in series.items() if ours(v) and x in d]
            c = [d[x] for v, d in series.items() if not ours(v) and x in d]
            if o and c:
                rr = max(o) / max(c); vd = "w" if rr >= 1.05 else ("t" if rr >= 0.95 else "l")
                svg.append(f'<circle cx="{xpx[i]:.1f}" cy="{H-18}" r="3" fill="{VC[vd]}"/>')
        ser = []
        for v in [v for v in ORDER if v in series] + [v for v in series if v not in ORDER]:
            col = lc(v)[1]; pts = [(i, series[v][x]) for i, x in enumerate(xs) if x in series[v] and series[v][x] > 0]
            if not pts: continue
            if len(pts) > 1:
                poly = " ".join(f"{xpx[i]:.1f},{ypx(s):.1f}" for i, s in pts)
                svg.append(f'<polyline points="{poly}" fill="none" stroke="{col}" stroke-width="{2.6 if ours(v) else 1.4}" stroke-linejoin="round" stroke-linecap="round" opacity="{1 if ours(v) else .9}"/>')
            for i, s in pts:
                svg.append(f'<circle cx="{xpx[i]:.1f}" cy="{ypx(s):.1f}" r="{2.4 if ours(v) else 1.8}" fill="{col}"/>')
            vals = [series[v].get(x) for x in xs]
            yps = [round(ypx(series[v][x]), 1) if x in series[v] and series[v][x] > 0 else None for x in xs]
            ser.append({"l": lc(v)[0], "c": col, "o": ours(v), "v": vals, "y": yps})
        svg.append(f'<line class="cross" x1="0" y1="0" x2="0" y2="0"/><g class="hl"></g>')
        svg.append(f'<rect class="hot" x="0" y="0" width="{W}" height="{H}"/></svg>')
        cdata[f"s{cid}"] = {"xs": xs, "xpx": [round(p, 1) for p in xpx], "h": H, "ser": ser}
        nice = cls.replace("Benchmark", "")
        for p in ("Int", "Str", "Long"):
            if nice.startswith(p): nice = nice[len(p):]; break
        title = nice + (f" · {op}" if op else "")
        badge = f'<span class="wl {"win" if (w and not l) else ("loss" if l>w else "mix")}">{w}/{t}/{l}</span>' if (w + t + l) else ""
        leg = "".join(f'<span class="lg"><i style="background:{s["c"]}"></i>{s["l"]}</span>' for s in ser)
        cards[sec].append(f'<div class="card" id="c{cid}"><h3>{title}{badge}</h3><div class="legend">{leg}</div>'
                          f'<div class="cw">{"".join(svg)}<div class="tip"></div></div></div>')
        cid += 1

    W_, T_, L_ = (sum(c) for c in zip(*sec_wtl.values()))
    def sechtml(sec):
        if not cards[sec]: return ""
        sw, st, sl = sec_wtl[sec]
        return (f'<h2>{sec} <span class="secwtl"><b>{sw}</b> win · {st} tie · <b>{sl}</b> loss</span></h2>'
                f'<div class="grid">{"".join(cards[sec])}</div>')

    html = f"""<!doctype html><html><head><meta charset="utf8"><title>FArray benchmarks</title>
<meta name="viewport" content="width=device-width, initial-scale=1"><style>
:root{{--fa:#16a34a;--bg:#f7f8fa;--card:#fff;--ink:#1f2430;--mut:#7a8699;--line:#eceef2}}
*{{box-sizing:border-box}}
body{{font:13px/1.5 ui-sans-serif,system-ui,-apple-system,"Segoe UI",Inter,sans-serif;margin:0;background:
  radial-gradient(1200px 500px at 80% -10%,#eafff2 0,var(--bg) 55%) fixed;color:var(--ink);-webkit-font-smoothing:antialiased}}
.wrap{{max-width:1500px;margin:0 auto;padding:30px 26px 60px}}
.hero{{display:flex;align-items:baseline;gap:18px;flex-wrap:wrap;margin-bottom:6px}}
h1{{margin:0;font-size:26px;font-weight:700;letter-spacing:-.02em}}
h1 b{{color:var(--fa)}}
.sub{{color:var(--mut);font-size:12.5px;margin:2px 0 20px}}
.scorebar{{display:inline-flex;gap:0;border-radius:11px;overflow:hidden;box-shadow:0 1px 3px rgba(20,30,50,.1);font-weight:600}}
.scorebar div{{padding:7px 15px;font-size:13px}}
.sc-w{{background:#dcfce7;color:#15803d}} .sc-t{{background:#f1f5f9;color:#475569}} .sc-l{{background:#fee2e2;color:#b91c1c}}
h2{{margin:34px 0 14px;font-size:19px;font-weight:680;letter-spacing:-.01em;display:flex;align-items:baseline;gap:12px}}
h2 .secwtl{{font-size:12px;font-weight:500;color:var(--mut)}} h2 .secwtl b{{color:var(--ink)}}
.grid{{display:grid;grid-template-columns:repeat(auto-fill,minmax(310px,1fr));gap:15px}}
.card{{background:var(--card);border:1px solid var(--line);border-radius:14px;padding:13px 14px 6px;
  box-shadow:0 1px 2px rgba(20,30,50,.04);transition:box-shadow .15s,transform .15s}}
.card:hover{{box-shadow:0 8px 26px rgba(20,30,50,.10);transform:translateY(-2px)}}
.card h3{{margin:0 0 7px;font-size:12.5px;font-weight:650;display:flex;align-items:center;gap:7px}}
.wl{{margin-left:auto;font-size:10px;font-weight:700;padding:2px 7px;border-radius:20px}}
.wl.win{{background:#dcfce7;color:#15803d}} .wl.loss{{background:#fee2e2;color:#b91c1c}} .wl.mix{{background:#f1f5f9;color:#64748b}}
.legend{{display:flex;flex-wrap:wrap;gap:3px 9px;margin-bottom:3px;font-size:10px;color:var(--mut)}}
.lg{{white-space:nowrap}} .lg i{{display:inline-block;width:8px;height:8px;border-radius:2px;margin-right:3px;vertical-align:middle}}
.cw{{position:relative}} svg{{width:100%;height:auto;display:block;cursor:crosshair}}
svg .g{{stroke:var(--line);stroke-width:1}} svg text.ax{{fill:#a3adbd;font-size:7.5px}}
svg .cross{{stroke:#9aa6b6;stroke-width:1;stroke-dasharray:3 3;opacity:0}}
svg .hot{{fill:#fff;fill-opacity:0}} svg circle{{pointer-events:none}} svg polyline{{pointer-events:none}}
.tip{{position:absolute;pointer-events:none;opacity:0;transition:opacity .08s;z-index:9;min-width:150px;
  background:rgba(24,28,40,.97);color:#fff;border-radius:10px;padding:8px 10px;font-size:11px;
  box-shadow:0 10px 30px rgba(0,0,0,.28);backdrop-filter:blur(4px)}}
.tip .th{{font-weight:700;font-size:10.5px;color:#aeb8c8;margin-bottom:5px;letter-spacing:.02em;display:flex;align-items:center;gap:6px}}
.tip .vd{{font-size:9px;font-weight:800;padding:1px 6px;border-radius:9px;letter-spacing:.04em}}
.tip .vd.w{{background:#14532d;color:#86efac}} .tip .vd.t{{background:#334155;color:#cbd5e1}} .tip .vd.l{{background:#5c1620;color:#fca5a5}}
.tip .row{{display:flex;align-items:center;gap:6px;padding:1.5px 0}}
.tip .row.me{{font-weight:700}}
.tip .row i{{width:8px;height:8px;border-radius:2px;flex:none}}
.tip .nm{{flex:1;white-space:nowrap}} .tip .vv{{font-variant-numeric:tabular-nums}}
.tip .rt{{font-size:9.5px;padding:1px 5px;border-radius:10px;font-weight:700}}
.tip .rt.up{{background:#14532d;color:#86efac}} .tip .rt.dn{{background:#5c1620;color:#fca5a5}}
</style></head><body><div class="wrap">
<div class="hero"><h1><b>FArray</b> — benchmark suite</h1>
<div class="scorebar"><div class="sc-w">{W_} win</div><div class="sc-t">{T_} tie</div><div class="sc-l">{L_} loss</div></div></div>
<div class="sub">throughput ops/s (log y) · x = size or swept parameter · FArray = bold green · hover any chart to compare numbers · vs the best competitor per size point{(' · '+str(skipped)+' non-ops/s skipped') if skipped else ''}</div>
{sechtml("Primitive")}{sechtml("String")}{sechtml("ListLike")}{sechtml("Diagnostics")}
</div><script>
const C={json.dumps(cdata,separators=(',',':'))};
function nf(x){{if(x>=1e9)return (x/1e9).toFixed(2)+'B';if(x>=1e6)return (x/1e6).toFixed(1)+'M';if(x>=1e3)return (x/1e3).toFixed(1)+'k';if(x>=1)return Math.round(x);return x.toPrecision(2);}}
document.querySelectorAll('.card').forEach(card=>{{
  const svg=card.querySelector('svg'); if(!svg)return; const d=C[svg.id]; if(!d)return;
  const tip=card.querySelector('.tip'),cross=svg.querySelector('.cross'),hl=svg.querySelector('.hl'),cw=card.querySelector('.cw');
  svg.addEventListener('mousemove',e=>{{
    const r=svg.getBoundingClientRect(), W=300, x=(e.clientX-r.left)/r.width*W;
    let i=0,bd=1e9; d.xpx.forEach((p,j)=>{{const q=Math.abs(p-x); if(q<bd){{bd=q;i=j;}}}});
    const px=d.xpx[i];
    cross.setAttribute('x1',px);cross.setAttribute('x2',px);cross.setAttribute('y1',6);cross.setAttribute('y2',d.h-30);cross.style.opacity=1;
    const present=d.ser.map(s=>({{s,v:s.v[i],y:s.y[i]}})).filter(o=>o.v!=null);
    present.sort((a,b)=>b.v-a.v);
    const fa=present.find(o=>o.s.o), comp=present.filter(o=>!o.s.o);
    let vd='',vl='';
    if(fa&&comp.length){{const bc=Math.max(...comp.map(o=>o.v)),k=fa.v/bc; vd=k>=1.05?'w':(k>=.95?'t':'l'); vl=vd=='w'?'WIN':vd=='t'?'TIE':'LOSS';}}
    let mk='',rows='';
    present.forEach(o=>{{
      mk+=`<circle cx="${{px}}" cy="${{o.y}}" r="3.4" fill="${{o.s.c}}" stroke="#fff" stroke-width="1.2"/>`;
      let rt='';
      if(fa&&!o.s.o){{const k=fa.v/o.v; rt=`<span class="rt ${{k>=1?'up':'dn'}}">${{k>=1?k.toFixed(2)+'×':'÷'+(1/k).toFixed(2)}}</span>`;}}
      rows+=`<div class="row ${{o.s.o?'me':''}}"><i style="background:${{o.s.c}}"></i><span class="nm">${{o.s.l}}</span><span class="vv">${{nf(o.v)}}</span>${{rt}}</div>`;
    }});
    hl.innerHTML=mk;
    tip.innerHTML=`<div class="th">size ${{d.xs[i]}}${{vl?` <span class="vd ${{vd}}">${{vl}}</span>`:''}}</div>${{rows}}`; tip.style.opacity=1;
    const cr=cw.getBoundingClientRect(); let tx=e.clientX-cr.left+14,ty=e.clientY-cr.top+10;
    if(tx+170>cr.width)tx=e.clientX-cr.left-170; if(tx<0)tx=4;
    tip.style.left=tx+'px'; tip.style.top=ty+'px';
  }});
  svg.addEventListener('mouseleave',()=>{{tip.style.opacity=0;cross.style.opacity=0;hl.innerHTML='';}});
}});
</script></body></html>"""
    open(out_path, "w").write(html)
    print(f"wrote {out_path}: {len(charts)} charts · {W_}W {T_}T {L_}L · {skipped} skipped")

def nf(x):
    if x >= 1e9: return f"{x/1e9:.2f}B"
    if x >= 1e6: return f"{x/1e6:.1f}M"
    if x >= 1e3: return f"{x/1e3:.1f}k"
    if x >= 1:   return f"{x:.0f}"
    return f"{x:.2g}"

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
