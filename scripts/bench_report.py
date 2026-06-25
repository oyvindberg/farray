#!/usr/bin/env python3
"""Render a JMH JSON result file into a self-contained, interactive HTML report.

Sections: Primitive (Int/Long) · String · ListLike · Diagnostics, by class prefix. Each
benchmark+op is a card holding a grouped bar chart: x = size (or swept param), and at each
size a cluster of bars (one per impl, FArray bold/emerald, competitors muted). Bars are
normalised WITHIN each size group (each group scaled to its own max) so the relative
comparison at each size is what reads — throughput spans orders of magnitude across sizes.

A subtle background band behind each size group is tinted by FArray's verdict there (soft
green win · gray tie · red loss). The whole card frame/header is tinted by the aggregate
verdict, with a W/T/L count. Hover a size group for a crosshair + tooltip comparing every
impl's throughput (and its ratio vs FArray) at that size.

Usage: python3 scripts/bench_report.py <results.json> <out.html>
"""
import json, sys, re, math

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

    def verdict_at(series, x):
        """Return ('w'|'t'|'l'|None, ratio) for FArray vs best competitor at size x."""
        o = [d[x] for v, d in series.items() if ours(v) and x in d]
        c = [d[x] for v, d in series.items() if not ours(v) and x in d]
        if not (o and c): return None, None
        r = max(o) / max(c)
        return ("w" if r >= 1.05 else ("t" if r >= 0.95 else "l")), r

    def tally(series):
        w = t = l = 0
        xs = set().union(*[set(d) for d in series.values()]) if series else set()
        for x in xs:
            vd, _ = verdict_at(series, x)
            if vd == "w": w += 1
            elif vd == "t": t += 1
            elif vd == "l": l += 1
        return w, t, l

    def band_color(r):
        """Size-group band, scaled by ratio: deep green (big win) · pale yellow (~1x) · red by ~1.15x · screaming red by 10x."""
        if r is None: return "#f6f8fa"
        if r >= 1.05:
            f = min(math.log10(r), 1.0)                       # 0 @1x .. 1 @10x
            return f"hsl(150,{50+38*f:.0f}%,{96-18*f:.0f}%)"
        if r >= 0.95: return "hsl(48,92%,95%)"                # ~1x tie: pale yellow
        f  = min(math.log10(1.0 / r), 1.0)                    # competitor faster: 0.06 @1.15x · 0.30 @2x · 1 @10x
        hp = min(f / 0.061, 1.0)                              # hue reaches full red by ~1.15x
        return f"hsl({48*(1-hp):.0f},{84+13*f:.0f}%,{93-15*hp-23*f:.0f}%)"

    def edge_color(r):
        """Saturated card top-border, scaled by the card's most dramatic ratio."""
        if r is None: return "#cbd5e1"
        if r >= 1.05:
            f = min(math.log10(r), 1.0); return f"hsl(150,68%,{52-12*f:.0f}%)"
        if r >= 0.95: return "#fbbf24"
        f  = min(math.log10(1.0 / r), 1.0)
        hp = min(f / 0.061, 1.0)
        return f"hsl({45*(1-hp):.0f},{82+13*f:.0f}%,{66-8*hp-20*f:.0f}%)"

    # ---- geometry ----
    W, H = 340, 168
    PL, PR, PT, PB = 6, 6, 12, 22          # plot insets; PB leaves room for size labels + verdict dots
    BAR_TOP = PT                            # top of the bar plotting area
    BAR_BOT = H - PB                        # baseline (bars grow up from here)
    PLOT_H  = BAR_BOT - BAR_TOP
    VBAND = {"w": "#ecfdf3", "t": "#f6f8fa", "l": "#fef2f2"}   # soft size-group bands
    VDOT  = {"w": "#16a34a", "t": "#cbd5e1", "l": "#ef4444"}

    cdata, cards = {}, {sec: [] for sec in ("Primitive", "String", "ListLike", "Diagnostics")}
    sec_wtl = {sec: [0, 0, 0] for sec in cards}
    cid = 0
    for (sec, cls, op) in sorted(charts):
        series = charts[(sec, cls, op)]
        w, t, l = tally(series)
        sec_wtl[sec] = [sec_wtl[sec][i] + v for i, v in enumerate((w, t, l))]
        xs = sorted(set(x for d in series.values() for x in d))
        # impl draw order: known order first, then any extras
        impls = [v for v in ORDER if v in series] + [v for v in series if v not in ORDER]

        n = len(xs)
        plot_w = W - PL - PR
        gw = plot_w / max(1, n)                      # width allotted to each size group
        GPAD = min(10.0, gw * 0.16)                  # inner padding inside a group
        inner = gw - 2 * GPAD
        nb = max(1, len(impls))
        bw = inner / nb if nb else inner             # bar width
        BAR_MIN = 2.0                                # floor so a present-but-tiny bar still shows

        svg = [f'<svg viewBox="0 0 {W} {H}" id="s{cid}">']
        # verdict background bands (per size group)
        band_svg, label_svg, bar_svg, dot_svg = [], [], [], []
        groups = []   # per-size payload for JS hover
        ratios = []   # per-size farray/best, for scaled card frame
        for i, x in enumerate(xs):
            gx = PL + i * gw
            vd, rr = verdict_at(series, x)
            if rr is not None: ratios.append(rr)
            if vd:
                band_svg.append(f'<rect x="{gx:.1f}" y="{BAR_TOP-2:.1f}" width="{gw:.1f}" '
                                f'height="{PLOT_H+4:.1f}" rx="5" fill="{band_color(rr)}"/>')
                dot_svg.append(f'<circle cx="{gx+gw/2:.1f}" cy="{BAR_BOT+12:.1f}" r="2.6" fill="{VDOT[vd]}"/>')
            label_svg.append(f'<text x="{gx+gw/2:.1f}" y="{BAR_BOT+9:.1f}" class="ax" text-anchor="middle">{nf_axis(x)}</text>')
            # normalise within this size group
            present = [(v, series[v][x]) for v in impls if x in series[v] and series[v][x] > 0]
            gmax = max((s for _, s in present), default=0) or 1
            bars = []   # for JS: list aligned with impls present
            for bi, (v, s) in enumerate(present):
                bx = gx + GPAD + bi * bw
                hgt = max(BAR_MIN, PLOT_H * (s / gmax))
                by = BAR_BOT - hgt
                col = lc(v)[1]; mine = ours(v)
                cls_b = "bar me" if mine else "bar"
                bar_svg.append(f'<rect class="{cls_b}" x="{bx:.2f}" y="{by:.2f}" width="{max(0.8,bw-0.7):.2f}" '
                               f'height="{hgt:.2f}" rx="1.5" fill="{col}" '
                               f'{"" if mine else "opacity=\"0.55\""}/>')
                bars.append({"l": lc(v)[0], "c": col, "o": mine, "v": s})
            # sort bars desc by value for tooltip
            groups.append({"x": x, "cx": round(gx + gw / 2, 1), "gx": round(gx, 1),
                           "gw": round(gw, 1), "vd": vd or "", "bars": bars})

        svg += band_svg + bar_svg + label_svg + dot_svg
        # baseline
        svg.append(f'<line x1="{PL}" y1="{BAR_BOT:.1f}" x2="{W-PR}" y2="{BAR_BOT:.1f}" class="base"/>')
        # hover crosshair (a translucent group highlight) + hot area
        svg.append('<rect class="ghi" x="0" y="0" width="0" height="0" rx="5"/>')
        svg.append(f'<rect class="hot" x="0" y="0" width="{W}" height="{H}"/></svg>')
        cdata[f"s{cid}"] = {"groups": groups, "barTop": BAR_TOP, "plotH": PLOT_H, "barBot": BAR_BOT, "w": W}

        nice = cls.replace("Benchmark", "")
        for p in ("Int", "Str", "Long"):
            if nice.startswith(p): nice = nice[len(p):]; break
        title = nice + (f" · {op}" if op else "")
        # aggregate frame verdict
        if w + t + l:
            if w > l: agg = "win"
            elif l > w: agg = "loss"
            else: agg = "mix"
        else:
            agg = "mix"
        frame = ""
        if ratios:
            r_edge = min(ratios) if agg == "loss" else (max(ratios) if agg == "win" else None)
            if r_edge is not None: frame = f' style="border-top-color:{edge_color(r_edge)}"'
        badge = (f'<span class="wl {agg}">'
                 f'<b class="w">{w}</b><span class="sep">·</span><b class="t">{t}</b>'
                 f'<span class="sep">·</span><b class="l">{l}</b></span>') if (w + t + l) else ""
        leg = "".join(f'<span class="lg{" me" if s_o else ""}"><i style="background:{c}"></i>{nm}</span>'
                      for nm, c, s_o in
                      [(lc(v)[0], lc(v)[1], ours(v)) for v in impls
                       if any(x in series[v] and series[v][x] > 0 for x in xs)])
        cards[sec].append(
            f'<div class="card {agg}" id="c{cid}"{frame}><div class="chead">'
            f'<h3>{title}</h3>{badge}</div>'
            f'<div class="legend">{leg}</div>'
            f'<div class="cw">{"".join(svg)}<div class="tip"></div></div></div>')
        cid += 1

    W_, T_, L_ = (sum(c) for c in zip(*sec_wtl.values()))

    # ---- score leaderboard: per-structure GEOMETRIC mean of (best throughput / structure throughput) ----
    # In every (benchmark, size) cell the FASTEST structure present = 1.00; each structure's ratio =
    # best throughput / its own (>= 1). The score is the GEOMETRIC mean of those ratios per structure
    # (section + total) — geometric so the structural-op blowouts (1000-30000x) don't swamp it the way
    # an arithmetic mean would. 1.00 = fastest everywhere; N = typically N times slower than the best.
    SUBV = {"farrayTree", "farrayMat", "ziochunkTree", "ziochunkMat"}   # diagnostic decompositions
    SECNAMES = ("Primitive", "String", "ListLike", "Diagnostics")
    sc_sum = {}                                        # (variant, col) -> [sum_ln_ratio, count]
    for (sec, cls, op), series in charts.items():
        for x in set(x for d in series.values() for x in d):
            present = [(v, series[v][x]) for v in series
                       if v not in SUBV and x in series[v] and series[v][x] > 0]
            if len(present) < 2: continue
            best = max(s for _, s in present)           # fastest structure in this cell = 1.00
            for v, s in present:
                lr = math.log(best / s)                 # log-ratio (>= 0), summed for the geometric mean
                for col in (sec, "TOTAL"):
                    a = sc_sum.setdefault((v, col), [0.0, 0]); a[0] += lr; a[1] += 1
    def sc_get(v, col):
        a = sc_sum.get((v, col)); return math.exp(a[0] / a[1]) if a and a[1] else None
    def sc_cls(x):
        if x is None: return "sc-na"
        return "sc-hi" if x <= 1.3 else ("sc-lo" if x > 3.0 else "sc-mid")   # near-best green · >=3x slow red
    sc_cols = list(SECNAMES) + ["TOTAL"]
    sc_vars = [v for v in ORDER if v not in SUBV and (v, "TOTAL") in sc_sum]
    sc_vars += [v for v in {k[0] for k in sc_sum} - set(sc_vars) if v not in SUBV]
    sc_vars.sort(key=lambda v: (sc_get(v, "TOTAL") or 1e9))     # leaderboard, fastest first (lowest FArray/x)
    sc_rows = ""
    for v in sc_vars:
        tds = ""
        for col in sc_cols:
            x = sc_get(v, col); tot = " sc-total" if col == "TOTAL" else ""
            tds += (f'<td class="{sc_cls(x)}{tot}">{x:.2f}</td>' if x is not None
                    else f'<td class="sc-na{tot}">·</td>')
        sc_rows += (f'<tr class="srow{" me" if ours(v) else ""}"><td class="snm">'
                    f'<i style="background:{lc(v)[1]}"></i>{lc(v)[0]}</td>{tds}</tr>')
    score_table = (
        '<div class="scorewrap"><table class="score"><thead><tr><th class="snm">structure</th>'
        + "".join(f'<th{" class=sc-total" if c=="TOTAL" else ""}>{c}</th>' for c in sc_cols)
        + f'</tr></thead><tbody>{sc_rows}</tbody></table>'
        + '<div class="scorenote"><b>How to read this.</b> For every benchmark at every size, the '
          'fastest structure there scores <b>1.00</b>; every other scores how many times slower it ran '
          'in that cell. Each number below is the <b>geometric mean</b> of those per-cell ratios across '
          'the group (geometric so a handful of extreme ops do not dominate). <b>Lower is better</b> — '
          '1.00 = fastest everywhere, 3.0 = typically 3× off the best. Columns are the benchmark groups; '
          'TOTAL is overall.</div></div>')

    def sechtml(sec):
        if not cards[sec]: return ""
        sw, st, sl = sec_wtl[sec]
        return (f'<h2>{sec} <span class="secwtl"><b class="gw">{sw}</b> win · {st} tie · '
                f'<b class="gl">{sl}</b> loss</span></h2>'
                f'<div class="grid">{"".join(cards[sec])}</div>')

    html = f"""<!doctype html><html><head><meta charset="utf8"><title>FArray benchmarks</title>
<meta name="viewport" content="width=device-width, initial-scale=1"><style>
:root{{--fa:#16a34a;--bg:#f7f8fa;--card:#fff;--ink:#1f2430;--mut:#7a8699;--line:#eceef2;
  --win:#16a34a;--loss:#ef4444}}
*{{box-sizing:border-box}}
body{{font:13px/1.5 ui-sans-serif,system-ui,-apple-system,"Segoe UI",Inter,sans-serif;margin:0;background:
  radial-gradient(1200px 500px at 80% -10%,#eafff2 0,var(--bg) 55%) fixed;color:var(--ink);-webkit-font-smoothing:antialiased}}
.wrap{{max-width:1560px;margin:0 auto;padding:30px 26px 60px}}
.hero{{display:flex;align-items:baseline;gap:18px;flex-wrap:wrap;margin-bottom:6px}}
h1{{margin:0;font-size:26px;font-weight:700;letter-spacing:-.02em}}
h1 b{{color:var(--fa)}}
.sub{{color:var(--mut);font-size:12.5px;margin:2px 0 20px;max-width:900px}}
.scorebar{{display:inline-flex;gap:0;border-radius:11px;overflow:hidden;box-shadow:0 1px 3px rgba(20,30,50,.1);font-weight:600}}
.scorebar div{{padding:7px 15px;font-size:13px}}
.sc-w{{background:#dcfce7;color:#15803d}} .sc-t{{background:#f1f5f9;color:#475569}} .sc-l{{background:#fee2e2;color:#b91c1c}}
h2{{margin:34px 0 14px;font-size:19px;font-weight:680;letter-spacing:-.01em;display:flex;align-items:baseline;gap:12px}}
h2 .secwtl{{font-size:12px;font-weight:500;color:var(--mut)}}
.gw{{color:var(--win)}} .gl{{color:var(--loss)}}
.grid{{display:grid;grid-template-columns:repeat(auto-fill,minmax(340px,1fr));gap:15px}}
.card{{background:var(--card);border:1px solid var(--line);border-top:3px solid var(--line);border-radius:14px;
  padding:11px 13px 7px;box-shadow:0 1px 2px rgba(20,30,50,.04);transition:box-shadow .15s,transform .15s}}
.card:hover{{box-shadow:0 8px 26px rgba(20,30,50,.10);transform:translateY(-2px)}}
.card.win{{border-top-color:#34d399;background:linear-gradient(#f3fdf7,#fff 60px)}}
.card.loss{{border-top-color:#f87171;background:linear-gradient(#fff5f5,#fff 60px)}}
.card.mix{{border-top-color:#cbd5e1}}
.chead{{display:flex;align-items:center;gap:8px;margin-bottom:5px}}
.card h3{{margin:0;font-size:12.5px;font-weight:650;line-height:1.25}}
.wl{{margin-left:auto;font-size:10px;font-weight:800;padding:2px 8px;border-radius:20px;
  display:inline-flex;align-items:center;gap:3px;flex:none;font-variant-numeric:tabular-nums}}
.wl .sep{{opacity:.4;font-weight:600}}
.wl.win{{background:#dcfce7}} .wl.loss{{background:#fee2e2}} .wl.mix{{background:#f1f5f9}}
.wl .w{{color:#15803d}} .wl .t{{color:#64748b}} .wl .l{{color:#b91c1c}}
.legend{{display:flex;flex-wrap:wrap;gap:3px 9px;margin-bottom:2px;font-size:10px;color:var(--mut)}}
.lg{{white-space:nowrap}} .lg.me{{color:var(--ink);font-weight:600}}
.lg i{{display:inline-block;width:8px;height:8px;border-radius:2px;margin-right:3px;vertical-align:middle}}
.cw{{position:relative}} svg{{width:100%;height:auto;display:block;cursor:crosshair}}
svg text.ax{{fill:#94a0b2;font-size:8px;font-variant-numeric:tabular-nums}}
svg .base{{stroke:#e2e6ec;stroke-width:1}}
svg .ghi{{fill:#1f2430;fill-opacity:0;transition:fill-opacity .08s}}
svg .hot{{fill:#fff;fill-opacity:0}} svg rect.bar{{transition:opacity .1s}}
.tip{{position:absolute;pointer-events:none;opacity:0;transition:opacity .08s;z-index:9;min-width:158px;
  background:rgba(24,28,40,.97);color:#fff;border-radius:10px;padding:8px 10px;font-size:11px;
  box-shadow:0 10px 30px rgba(0,0,0,.28);backdrop-filter:blur(4px)}}
.tip .th{{font-weight:700;font-size:10.5px;color:#aeb8c8;margin-bottom:5px;letter-spacing:.02em;display:flex;align-items:center;gap:6px}}
.tip .vd{{font-size:9px;font-weight:800;padding:1px 6px;border-radius:9px;letter-spacing:.04em;margin-left:auto}}
.tip .vd.w{{background:#14532d;color:#86efac}} .tip .vd.t{{background:#334155;color:#cbd5e1}} .tip .vd.l{{background:#5c1620;color:#fca5a5}}
.tip .row{{display:flex;align-items:center;gap:6px;padding:1.5px 0}}
.tip .row.me{{font-weight:700}}
.tip .row i{{width:8px;height:8px;border-radius:2px;flex:none}}
.tip .nm{{flex:1;white-space:nowrap}} .tip .vv{{font-variant-numeric:tabular-nums}}
.tip .rt{{font-size:9.5px;padding:1px 5px;border-radius:10px;font-weight:700}}
.tip .rt.up{{background:#14532d;color:#86efac}} .tip .rt.dn{{background:#5c1620;color:#fca5a5}}
.scorewrap{{margin:4px 0 28px;overflow-x:auto}}
table.score{{border-collapse:collapse;font-size:12.5px;font-variant-numeric:tabular-nums;background:var(--card);box-shadow:0 1px 3px rgba(20,30,50,.08);border-radius:11px;overflow:hidden}}
table.score th{{background:#f1f5f9;color:#475569;font-weight:650;padding:8px 14px;text-align:right;font-size:11px;letter-spacing:.02em;white-space:nowrap}}
table.score th.snm{{text-align:left}}
table.score td{{padding:6px 14px;text-align:right;border-top:1px solid var(--line)}}
table.score td.snm{{text-align:left;font-weight:600;white-space:nowrap}}
table.score td.snm i{{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:7px;vertical-align:middle}}
table.score tr.me{{background:#f3fdf7}}
table.score tr.me td.snm{{color:var(--fa);font-weight:750}}
table.score td.sc-hi{{color:#15803d;font-weight:650}} table.score td.sc-lo{{color:#b91c1c}} table.score td.sc-mid{{color:#64748b}} table.score td.sc-na{{color:#cbd5e1}}
table.score .sc-total{{border-left:2px solid #e2e8f0;font-weight:750}}
.scorenote{{color:var(--mut);font-size:11px;margin-top:8px;max-width:900px}}
</style></head><body><div class="wrap">
<div class="hero"><h1><b>FArray</b> — benchmark suite</h1>
<div class="scorebar"><div class="sc-w">{W_} win</div><div class="sc-t">{T_} tie</div><div class="sc-l">{L_} loss</div></div></div>
<div class="sub">grouped bars per benchmark · x = size or swept parameter · bar heights normalised <b>within each size</b> (relative comparison, not absolute throughput) · FArray = bold emerald, competitors muted · band behind each size + card frame tinted by verdict (green win · gray tie · red loss) vs the best competitor · hover a size to compare numbers{(' · '+str(skipped)+' non-ops/s skipped') if skipped else ''}</div>
{score_table}
{sechtml("Primitive")}{sechtml("String")}{sechtml("ListLike")}{sechtml("Diagnostics")}
</div><script>
const C={json.dumps(cdata,separators=(',',':'))};
function nf(x){{if(x>=1e9)return (x/1e9).toFixed(2)+'B';if(x>=1e6)return (x/1e6).toFixed(1)+'M';if(x>=1e3)return (x/1e3).toFixed(1)+'k';if(x>=1)return Math.round(x);return x.toPrecision(2);}}
const VL={{w:'WIN',t:'TIE',l:'LOSS'}};
document.querySelectorAll('.card').forEach(card=>{{
  const svg=card.querySelector('svg'); if(!svg)return; const d=C[svg.id]; if(!d)return;
  const tip=card.querySelector('.tip'),ghi=svg.querySelector('.ghi'),cw=card.querySelector('.cw');
  svg.addEventListener('mousemove',e=>{{
    const r=svg.getBoundingClientRect(), x=(e.clientX-r.left)/r.width*d.w;
    let g=null,bd=1e9; d.groups.forEach(o=>{{const q=Math.abs(o.cx-x); if(q<bd){{bd=q;g=o;}}}});
    if(!g)return;
    ghi.setAttribute('x',g.gx);ghi.setAttribute('y',d.barTop-2);
    ghi.setAttribute('width',g.gw);ghi.setAttribute('height',d.plotH+4);
    ghi.style.fillOpacity=0.05;
    const bars=g.bars.slice().sort((a,b)=>b.v-a.v);
    const fa=bars.find(o=>o.o), comp=bars.filter(o=>!o.o);
    let vd=g.vd, vlh='';
    if(vd) vlh=` <span class="vd ${{vd}}">${{VL[vd]}}</span>`;
    let rows='';
    bars.forEach(o=>{{
      let rt='';
      if(fa&&!o.o){{const k=fa.v/o.v; rt=`<span class="rt ${{k>=1?'up':'dn'}}">${{k>=1?k.toFixed(2)+'×':'÷'+(1/k).toFixed(2)}}</span>`;}}
      rows+=`<div class="row ${{o.o?'me':''}}"><i style="background:${{o.c}}"></i><span class="nm">${{o.l}}</span><span class="vv">${{nf(o.v)}}</span>${{rt}}</div>`;
    }});
    tip.innerHTML=`<div class="th">size ${{g.x}}${{vlh}}</div>${{rows}}`; tip.style.opacity=1;
    const cr=cw.getBoundingClientRect(); let tx=e.clientX-cr.left+14,ty=e.clientY-cr.top+10;
    if(tx+180>cr.width)tx=e.clientX-cr.left-180; if(tx<0)tx=4;
    if(ty+10>cr.height-40)ty=cr.height-160;
    tip.style.left=tx+'px'; tip.style.top=Math.max(0,ty)+'px';
  }});
  svg.addEventListener('mouseleave',()=>{{tip.style.opacity=0;ghi.style.fillOpacity=0;}});
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

def nf_axis(x):
    """Compact x-axis size label."""
    if x >= 1e6: return f"{x//1000000}M" if x % 1000000 == 0 else f"{x/1e6:.1f}M"
    if x >= 1e3: return f"{x//1000}k" if x % 1000 == 0 else f"{x/1e3:.1f}k"
    return str(x)

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
