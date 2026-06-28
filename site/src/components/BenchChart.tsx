import { useMemo, useState } from "react";
import { useStore } from "../data/store";
import { useTheme } from "../theme";
import {
  bandColor, edgeColor, lc, nf, nfAxis, ours, verdictAt, type Chart,
} from "../data/bench";

// geometry — ported from bench_report.py
const W = 340, H = 168, PL = 6, PR = 6, PT = 12, PB = 22;
const BAR_TOP = PT, BAR_BOT = H - PB, PLOT_H = BAR_BOT - BAR_TOP, BAR_MIN = 2;
const VDOT: Record<string, string> = { w: "#16a34a", t: "#cbd5e1", l: "#ef4444", "": "transparent" };
const VLABEL: Record<string, string> = { w: "WIN", t: "TIE", l: "LOSS" };

interface Bar { label: string; color: string; ours: boolean; v: number; x: number; y: number; w: number; h: number; }
interface Group { x: number; gx: number; gw: number; cx: number; vd: string; r: number | null; bars: Bar[]; }

function layout(chart: Chart) {
  const { xs, impls, series } = chart;
  const n = xs.length;
  const plotW = W - PL - PR;
  const gw = plotW / Math.max(1, n);
  const GPAD = Math.min(10, gw * 0.16);
  const inner = gw - 2 * GPAD;
  const nb = Math.max(1, impls.length);
  const bw = inner / nb;
  const groups: Group[] = [];
  const ratios: number[] = [];
  for (let i = 0; i < n; i++) {
    const x = xs[i];
    const gx = PL + i * gw;
    const { vd, r } = verdictAt(series, x);
    if (r != null) ratios.push(r);
    const present = impls
      .filter((v) => series[v]?.[x] != null && series[v][x] > 0)
      .map((v) => [v, series[v][x]] as [string, number]);
    const gmax = Math.max(1, ...present.map(([, s]) => s));
    const bars: Bar[] = present.map(([v, s], bi) => {
      const h = Math.max(BAR_MIN, PLOT_H * (s / gmax));
      return {
        label: lc(v)[0], color: lc(v)[1], ours: ours(v), v: s,
        x: gx + GPAD + bi * bw, y: BAR_BOT - h, w: Math.max(0.8, bw - 0.7), h,
      };
    });
    groups.push({ x, gx, gw, cx: gx + gw / 2, vd, r, bars });
  }
  // card frame color from the most dramatic ratio (win -> max, loss -> min)
  let frame: string | null = null;
  if (ratios.length) {
    const edge = chart.agg === "loss" ? Math.min(...ratios) : chart.agg === "win" ? Math.max(...ratios) : null;
    if (edge != null) frame = edgeColor(edge);
  }
  return { groups, gw, frame };
}

function Card({ chart, title }: { chart: Chart; title?: string }) {
  const { groups, frame } = useMemo(() => layout(chart), [chart]);
  const [hover, setHover] = useState<number | null>(null);
  const dark = useTheme().theme === "dark";

  const legend = chart.impls.filter((v) => chart.xs.some((x) => chart.series[v]?.[x] > 0));
  const g = hover != null ? groups[hover] : null;
  const sortedBars = g ? [...g.bars].sort((a, b) => b.v - a.v) : [];
  const fa = sortedBars.find((b) => b.ours);

  return (
    <div className={`bcard bcard--${chart.agg}`} style={frame ? { borderTopColor: frame } : undefined}>
      <div className="bcard__head">
        <h4>{title ?? chart.title}</h4>
        {chart.w + chart.t + chart.l > 0 && (
          <span className={`bcard__wl bcard__wl--${chart.agg}`}>
            <b className="w">{chart.w}</b>·<b className="t">{chart.t}</b>·<b className="l">{chart.l}</b>
          </span>
        )}
      </div>
      <div className="bcard__legend">
        {legend.map((v) => (
          <span key={v} className={ours(v) ? "lg lg--me" : "lg"}>
            <i style={{ background: lc(v)[1] }} />{lc(v)[0]}
          </span>
        ))}
      </div>
      <div className="bcard__plot">
        <svg viewBox={`0 0 ${W} ${H}`}>
          {groups.map((grp, i) => (
            <g key={i}>
              {grp.vd && (
                <rect x={grp.gx} y={BAR_TOP - 2} width={grp.gw} height={PLOT_H + 4} rx={5} fill={bandColor(grp.r, dark)} />
              )}
            </g>
          ))}
          {groups.map((grp, i) =>
            grp.bars.map((b, k) => (
              <rect key={`${i}-${k}`} x={b.x} y={b.y} width={b.w} height={b.h} rx={1.5}
                fill={b.color} opacity={b.ours ? 1 : 0.78} />
            )),
          )}
          {groups.map((grp, i) => (
            <text key={i} x={grp.cx} y={BAR_BOT + 9} textAnchor="middle" className="ax">{nfAxis(grp.x)}</text>
          ))}
          {groups.map((grp, i) =>
            grp.vd ? <circle key={i} cx={grp.cx} cy={BAR_BOT + 12} r={2.6} fill={VDOT[grp.vd]} /> : null,
          )}
          <line x1={PL} y1={BAR_BOT} x2={W - PR} y2={BAR_BOT} className="base" />
          {hover != null && (
            <rect x={groups[hover].gx} y={BAR_TOP - 2} width={groups[hover].gw} height={PLOT_H + 4} rx={5}
              fill="#1f2430" fillOpacity={0.05} />
          )}
          {groups.map((grp, i) => (
            <rect key={i} x={grp.gx} y={0} width={grp.gw} height={H} fill="transparent"
              onMouseEnter={() => setHover(i)} onMouseLeave={() => setHover((h) => (h === i ? null : h))} />
          ))}
        </svg>
        {g && (
          <div className="bcard__tip" style={{ left: `${(g.cx / W) * 100}%` }}>
            <div className="bcard__tip-h">
              size {nf(g.x)}
              {g.vd && <span className={`vd vd--${g.vd}`}>{VLABEL[g.vd]}</span>}
            </div>
            {sortedBars.map((b, i) => {
              const ratio = fa && !b.ours ? fa.v / b.v : null;
              return (
                <div key={i} className={b.ours ? "row row--me" : "row"}>
                  <i style={{ background: b.color }} />
                  <span className="nm">{b.label}</span>
                  <span className="vv">{nf(b.v)}</span>
                  {ratio != null && (
                    <span className={ratio >= 1 ? "rt rt--up" : "rt rt--dn"}>
                      {ratio >= 1 ? `${ratio.toFixed(2)}×` : `÷${(1 / ratio).toFixed(2)}`}
                    </span>
                  )}
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
}

interface Props {
  /** benchmark class, e.g. "MapFilterFoldStrBenchmark" (suffix ok: matches endsWith) */
  cls: string;
  /** restrict to a single op token within an all-ops benchmark */
  op?: string;
  /** plain descriptive caption rendered under the figure */
  caption?: string;
  /** override the card title (single-match charts only) */
  title?: string;
  /** render just the grid (no <figure>/caption) — for embedding inside a custom layout */
  bare?: boolean;
}

export default function BenchChart({ cls, op, caption, title, bare }: Props) {
  const { charts, ready } = useStore();
  const matches = useMemo(
    () => charts.filter((c) => (c.cls === cls || c.cls.endsWith(cls)) && (op == null || c.op === op)),
    [charts, cls, op],
  );
  if (!ready) return <div className="bench-grid bench-grid--loading">measuring…</div>;
  if (!matches.length) return <div className="snippet snippet--error">no benchmark matched <code>{cls}{op ? ` · ${op}` : ""}</code></div>;

  const solo = matches.length === 1;
  const grid = (
    <div className={`bench-grid${bare ? " bench-grid--bare" : solo ? " bench-grid--solo" : ""}`}>
      {matches.map((c) => <Card key={c.key} chart={c} title={solo ? title : undefined} />)}
    </div>
  );
  if (bare) return grid;
  return (
    <figure className="figure">
      {grid}
      {caption && <figcaption className="figure__cap">{caption}</figcaption>}
    </figure>
  );
}
