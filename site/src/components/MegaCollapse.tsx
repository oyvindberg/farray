import { useStore } from "../data/store";
import { ratioEdge } from "../data/bench";
import { useColorMode } from "@docusaurus/theme-common";

// The megamorphic cliff, made visible: IArray's throughput RELATIVE TO FArray across sizes, on the
// eight-distinct-maps benchmark. It rides at parity while the boxed intermediates fit in cache,
// then falls off a cliff the moment they don't — one shared Array.map site, eight lambda classes,
// re-boxing on Int that becomes pure memory traffic at scale. Each point is coloured by the site's
// own ratio scale (green at parity, blood red once collapsed), so the line changes colour as it
// falls. FArray is the flat baseline: it has no shared site to poison.
const CLS = "MapMegaIntBenchmark";
const SIZES = [100, 1000, 10000, 100000];
const XLABEL: Record<number, string> = { 100: "100", 1000: "1k", 10000: "10k", 100000: "100k" };

export default function MegaCollapse() {
  const { charts, ready } = useStore();
  const dark = useColorMode().colorMode === "dark";
  if (!ready) return <div className="snippet snippet--loading">loading benchmark data…</div>;
  const chart = charts.find((c) => c.cls === CLS && c.op === "");
  if (!chart) return null;
  const fa = (s: number) => chart.series.farray?.[s];
  const ia = (s: number) => chart.series.iarray?.[s];
  const pts = SIZES
    .map((s) => ({ s, r: fa(s) && ia(s) ? (ia(s) as number) / (fa(s) as number) : null }))
    .filter((p): p is { s: number; r: number } => p.r != null && p.r > 0);
  if (pts.length < 3) return null;

  const W = 660, H = 260, padL = 52, padR = 20, padT = 22, padB = 44;
  const maxR = Math.max(1.12, ...pts.map((p) => p.r));
  const x = (i: number) => padL + (i / (pts.length - 1)) * (W - padL - padR);
  const y = (r: number) => padT + (1 - r / maxR) * (H - padT - padB);
  const baseY = y(1);
  const floorY = H - padB;

  // find the steepest downward segment — the cliff edge — to annotate
  let cliff = 1;
  for (let i = 1; i < pts.length; i++) if (pts[i].r - pts[i - 1].r < pts[cliff].r - pts[cliff - 1].r) cliff = i;

  const areaPath =
    `M ${x(0)} ${floorY} ` + pts.map((p, i) => `L ${x(i)} ${y(p.r)}`).join(" ") + ` L ${x(pts.length - 1)} ${floorY} Z`;

  return (
    <figure className="figure megacollapse">
      <svg viewBox={`0 0 ${W} ${H}`} role="img" aria-label="IArray throughput relative to FArray, collapsing at 10k elements">
        <defs>
          <linearGradient id="mc-area" x1="0" y1="0" x2="0" y2="1">
            <stop offset="0%" stopColor={ratioEdge(0.3, dark)} stopOpacity="0.28" />
            <stop offset="100%" stopColor={ratioEdge(0.3, dark)} stopOpacity="0.02" />
          </linearGradient>
        </defs>

        {/* FArray baseline at 1.0 */}
        <line x1={padL} y1={baseY} x2={W - padR} y2={baseY} className="mc-base" />
        <text x={W - padR} y={baseY - 7} textAnchor="end" className="mc-baselabel">
          FArray = 1.00× (no shared site to poison)
        </text>

        {/* IArray area + line, coloured per segment by where it lands */}
        <path d={areaPath} fill="url(#mc-area)" />
        {pts.slice(1).map((p, i) => (
          <line key={i} x1={x(i)} y1={y(pts[i].r)} x2={x(i + 1)} y2={y(p.r)}
            stroke={ratioEdge(p.r, dark)} strokeWidth={i + 1 === cliff ? 4 : 2.5} strokeLinecap="round" />
        ))}

        {/* points + value labels */}
        {pts.map((p, i) => (
          <g key={p.s}>
            <circle cx={x(i)} cy={y(p.r)} r={i === cliff ? 6 : 4.5} fill={ratioEdge(p.r, dark)} />
            <text x={x(i)} y={y(p.r) + (p.r > maxR * 0.7 ? 20 : -12)} textAnchor="middle" className="mc-val">
              {p.r.toFixed(2)}×
            </text>
            <text x={x(i)} y={floorY + 18} textAnchor="middle" className="mc-x">{XLABEL[p.s] ?? p.s}</text>
          </g>
        ))}

        {/* the cliff annotation */}
        <text x={(x(cliff - 1) + x(cliff)) / 2} y={padT + 6} textAnchor="middle" className="mc-cliff">
          ↓ the cliff: 8 boxed maps stop fitting in cache
        </text>
      </svg>
      <figcaption className="figure__cap">
        IArray's speed <em>relative to FArray</em> on the eight-distinct-maps benchmark, per input
        size (Int). At 1k elements the two tie; at 10k IArray's megamorphic boxed intermediates spill
        the cache and it drops to ~0.3× and stays there. FArray, with no shared call site, holds the
        line. Points are coloured by the site's ratio scale.
      </figcaption>
    </figure>
  );
}
