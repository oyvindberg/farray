import { useStore } from "../data/store";
import { lc, nf } from "../data/bench";

// Three speeds, one chart: the intro's 14-stage pipeline at one size, every collection as a
// horizontal bar, grouped under a tier caption that names the MECHANISM — so the chart explains
// itself without the prose having to enumerate bars. Width is log-scaled (the span is ~80x; linear
// would flatten the bottom tier to nothing — true, but unreadable). Labels carry throughput and the
// multiple over List.
const CLS = "LongMixedPipelineIntBenchmark";
const SIZE = 100000;

type Rung = { v: string | ["ziochunk", "fs2chunk"]; note: string };
type Tier = { caption: string; rungs: Rung[] };

// top tier first — the chart reads best-down like a leaderboard.
const TIERS: Tier[] = [
  {
    caption: "one compiled loop, no intermediates",
    rungs: [{ v: "farrayFused", note: "FArray + .fuse: the whole chain rewritten at compile time" }],
  },
  {
    caption: "unboxed elements, O(1) structure, same immutable code",
    rungs: [{ v: "farrayEager", note: "FArray, eager: a type swap away from the code above" }],
  },
  {
    caption: "raw arrays: no boxes, but a full copy per stage",
    rungs: [{ v: "iarray", note: "IArray: 13 intermediate copies for 14 stages" }],
  },
  {
    caption: "boxed at every stage boundary, plus the copies",
    rungs: [
      { v: ["ziochunk", "fs2chunk"], note: "the better Chunk of the two, per measurement" },
      { v: "vector", note: "Vector" },
      { v: "list", note: "List" },
    ],
  },
];

export default function Ladder() {
  const { charts, ready } = useStore();
  if (!ready) return <div className="snippet snippet--loading">loading benchmark data…</div>;
  const chart = charts.find((c) => c.cls === CLS && c.op === "");
  if (!chart) return null;
  const at = (v: string): number | null => chart.series[v]?.[SIZE] ?? null;

  const resolve = (r: Rung): { v: string; score: number; note: string } | null => {
    if (Array.isArray(r.v)) {
      const [a, b] = [at(r.v[0]), at(r.v[1])];
      if (a == null && b == null) return null;
      return { v: (b ?? 0) > (a ?? 0) ? r.v[1] : r.v[0], score: Math.max(a ?? 0, b ?? 0), note: r.note };
    }
    const s = at(r.v);
    return s == null || s <= 0 ? null : { v: r.v, score: s, note: r.note };
  };

  const tiers = TIERS.map((t) => ({ caption: t.caption, rows: t.rungs.map(resolve).filter((x) => x != null) }))
    .filter((t) => t.rows.length > 0);
  const all = tiers.flatMap((t) => t.rows);
  if (all.length < 3) return null;

  const base = Math.min(...all.map((r) => r.score)); // List — the bottom bar
  const hi = Math.max(...all.map((r) => r.score));
  const width = (s: number) => 10 + 90 * (Math.log10(s / base) / Math.max(1e-9, Math.log10(hi / base)));

  return (
    <figure className="figure ladder">
      <div className="ladder__rows">
        {tiers.map((t) => (
          <div className="ladder__tier" key={t.caption}>
            <div className="ladder__tiercap">{t.caption}</div>
            {t.rows.map((r) => {
              const [label, color] = lc(r.v);
              const mult = r.score / base;
              return (
                <div className="ladder__row" key={r.v} title={r.note}>
                  <span className="ladder__label">{label}</span>
                  <span className="ladder__track">
                    <span className="ladder__bar" style={{ width: `${width(r.score).toFixed(1)}%`, background: color }} />
                    <span className="ladder__val">
                      {nf(r.score)} ops/s{mult >= 1.05 ? ` · ${mult >= 10 ? mult.toFixed(0) : mult.toFixed(1)}×` : ""}
                    </span>
                  </span>
                </div>
              );
            })}
          </div>
        ))}
      </div>
      <figcaption className="figure__cap">
        The 14-stage pipeline above, at 100k elements. Same immutable code on every bar; the captions
        name what changes underneath it. Multiples are over List; widths are log-scaled.
      </figcaption>
    </figure>
  );
}
