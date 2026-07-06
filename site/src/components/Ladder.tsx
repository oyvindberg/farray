import { useStore } from "../data/store";
import { lc, nf } from "../data/bench";

// The ladder: ONE pipeline (the intro's 14-stage LongMixedPipeline), one size, every rung as a
// horizontal bar. Same immutable code top to bottom — the rungs differ only in what the collection
// does with it. Width is log-scaled (the span is ~80x; linear would flatten the bottom rungs to
// nothing, which is true but unreadable). Each bar is labeled with throughput and its multiple over
// List, the bottom rung.
const CLS = "LongMixedPipelineIntBenchmark";
const SIZE = 100000;
// bottom -> top; ziochunk/fs2chunk collapse to whichever measured best ("best Chunk").
const RUNGS: { v: string | ["ziochunk", "fs2chunk"]; note: string }[] = [
  { v: "list", note: "boxed cells, one collection allocated per stage" },
  { v: "vector", note: "boxed, tree-backed, same per-stage materialization" },
  { v: ["ziochunk", "fs2chunk"], note: "array-backed chunks — still boxed Ints, still 13 intermediates" },
  { v: "iarray", note: "raw arrays, eagerly copied per stage — the mutable-adjacent baseline" },
  { v: "farrayEager", note: "FArray, same code: unboxed storage, inline loops, O(1) structure" },
  { v: "farrayFused", note: "FArray + .fuse: the whole chain compiled to one loop" },
];

export default function Ladder() {
  const { charts, ready } = useStore();
  if (!ready) return <div className="snippet snippet--loading">loading benchmark data…</div>;
  const chart = charts.find((c) => c.cls === CLS && c.op === "");
  if (!chart) return null;
  const at = (v: string): number | null => chart.series[v]?.[SIZE] ?? null;

  const rows = RUNGS.flatMap((r) => {
    let v: string, score: number | null;
    if (Array.isArray(r.v)) {
      const [a, b] = [at(r.v[0]), at(r.v[1])];
      if (a == null && b == null) return [];
      v = (b ?? 0) > (a ?? 0) ? r.v[1] : r.v[0];
      score = Math.max(a ?? 0, b ?? 0);
    } else {
      v = r.v;
      score = at(v);
    }
    return score == null || score <= 0 ? [] : [{ v, score, note: r.note }];
  });
  if (rows.length < 3) return null;

  const base = rows[0].score;
  const lo = Math.min(...rows.map((r) => r.score));
  const hi = Math.max(...rows.map((r) => r.score));
  const width = (s: number) => {
    const f = Math.log10(s / lo) / Math.max(1e-9, Math.log10(hi / lo));
    return 10 + 90 * f; // percent; floor keeps the bottom rung visible
  };

  return (
    <figure className="figure ladder">
      <div className="ladder__rows">
        {[...rows].reverse().map((r) => {
          const [label, color] = lc(r.v);
          const mult = r.score / base;
          return (
            <div className="ladder__row" key={r.v} title={r.note}>
              <span className="ladder__label">{label}</span>
              <span className="ladder__track">
                <span className="ladder__bar" style={{ width: `${width(r.score).toFixed(1)}%`, background: color }} />
                <span className="ladder__val">
                  {nf(r.score)} ops/s{mult >= 1.05 ? ` · ${mult >= 10 ? mult.toFixed(0) : mult.toFixed(1)}× List` : ""}
                </span>
              </span>
            </div>
          );
        })}
      </div>
      <figcaption className="figure__cap">
        The intro's 14-stage pipeline at 100k elements — identical immutable code on every rung, log-scaled
        widths. Hover a rung for what it's doing to your CPU.
      </figcaption>
    </figure>
  );
}
