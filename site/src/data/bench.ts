// The grouping + verdict + scorecard math (originally ported from the retired scripts/bench_report.py).
// Input is the slimmed bench.json: {b: benchmark, p: params, s: score}[].

export type Slim = { b: string; p: Record<string, string>; s: number };

export type Section = "Primitive" | "String" | "ListLike" | "Diagnostics";
export const SECTIONS: Section[] = ["Primitive", "String", "ListLike", "Diagnostics"];

// variant -> [label, color]. FArray/FSet is the only saturated series (emerald); every competitor is
// desaturated so the eye reads "is green tallest?" at a glance and the chart stays calm.
export const KNOWN: Record<string, [string, string]> = {
  farray: ["FArray", "#16a34a"],
  farrayFused: ["FArray fused", "#4ade80"],
  farraySource: ["FArray fused", "#16a34a"], farrayLines: ["FArray fused", "#16a34a"],
  array: ["Array", "#a0a6ac"],
  arraybuffer: ["ArrayBuffer", "#8aa2b8"], arraybuilder: ["ArrayBuilder", "#c29a63"],
  iarray: ["IArray", "#bf9d57"], list: ["List", "#9b8fb2"], listView: ["List.view", "#c3b7d6"],
  javastream: ["java.stream", "#5382a1"],
  farrayFuse: ["FArray .fuse", "#16a34a"], farrayFuseFold: ["FArray .fuse (fold)", "#6ee7a8"],
  jsoniterNarrow: ["jsoniter (narrow)", "#c58a4e"], jsoniterFull: ["jsoniter (full)", "#d8b184"],
  jsoniterManual: ["jsoniter (manual)", "#a9743d"], jawn: ["jawn", "#9b8fb2"], jackson: ["Jackson", "#7ea2bd"],
  vector: ["Vector", "#7ea2bd"], vectorView: ["Vector.view", "#a6c3d9"], fs2chunk: ["fs2.Chunk", "#c5876b"],
  ziochunk: ["zio.Chunk", "#bd8aa6"], kyochunk: ["kyo.Chunk", "#8a7fc0"],
  scalaRange: ["Range", "#8b9197"],
  farrayTree: ["FArray·tree", "#16a34a"], farrayMat: ["FArray·flat", "#6ee7a8"],
  ziochunkTree: ["zio·tree", "#bd8aa6"], ziochunkMat: ["zio·flat", "#d6b3c6"],
  kyochunkTree: ["kyo·tree", "#8a7fc0"], kyochunkMat: ["kyo·flat", "#b3aade"],
  // ---- the FSet suite (subject = fset; competitors muted; same palette as scripts/setbench_report.py) ----
  fset: ["FSet", "#16a34a"],
  scalaset: ["scala.immut", "#8b5cf6"], scalamut: ["scala.mut", "#a78bfa"],
  immbitset: ["immut.BitSet", "#0ea5e9"], jubitset: ["java.BitSet", "#38bdf8"],
  fastutil: ["fastutil", "#f59e0b"], hppc: ["HPPC", "#f97316"],
  eclipse: ["Eclipse", "#ec4899"], eclipsemut: ["Eclipse.mut", "#ec4899"], eclipseimm: ["Eclipse.immut", "#f9a8d4"],
  roaring: ["Roaring", "#ef4444"], guava: ["Guava", "#14b8a6"],
  juhashset: ["java.HashSet", "#64748b"], jusetof: ["java.Set.of", "#94a3b8"],
};
export const ORDER = Object.keys(KNOWN);
const XPRIORITY = ["size", "numChunks", "chunkCount", "numLeaves", "n", "innerSize", "chunkSize", "leafSize"];
// diagnostic / non-structure variants excluded from the leaderboard (decompositions, and the
// fused/eager pipeline probes which are FArray-vs-itself, not a competing collection)
const SUBV = new Set([
  "farrayTree", "farrayMat", "ziochunkTree", "ziochunkMat", "kyochunkTree", "kyochunkMat", "farrayFused",
  // mutable builders and generators, not immutable-collection competitors — excluded from the
  // leaderboard summary (they still appear as bars on their individual benchmark charts).
  "arraybuffer", "arraybuilder", "scalaRange",
  // java.util.stream is a lazy FUSED pipeline, not an eager immutable collection — the honest
  // comparison is against .fuse, so it stays out of the eager-collection leaderboard and is shown
  // only on the fusion-context charts (elsewhere hidden per-chart via the `ignore` prop).
  "javastream",
  // List.view / Vector.view are the standard library's own lazy single-pass forms — same status as
  // java.util.stream: compared against .fuse on the fusion pages, kept out of the eager leaderboard.
  "listView", "vectorView",
]);
// the actual contending structures — the leaderboard ranks only these (a benchmark method like
// MapMega's `committed` is a scenario, not a collection, and must not show up as a "structure").
const STRUCTS = new Set(ORDER.filter((v) => !SUBV.has(v)));

export const lc = (v: string): [string, string] => KNOWN[v] ?? [v, "#cbd5e1"];
export const ours = (v: string): boolean => v.startsWith("farray") || v.startsWith("fset");

// The FSet suite names classes by element-kind PREFIX (IntSetUnionBenchmark / StrSetUnionBenchmark);
// the FArray suite by SUFFIX (MapIntBenchmark / MapStrBenchmark). Detect the prefix form first.
const SET_PREFIX = /^(Int|Long|Str)Set/;

function section(cls: string): Section {
  // benchmark classes are named by element-kind SUFFIX, e.g. MapIntBenchmark / MapStrBenchmark
  // (not IntMap…), so classify on the kind before the Benchmark/Bench tail. (FArray's own set ops —
  // SetOpsInt/… — are ordinary element ops and stay Primitive/String.) The FSet suite instead
  // prefixes: IntSet…/StrSet… — those classify by the prefix.
  const pm = cls.match(SET_PREFIX);
  if (pm) return pm[1] === "Str" ? "String" : "Primitive";
  if (cls.startsWith("ListLike")) return "ListLike";
  const base = cls.replace(/Benchmark$|Bench$/, "");
  if (/(Int|Long)$/.test(base)) return "Primitive";
  if (/Str$/.test(base)) return "String";
  return "Diagnostics";
}

export type Series = Record<string, Record<number, number>>; // variant -> xval -> score
export type Verdict = "w" | "t" | "l" | "";

export interface Chart {
  key: string;
  section: Section;
  cls: string;
  op: string; // raw op token ("" for whole-method-is-variant benchmarks)
  title: string;
  series: Series;
  xs: number[];
  impls: string[];
  w: number; t: number; l: number;
  agg: "win" | "loss" | "mix";
}

function niceTitle(cls: string, op: string): string {
  // drop Benchmark/Bench tail and the element-kind prefix/suffix — the kind moves to a colored chip.
  const nice = SET_PREFIX.test(cls)
    ? cls.replace(SET_PREFIX, "").replace(/Benchmark$|Bench$/, "")
    : cls.replace(/Benchmark$|Bench$/, "").replace(/(Int|Long|Str)$/, "");
  return nice + (op ? ` · ${op}` : "");
}

// element kind of a benchmark class, for the per-chart Int/reference color chip.
export function kindOf(cls: string): "int" | "ref" | null {
  const pm = cls.match(SET_PREFIX);
  if (pm) return pm[1] === "Str" ? "ref" : "int";
  const base = cls.replace(/Benchmark$|Bench$/, "");
  if (/(Int|Long)$/.test(base)) return "int";
  if (/Str$/.test(base)) return "ref";
  return null;
}

export function buildCharts(data: Slim[]): Chart[] {
  const charts = new Map<string, { section: Section; cls: string; opLabel: string; op: string; series: Series }>();
  for (const e of data) {
    const parts = e.b.split(".");
    const cls = parts[parts.length - 2];
    const meth = parts[parts.length - 1];
    const us = meth.indexOf("_");
    const variant = us < 0 ? meth : meth.slice(0, us);
    const op = us < 0 ? "" : meth.slice(us + 1);
    const params = e.p ?? {};
    const pkeys = Object.keys(params);
    const xkey = XPRIORITY.find((k) => k in params) ?? (pkeys.length ? pkeys.slice().sort()[0] : null);

    // size baked into the method name (e.g. create02) -> one swept chart on that op label
    const m = pkeys.length === 0 ? op.match(/^([A-Za-z]+)(\d+)$/) : null;
    let opLabel: string, xval: number;
    if (m) {
      opLabel = m[1];
      xval = parseInt(m[2], 10);
    } else {
      const extra = pkeys.filter((k) => k !== xkey).sort().map((k) => `${k}=${params[k]}`).join(" · ");
      opLabel = op + (extra ? `  [${extra}]` : "");
      const xv = xkey ? params[xkey] : undefined;
      xval = xv != null && /^-?\d+$/.test(String(xv)) ? parseInt(String(xv), 10) : 0;
    }

    const ck = `${section(cls)} ${cls} ${opLabel}`;
    let c = charts.get(ck);
    if (!c) { c = { section: section(cls), cls, opLabel, op, series: {} }; charts.set(ck, c); }
    (c.series[variant] ??= {})[xval] = e.s;
  }

  const out: Chart[] = [];
  for (const c of charts.values()) {
    const { series } = c;
    const xs = [...new Set(Object.values(series).flatMap((d) => Object.keys(d).map(Number)))].sort((a, b) => a - b);
    const impls = [...ORDER.filter((v) => v in series), ...Object.keys(series).filter((v) => !ORDER.includes(v))];
    let w = 0, t = 0, l = 0;
    for (const x of xs) {
      const v = verdictAt(series, x).vd;
      if (v === "w") w++; else if (v === "t") t++; else if (v === "l") l++;
    }
    const agg = w + t + l === 0 ? "mix" : w > l ? "win" : l > w ? "loss" : "mix";
    out.push({ key: c.section + "/" + c.cls + "/" + c.opLabel, section: c.section, cls: c.cls, op: c.op,
      title: niceTitle(c.cls, c.op), series, xs, impls, w, t, l, agg });
  }
  out.sort((a, b) => a.key.localeCompare(b.key));
  return out;
}

export function verdictAt(series: Series, x: number): { vd: Verdict; r: number | null } {
  const o: number[] = [], comp: number[] = [];
  for (const v of Object.keys(series)) {
    const s = series[v][x];
    if (s == null) continue;
    (ours(v) ? o : comp).push(s);
  }
  if (!o.length || !comp.length) return { vd: "", r: null };
  const r = Math.max(...o) / Math.max(...comp);
  return { vd: r >= 1.05 ? "w" : r >= 0.95 ? "t" : "l", r };
}

/** A view of a chart with some competitors dropped — for reusing one benchmark in different
  * contexts (e.g. showing java.stream only where a fused comparison is on the page, hiding it on
  * the eager charts). W/T/L and every per-cell ratio are RECOMPUTED against the remaining rivals,
  * so a hidden competitor that happened to be fastest doesn't leave stale verdicts behind. */
export function filterChart(chart: Chart, ignore?: string[]): Chart {
  if (!ignore || !ignore.length) return chart;
  const drop = new Set(ignore);
  if (!chart.impls.some((v) => drop.has(v))) return chart;
  const series: Series = {};
  for (const v of Object.keys(chart.series)) if (!drop.has(v)) series[v] = chart.series[v];
  let w = 0, t = 0, l = 0;
  for (const x of chart.xs) {
    const v = verdictAt(series, x).vd;
    if (v === "w") w++; else if (v === "t") t++; else if (v === "l") l++;
  }
  const agg: Chart["agg"] = w + t + l === 0 ? "mix" : w > l ? "win" : l > w ? "loss" : "mix";
  return { ...chart, series, impls: chart.impls.filter((v) => !drop.has(v)), w, t, l, agg };
}

// ---- the ratio color scale: one color language for the whole site ----
// r = ours / best-competitor. r >= 1 means we ARE the fastest (by margin r); r < 1 means we sit
// 1/r behind the winner. The scale is continuous and log-spaced so it reads pedagogically:
//   winner            -> green (deepening slightly with margin)
//   1.1x behind       -> green-yellow
//   1.2x              -> amber
//   1.3x              -> red (the boundary: past 1.3x behind is a real loss)
//   2x                -> deepening red
//   3x or worse       -> blood red
// Discrete W/T/L verdicts stay as chips/dots; color always answers "how far from the winner?".
const HUE_STOPS: [number, number][] = [[1.0, 100], [1.1, 68], [1.2, 40], [1.3, 15], [2.0, 8], [3.0, 0]];
function behindHue(b: number): number {
  if (b <= HUE_STOPS[0][0]) return HUE_STOPS[0][1];
  for (let i = 1; i < HUE_STOPS.length; i++) {
    const [b1, h1] = HUE_STOPS[i - 1], [b2, h2] = HUE_STOPS[i];
    if (b <= b2) {
      const t = (Math.log(b) - Math.log(b1)) / (Math.log(b2) - Math.log(b1));
      return h1 + (h2 - h1) * t;
    }
  }
  return 0;
}
const clamp1 = (x: number) => Math.min(Math.max(x, 0), 1);
/** translucent fill: hover bands, scorecard cells, chips. */
export function ratioBand(r: number | null, dark = false): string {
  if (r == null) return "transparent";
  if (r >= 1) { // winner: green tint, a touch deeper with margin
    const f = clamp1(Math.log10(r) / Math.log10(3));
    return dark ? `hsla(150, 60%, 45%, ${(0.10 + 0.14 * f).toFixed(3)})`
                : `hsla(150, 55%, 42%, ${(0.10 + 0.13 * f).toFixed(3)})`;
  }
  const b = Math.min(1 / r, 6);
  const h = behindHue(b);
  const t = clamp1(Math.log(b) / Math.log(3)); // 0 at the winner's doorstep, 1 (blood) at 3x behind
  const a = 0.12 + (dark ? 0.42 : 0.38) * t;
  return `hsla(${h.toFixed(0)}, 78%, ${dark ? 50 : 44}%, ${a.toFixed(3)})`;
}
/** full-strength stroke: card frames, accents. */
export function ratioEdge(r: number | null, dark = false): string {
  if (r == null) return "transparent";
  if (r >= 1) return dark ? "hsl(150, 55%, 48%)" : "hsl(150, 55%, 38%)";
  const b = Math.min(1 / r, 6);
  const h = behindHue(b);
  return dark ? `hsl(${h.toFixed(0)}, 70%, 52%)` : `hsl(${h.toFixed(0)}, 72%, 42%)`;
}
// transitional aliases (old names) so nothing else breaks while callers migrate
export function bandColor(r: number | null, dark = false): string { return ratioBand(r, dark); }
export function edgeColor(r: number | null): string { return ratioEdge(r); }

export interface Scorecard { cols: string[]; rows: { v: string; label: string; color: string; ours: boolean; vals: (number | null)[] }[]; }
export function buildScorecard(charts: Chart[]): Scorecard {
  const sum = new Map<string, [number, number]>(); // `${v} ${col}` -> [sumLn, count]
  for (const ch of charts) {
    if (ch.section === "Diagnostics") continue; // probes/pipeline shapes, not structure-vs-structure races
    for (const x of ch.xs) {
      const present: [string, number][] = [];
      for (const v of Object.keys(ch.series)) {
        if (!STRUCTS.has(v)) continue;
        const s = ch.series[v][x];
        if (s != null && s > 0) present.push([v, s]);
      }
      if (present.length < 2) continue;
      const best = Math.max(...present.map(([, s]) => s));
      for (const [v, s] of present) {
        const lr = Math.log(best / s);
        for (const col of [ch.section, "TOTAL"]) {
          const k = `${v} ${col}`;
          const a = sum.get(k) ?? [0, 0];
          a[0] += lr; a[1] += 1; sum.set(k, a);
        }
      }
    }
  }
  const get = (v: string, col: string): number | null => {
    const a = sum.get(`${v} ${col}`);
    return a && a[1] ? Math.exp(a[0] / a[1]) : null;
  };
  const seen = new Set<string>();
  for (const k of sum.keys()) seen.add(k.split(" ")[0]);
  const vars = [...ORDER.filter((v) => !SUBV.has(v) && seen.has(v)),
    ...[...seen].filter((v) => !SUBV.has(v) && !ORDER.includes(v))];
  vars.sort((a, b) => (get(a, "TOTAL") ?? 1e9) - (get(b, "TOTAL") ?? 1e9));
  // only show section columns that actually have data (Diagnostics is empty once everything is
  // classified by its real element kind), plus TOTAL.
  const cols = [...SECTIONS.filter((sec) => [...sum.keys()].some((k) => k.endsWith(" " + sec))), "TOTAL"];
  return {
    cols,
    rows: vars.map((v) => ({ v, label: lc(v)[0], color: lc(v)[1], ours: ours(v), vals: cols.map((c) => get(v, c)) })),
  };
}

// compact number formatting (mirrors python nf / nf_axis)
export function nf(x: number): string {
  if (x >= 1e9) return (x / 1e9).toFixed(2) + "B";
  if (x >= 1e6) return (x / 1e6).toFixed(1) + "M";
  if (x >= 1e3) return (x / 1e3).toFixed(1) + "k";
  if (x >= 1) return String(Math.round(x));
  return x.toPrecision(2);
}
export function nfAxis(x: number): string {
  if (x >= 1e6) return x % 1e6 === 0 ? `${x / 1e6}M` : `${(x / 1e6).toFixed(1)}M`;
  if (x >= 1e3) return x % 1e3 === 0 ? `${x / 1e3}k` : `${(x / 1e3).toFixed(1)}k`;
  return String(x);
}
