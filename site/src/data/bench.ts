// Faithful TypeScript port of scripts/bench_report.py's grouping + verdict + scorecard math.
// Input is the slimmed bench.json: {b: benchmark, p: params, s: score}[].

export type Slim = { b: string; p: Record<string, string>; s: number };

export type Section = "Primitive" | "String" | "ListLike" | "Diagnostics";
export const SECTIONS: Section[] = ["Primitive", "String", "ListLike", "Diagnostics"];

// variant -> [label, color]. FArray is the only saturated series (emerald); every competitor is
// desaturated so the eye reads "is green tallest?" at a glance and the chart stays calm.
export const KNOWN: Record<string, [string, string]> = {
  farray: ["FArray", "#16a34a"],
  farrayFused: ["FArray fused", "#16a34a"], farrayEager: ["FArray eager", "#9bb6a6"],
  array: ["Array", "#a0a6ac"],
  iarray: ["IArray", "#bf9d57"], list: ["List", "#9b8fb2"],
  vector: ["Vector", "#7ea2bd"], fs2chunk: ["fs2.Chunk", "#c5876b"],
  ziochunk: ["zio.Chunk", "#bd8aa6"], scalaRange: ["Range", "#8b9197"],
  farrayTree: ["FArray·tree", "#16a34a"], farrayMat: ["FArray·flat", "#6ee7a8"],
  ziochunkTree: ["zio·tree", "#bd8aa6"], ziochunkMat: ["zio·flat", "#d6b3c6"],
};
export const ORDER = Object.keys(KNOWN);
const XPRIORITY = ["size", "numChunks", "chunkCount", "numLeaves", "n", "innerSize", "chunkSize", "leafSize"];
// diagnostic / non-structure variants excluded from the leaderboard (decompositions, and the
// fused/eager pipeline probes which are FArray-vs-itself, not a competing collection)
const SUBV = new Set([
  "farrayTree", "farrayMat", "ziochunkTree", "ziochunkMat", "farrayFused", "farrayEager",
]);
// the actual contending structures — the leaderboard ranks only these (a benchmark method like
// MapMega's `committed` is a scenario, not a collection, and must not show up as a "structure").
const STRUCTS = new Set(ORDER.filter((v) => !SUBV.has(v)));

export const lc = (v: string): [string, string] => KNOWN[v] ?? [v, "#cbd5e1"];
export const ours = (v: string): boolean => v.startsWith("farray");

function section(cls: string): Section {
  // benchmark classes are named by element-kind SUFFIX, e.g. MapIntBenchmark / MapStrBenchmark
  // (not IntMap…), so classify on the kind before the Benchmark/Bench tail. (FArray's own set ops —
  // SetOpsInt/… — are ordinary element ops and stay Primitive/String; the future FSet collection gets
  // its own page, not a section here.)
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
  // drop Benchmark/Bench tail and the element-kind suffix — the kind moves to a colored chip.
  const nice = cls.replace(/Benchmark$|Bench$/, "").replace(/(Int|Long|Str)$/, "");
  return nice + (op ? ` · ${op}` : "");
}

// element kind of a benchmark class, for the per-chart Int/reference color chip.
export function kindOf(cls: string): "int" | "ref" | null {
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

// ---- band / edge color, ported 1:1 (HSL) ----
const clamp1 = (x: number) => Math.min(x, 1);
export function bandColor(r: number | null, dark = false): string {
  if (r == null) return "transparent";
  if (dark) {
    // translucent fills that read as a tint over a dark card, never as a glowing block
    if (r >= 1.05) { const f = clamp1(Math.log10(r)); return `rgba(52,210,122,${(0.07 + 0.17 * f).toFixed(3)})`; }
    if (r >= 0.95) return "rgba(148,163,184,0.09)";
    const f = clamp1(Math.log10(1 / r)); return `rgba(236,106,94,${(0.07 + 0.18 * f).toFixed(3)})`;
  }
  if (r >= 1.05) { const f = clamp1(Math.log10(r)); return `hsl(148,${(36 + 22 * f).toFixed(0)}%,${(97 - 9 * f).toFixed(0)}%)`; }
  if (r >= 0.95) return "hsl(48,70%,96%)";
  const f = clamp1(Math.log10(1 / r)); const hp = clamp1(f / 0.061);
  return `hsl(${(48 * (1 - hp)).toFixed(0)},${(84 + 13 * f).toFixed(0)}%,${(93 - 15 * hp - 23 * f).toFixed(0)}%)`;
}
export function edgeColor(r: number | null): string {
  if (r == null) return "#cbd5e1";
  if (r >= 1.05) { const f = clamp1(Math.log10(r)); return `hsl(150,68%,${(52 - 12 * f).toFixed(0)}%)`; }
  if (r >= 0.95) return "#fbbf24";
  const f = clamp1(Math.log10(1 / r)); const hp = clamp1(f / 0.061);
  return `hsl(${(45 * (1 - hp)).toFixed(0)},${(82 + 13 * f).toFixed(0)}%,${(66 - 8 * hp - 20 * f).toFixed(0)}%)`;
}

// ---- scorecard: geometric mean of (best-in-cell / own) per structure, per section + TOTAL ----
export interface Scorecard { cols: string[]; rows: { v: string; label: string; color: string; ours: boolean; vals: (number | null)[] }[]; }
export function buildScorecard(charts: Chart[]): Scorecard {
  const sum = new Map<string, [number, number]>(); // `${v} ${col}` -> [sumLn, count]
  for (const ch of charts) {
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
