import { useMemo, useState } from "react";
import { useStore } from "../data/store";
import { lc, ORDER, ours } from "../data/bench";

interface Props {
  /** real benchmark class name, e.g. "MapIntBenchmark" */
  cls: string;
  /** raw op token of the chart ("" when the whole method name is the variant) */
  op: string;
}

// variant / op split — IDENTICAL to bench.ts's grouping, so the methods shown are exactly the ones
// the chart card plotted (apples-to-apples: every variant measured for this op).
const variantOf = (m: string) => { const i = m.indexOf("_"); return i < 0 ? m : m.slice(0, i); };
const opOf = (m: string) => { const i = m.indexOf("_"); return i < 0 ? "" : m.slice(i + 1); };

// Order methods like the chart legend: our farray variants first (ORDER), then unknown alphabetical.
function orderMethods(methods: string[]): string[] {
  const rank = (m: string) => { const r = ORDER.indexOf(variantOf(m)); return r < 0 ? ORDER.length : r; };
  return [...methods].sort((a, b) => rank(a) - rank(b) || a.localeCompare(b));
}

// The "what exactly was measured" reveal: the verbatim @Benchmark source for every variant on this
// chart. Collapsed by default, styled like <GeneratedCode>, follows the site's light/dark theme.
export default function BenchSource({ cls, op }: Props) {
  const { benchSources, ready } = useStore();
  const [open, setOpen] = useState(false);

  const methods = useMemo(() => {
    const src = benchSources[cls];
    if (!src) return [];
    return orderMethods(Object.keys(src).filter((m) => opOf(m) === op));
  }, [benchSources, cls, op]);

  if (!ready || !methods.length) return null;
  const src = benchSources[cls];

  return (
    <div className={`bsrc${open ? " bsrc--open" : ""}`}>
      <button className="bsrc__toggle" onClick={() => setOpen((o) => !o)} aria-expanded={open}>
        <span className="bsrc__chevron" aria-hidden>{open ? "▾" : "▸"}</span>
        source
        <span className="bsrc__meta">{methods.length} @Benchmark method{methods.length === 1 ? "" : "s"}</span>
      </button>
      {open && (
        <div className="bsrc__body">
          {methods.map((m) => (
            <div key={m} className="bsrc__one">
              <div className={ours(variantOf(m)) ? "bsrc__name bsrc__name--me" : "bsrc__name"}>
                <i style={{ background: lc(variantOf(m))[1] }} />
                {lc(variantOf(m))[0]}
                <code>{cls}.{m}</code>
              </div>
              <div className="snippet__code" dangerouslySetInnerHTML={{ __html: src[m].html }} />
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
