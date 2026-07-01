import type { ReactNode } from "react";
import { useStore } from "../data/store";
import { Card } from "./BenchChart";

interface Props {
  /** the Int benchmark class, e.g. "MapIntBenchmark" */
  int: string;
  /** the String/reference equivalent, e.g. "MapStrBenchmark" */
  str: string;
  caption?: ReactNode;
  /** which benchmark suite to look the classes up in (default: the FArray suite) */
  suite?: "farray" | "fset";
}

// Int (left) next to its String/reference equivalent (right), matched op-by-op, so the
// primitive-vs-reference picture sits side by side and stays honest.
export default function BenchPair({ int: intCls, str: strCls, caption, suite = "farray" }: Props) {
  const { charts: faCharts, setCharts, ready } = useStore();
  const charts = suite === "fset" ? setCharts : faCharts;
  if (!ready) return <div className="bench-grid bench-grid--loading">measuring…</div>;

  const pick = (cls: string) => charts.filter((c) => c.cls === cls);
  const ic = pick(intCls);
  const sc = pick(strCls);
  if (!ic.length && !sc.length) {
    return <div className="snippet snippet--error">no benchmark matched <code>{intCls}</code> / <code>{strCls}</code></div>;
  }
  const ops = [...new Set([...ic, ...sc].map((c) => c.op))].sort();

  return (
    <figure className="figure">
      {ops.map((op) => {
        const i = ic.find((c) => c.op === op);
        const s = sc.find((c) => c.op === op);
        return (
          <div className="bench-pair" key={op}>
            {i ? <Card chart={i} /> : <div className="bench-pair__gap">no Int variant</div>}
            {s ? <Card chart={s} /> : <div className="bench-pair__gap">no String variant</div>}
          </div>
        );
      })}
      {caption && <figcaption className="figure__cap">{caption}</figcaption>}
    </figure>
  );
}
