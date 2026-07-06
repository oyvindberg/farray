import { useMemo } from "react";
import { useStore } from "../data/store";
import { Card } from "./BenchChart";
import Scorecard from "./Scorecard";
import { SECTIONS, ours, type Section, type Chart } from "../data/bench";

type SectionInfo = Partial<Record<Section, { title: string; blurb: string }>>;

const FARRAY_SECTIONS: SectionInfo = {
  Primitive: {
    title: "Primitive elements",
    blurb: "Int and Long payloads, where unboxing is the whole game and the gaps open widest.",
  },
  String: {
    title: "Reference elements",
    blurb:
      "String payloads. Nothing to unbox, so these isolate structure and dispatch alone.",
  },
  ListLike: {
    title: "Cons-list workloads",
    blurb:
      "FArray driven like a List (built with ::, torn down by head/tail recursion) against the structure built for exactly that.",
  },
  Diagnostics: {
    title: "Diagnostics & internals",
    blurb: "Decompositions and micro-probes, including FArray-vs-itself variants that aren't a competing collection.",
  },
};

const FSET_SECTIONS: SectionInfo = {
  Primitive: {
    title: "Int elements",
    blurb:
      "Unboxed Int payloads: the dense-bitmap and frozen-hash territory, against fastutil, HPPC, " +
      "Eclipse, Roaring, both BitSets and both Scala sets.",
  },
  String: {
    title: "String elements",
    blurb:
      "Reference payloads. Nothing to unbox, so these isolate the F14 table, the cached-hash merge " +
      "algebra and the lazy nodes, against every JVM hash set that matters.",
  },
};

// The full-suite chart index (used by the two benchmark reference pages): leaderboard on top,
// then every chart of the suite grouped by section.
export default function BenchIndex({ suite = "farray" }: { suite?: "farray" | "fset" }) {
  const { charts: faCharts, setCharts, ready } = useStore();
  const charts = suite === "fset" ? setCharts : faCharts;
  const info = suite === "fset" ? FSET_SECTIONS : FARRAY_SECTIONS;
  const grouped = useMemo(() => {
    const g = new Map<Section, Chart[]>();
    for (const c of charts) {
      // self-races (every series is a farray/fset variant) aren't a competition; docs pages may
      // still embed them directly, they just don't belong in the reference index.
      if (!Object.keys(c.series).some((v) => !ours(v))) continue;
      const arr = g.get(c.section) ?? [];
      arr.push(c);
      g.set(c.section, arr);
    }
    return g;
  }, [charts]);

  if (!ready) return <p className="ref-loading">measuring…</p>;

  return (
    <>
      <section className="ref-section">
        <h2>Leaderboard</h2>
        <p className="ref-blurb">
          One number per structure and section: the geometric mean of how far each sits behind the
          fastest-in-cell. 1.00 means fastest across the board; higher is slower.
        </p>
        <Scorecard suite={suite} />
      </section>

      {SECTIONS.filter((s) => grouped.has(s) && info[s]).map((s) => (
        <section key={s} className="ref-section">
          <h2>
            {info[s]!.title} <span className="ref-count">{grouped.get(s)!.length} charts</span>
          </h2>
          <p className="ref-blurb">{info[s]!.blurb}</p>
          <div className="bench-grid ref-grid">
            {grouped.get(s)!.map((c) => <Card key={c.key} chart={c} />)}
          </div>
        </section>
      ))}
    </>
  );
}
