import { useMemo } from "react";
import { useStore } from "../data/store";
import { Card } from "../components/BenchChart";
import Scorecard from "../components/Scorecard";
import { SECTIONS, type Section, type Chart } from "../data/bench";

const SECTION_INFO: Partial<Record<Section, { title: string; blurb: string }>> = {
  Primitive: {
    title: "Int elements",
    blurb:
      "Unboxed Int payloads — the dense-bitmap and frozen-hash territory, against fastutil, HPPC, " +
      "Eclipse, Roaring, both BitSets and both Scala sets.",
  },
  String: {
    title: "String elements",
    blurb:
      "Reference payloads. Nothing to unbox, so these isolate the F14 table, the cached-hash merge " +
      "algebra and the lazy nodes — against every JVM hash set that matters.",
  },
};

export default function SetReferencePage() {
  const { setCharts, ready } = useStore();
  const grouped = useMemo(() => {
    const g = new Map<Section, Chart[]>();
    for (const c of setCharts) {
      const arr = g.get(c.section) ?? [];
      arr.push(c);
      g.set(c.section, arr);
    }
    return g;
  }, [setCharts]);

  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">the complete scorecard</p>
        <h1>FSet benchmarks</h1>
        <p className="page__lede">
          The whole FSet JMH suite, unabridged — every operation at every size, FSet against every set on
          the JVM worth racing: Scala's immutable and mutable sets, <code>java.util.HashSet</code>, both
          BitSets, fastutil, HPPC, Eclipse Collections, Guava and RoaringBitmap. Wins and losses alike;
          the <a href="#/fset">FSet page</a> pulls out the highlights and explains the machinery. Every bar
          is a measurement you can hover, and every card reveals the exact <code>@Benchmark</code> source it
          came from.
        </p>
      </header>

      {!ready ? (
        <p className="ref-loading">measuring…</p>
      ) : (
        <>
          <section className="ref-section">
            <h2>Leaderboard</h2>
            <p className="ref-blurb">
              One number per structure and section: the geometric mean of how far each sits behind the
              fastest-in-cell. 1.00 means fastest across the board; higher is slower.
            </p>
            <Scorecard suite="fset" />
          </section>

          {SECTIONS.filter((s) => grouped.has(s) && SECTION_INFO[s]).map((s) => (
            <section key={s} className="ref-section">
              <h2>
                {SECTION_INFO[s]!.title} <span className="ref-count">{grouped.get(s)!.length} charts</span>
              </h2>
              <p className="ref-blurb">{SECTION_INFO[s]!.blurb}</p>
              <div className="bench-grid ref-grid">
                {grouped.get(s)!.map((c) => <Card key={c.key} chart={c} />)}
              </div>
            </section>
          ))}
        </>
      )}
    </article>
  );
}
