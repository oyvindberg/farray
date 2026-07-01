import { useMemo } from "react";
import { useStore } from "../data/store";
import { Card } from "../components/BenchChart";
import Scorecard from "../components/Scorecard";
import { SECTIONS, type Section, type Chart } from "../data/bench";

const SECTION_INFO: Record<Section, { title: string; blurb: string }> = {
  Primitive: {
    title: "Primitive elements",
    blurb: "Int and Long payloads — where unboxing is the whole game and the gaps open widest.",
  },
  String: {
    title: "Reference elements",
    blurb: "String payloads. There's no boxing to save, so these isolate structure and dispatch alone.",
  },
  ListLike: {
    title: "Cons-list workloads",
    blurb: "FArray driven like a List — built with ::, torn down by head/tail recursion.",
  },
  Diagnostics: {
    title: "Diagnostics & internals",
    blurb: "Decompositions and micro-probes — including FArray-vs-itself variants that aren't a competing collection.",
  },
};

export default function ReferencePage() {
  const { charts, ready } = useStore();
  const grouped = useMemo(() => {
    const g = new Map<Section, Chart[]>();
    for (const c of charts) {
      const arr = g.get(c.section) ?? [];
      arr.push(c);
      g.set(c.section, arr);
    }
    return g;
  }, [charts]);

  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">the complete scorecard</p>
        <h1>Benchmarks</h1>
        <p className="page__lede">
          The whole JMH suite, unabridged — every operation, every size, FArray against every competitor. The
          chapters up front pull out the highlights and explain the machinery; this page is the reference.
          Every bar is a measurement you can hover, and every card reveals the exact benchmark source it came
          from.
        </p>
      </header>

      {!ready ? (
        <p className="ref-loading">measuring…</p>
      ) : (
        <>
          <section className="ref-section">
            <h2>Leaderboard</h2>
            <p className="ref-blurb">
              Geometric mean of best-in-class ÷ own, per structure and section — one number each. 1.00 is the
              fastest of the field; higher is slower.
            </p>
            <Scorecard />
          </section>

          {SECTIONS.filter((s) => grouped.has(s)).map((s) => (
            <section key={s} className="ref-section">
              <h2>
                {SECTION_INFO[s].title} <span className="ref-count">{grouped.get(s)!.length} charts</span>
              </h2>
              <p className="ref-blurb">{SECTION_INFO[s].blurb}</p>
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
