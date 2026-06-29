export default function JsonPage() {
  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">the same optimizer, over bytes</p>
        <h1>Fused JSON</h1>
        <p className="page__lede">
          Now turn the optimizer loose on JSON. The insight that makes it work: a JSON object is a{" "}
          <strong>product</strong> whose fields are <strong>columns</strong>, sourced from byte ranges. So the
          same optimizer — unchanged — becomes a projection-aware JSON parser. You wrap a byte buffer as a
          source and start a pipeline with <code>.stream</code>; the macro reads your{" "}
          <code>.filter(…).map(…)</code> chain, works out which fields it actually touches, and emits one
          specialized per-record scanner that reads straight off the <code>byte[]</code>: dead fields are
          skipped at the byte level, a projected string is decoded only for survivors, a record whose filter
          fails is abandoned mid-scan, and no per-record object is ever allocated. It beats the JVM's best
          hand-tuned parser on a projection query, and laps the AST parsers.
        </p>
      </header>

      <section className="ex">
        <h2>The benchmark</h2>
        <p>
          The dataset is NDJSON — one JSON object per line — of <strong>10 000 records</strong>,{" "}
          <strong>~3.2&nbsp;MB</strong>, each a <strong>20-field</strong> object (~334 bytes: a realistic mix of
          longs, ints, doubles, and strings). Every query touches just <strong>1–2 of those 20 fields</strong> —
          the projection case where pushdown pays.
        </p>
        <p className="page__note">
          The worked examples — projection pushdown, lazy decode, discard-DCE, predicate-fail early-out, and
          the one-pass multi-aggregate — are being ported here next, each with its regenerated golden and a
          live, hover-able benchmark chart (fuse vs jsoniter-scala, Jackson, jawn). Coming shortly.
        </p>
      </section>

      <footer className="page__foot">
        <a className="page__next" href="#/fusion">← Back to the optimizer</a>
      </footer>
    </article>
  );
}
