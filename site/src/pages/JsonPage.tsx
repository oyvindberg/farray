import Snippet from "../components/Snippet";

type Row = { label: string; ops: string; bop: string; win?: boolean };
type Bench = { headline: string; rows: Row[] };

function BenchTable({ bench }: { bench: Bench }) {
  return (
    <div className="btab-wrap">
      <p className="fuse-cap">measured — JMH, <code>-prof gc</code></p>
      <table className="btab">
        <thead>
          <tr><th /><th>throughput</th><th>allocation</th></tr>
        </thead>
        <tbody>
          {bench.rows.map((r) => (
            <tr key={r.label} className={r.win ? "btab__win" : undefined}>
              <td>{r.win && <span className="btab__tro">▲</span>}{r.label}</td>
              <td className="btab__num">{r.ops}</td>
              <td className="btab__num">{r.bop}</td>
            </tr>
          ))}
        </tbody>
      </table>
      <p className="btab__head">{bench.headline}</p>
    </div>
  );
}

function JsonEx({
  src, gen, rival, rivalLabel, bench,
}: { src: string; gen: string; rival?: string; rivalLabel?: string; bench?: Bench }) {
  return (
    <div className="fuseex">
      <p className="fuse-cap">you write</p>
      <Snippet name={src} hideFull />
      <p className="fuse-cap">the macro emits — one per-record scanner</p>
      <Snippet name={gen} />
      {rival && (
        <>
          <p className="fuse-cap fuse-cap--rival">what we beat — {rivalLabel}</p>
          <Snippet name={rival} hideFull />
        </>
      )}
      {bench && <BenchTable bench={bench} />}
    </div>
  );
}

// JMH numbers (GraalVM 25, Apple Silicon) from benchmarks/JsonProjectionBenchmark, -prof gc.
const SUM_BENCH: Bench = {
  headline: "fuse beats the JVM's best hand-tuned parser 1.6×, and laps the AST parsers 4–9× — at ~zero allocation",
  rows: [
    { label: "fuse (generated scanner)", ops: "391 ops/s", bop: "1.0 B/op", win: true },
    { label: "jsoniter — hand-written reader, no object", ops: "241 ops/s", bop: "1.7 B/op" },
    { label: "jsoniter — full codec (parses all 20 fields)", ops: "136 ops/s", bop: "6 464 003 B/op" },
    { label: "Jackson — tree model (JsonNode)", ops: "94 ops/s", bop: "31 047 492 B/op" },
    { label: "jawn — AST (JValue)", ops: "45 ops/s", bop: "45 485 209 B/op" },
  ],
};
const COUNT_BENCH: Bench = {
  headline: "fuse 1.48× faster, and ~zero allocation vs jsoniter's per-record object",
  rows: [
    { label: "fuse (no String decoded at all)", ops: "345 ops/s", bop: "1.2 B/op", win: true },
    { label: "jsoniter — narrow codec", ops: "232 ops/s", bop: "720 002 B/op" },
  ],
};
const WIDE_BENCH: Bench = {
  headline: "fuse 2.5× faster, near-zero allocation vs 1.4 MB/op",
  rows: [
    { label: "fuse (rejected records stop at key)", ops: "1207 ops/s", bop: "0.4 B/op", win: true },
    { label: "jsoniter — narrow codec {key, payload}", ops: "478 ops/s", bop: "1 432 001 B/op" },
  ],
};

export default function JsonPage() {
  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">the same optimizer, over bytes</p>
        <h1>Fused JSON</h1>
        <p className="page__lede">
          Now turn the optimizer loose on JSON. The insight that makes it work: a JSON object is a{" "}
          <strong>product</strong> whose fields are <strong>columns</strong> — sourced from byte ranges
          instead of tuple slots. So the same optimizer, <em>unchanged</em>, becomes a projection-aware JSON
          parser. Wrap a byte buffer as a source, start a pipeline with <code>.stream</code>, and the macro
          reads your <code>.filter(…).map(…)</code> chain, works out which fields it actually touches, and
          emits one specialized per-record scanner over the raw <code>byte[]</code>. Dead fields are skipped
          at the byte level. A projected string is decoded only for survivors. A record whose filter fails is
          abandoned mid-scan. No per-record object is ever allocated. The result beats the JVM's best
          hand-tuned parser on a projection query, and laps the AST parsers.
        </p>
      </header>

      <section className="ex">
        <h2>The benchmark</h2>
        <p>
          The dataset is NDJSON — one JSON object per line — of <strong>10 000 records</strong>,{" "}
          <strong>~3.2&nbsp;MB</strong>, each a <strong>20-field</strong> object (~334 bytes: a realistic mix
          of longs, ints, doubles, and strings). Every query touches just <strong>1–2 of those 20
          fields</strong> — the projection case where pushdown pays. Throughput is ops/s over the whole
          10 000-record buffer; allocation is bytes per buffer-op (÷10 000 ≈ bytes per record). Every parser
          runs the same query: the fuse row is the generated scanner, and each rival row is that library's
          best idiom for "read a couple of fields per line."
        </p>
      </section>

      <section className="ex">
        <h2>Projection pushdown — read 1 field of 20</h2>
        <p>
          The pipeline reads only <code>amount</code>. Every other field is a dead column with no slot: the
          generated scanner hands it to <code>skipValue</code>, which advances past its bytes without decoding
          or allocating anything. A 20-field record costs one number-parse and nineteen byte-skips. No{" "}
          <code>Event</code> is built; the fold runs into a <code>var</code>.
        </p>
        <JsonEx
          src="fuse-src-jsum" gen="fuse-json-sum" bench={SUM_BENCH}
          rival="rival-sum"
          rivalLabel="the field — jsoniter-scala (hand-written reader, no object), jawn, and Jackson, all the same query"
        />
      </section>

      <section className="ex">
        <h2>Lazy decode — a String built only for survivors</h2>
        <p>
          <code>category</code> gets a slot, but a string slot is just <code>(start, len)</code> into the
          buffer — not a decoded String. <code>amount</code> decodes, because the filter needs it;{" "}
          <code>category</code> stays a byte slice and becomes a real <code>String</code> only inside the
          survivor <code>if</code>. Rejected rows never allocate a String. It's the same
          compute-for-survivors sink from the optimizer page, now operating on bytes.
        </p>
        <JsonEx
          src="fuse-src-jcat" gen="fuse-json-cat"
          rival="rival-cat" rivalLabel="jsoniter-scala, narrow codec (only the read fields; the rest skipped)"
        />
      </section>

      <section className="ex">
        <h2>Discard-DCE — count never decodes the projection</h2>
        <p>
          <code>count</code> ignores the element, so <code>map(_.category)</code> is a dead value. The
          optimizer captures the slice but never turns it into a String — zero String allocations across the
          whole run. The same holds for <code>isEmpty</code>/<code>nonEmpty</code>, and for{" "}
          <code>map(expensive).count</code>, where <code>expensive</code> never runs at all. Dead-column
          elimination, reaching all the way to the terminal.
        </p>
        <JsonEx src="fuse-src-jcount" gen="fuse-json-count" bench={COUNT_BENCH} />
      </section>

      <section className="ex">
        <h2>Predicate-fail early-out — abandon a record mid-scan</h2>
        <p>
          The filter field is decoded <em>during</em> the scan; the instant it's known, the predicate runs
          inline, and on failure the record is abandoned right there — the scanner never even looks for the
          later projected field. Here <code>key</code> comes first, <code>payload</code> (a long string) comes
          last, and 90% of records are rejected: each rejected record costs only "scan to <code>key</code>,
          compare, stop." On fat, selective queries this is the biggest win of all.
        </p>
        <JsonEx src="fuse-src-jwide" gen="fuse-json-wide" bench={WIDE_BENCH} />
      </section>

      <section className="ex">
        <h2>Multi-aggregate — three aggregates, one pass, into a case class</h2>
        <p>
          Sum the amount, count the rows, and take the peak score of the active events — <code>aggTo</code>{" "}
          runs all three in a single fused scan, carrying one unboxed accumulator each. The columns the
          aggregates read are merged automatically: <code>amount</code>, <code>status</code>, and{" "}
          <code>score</code> are scanned — 3 of 20 fields, each exactly once — and <code>count</code> reads
          nothing extra. The result is a case class, not a tuple; no <code>Event</code> is ever built, and the
          sum and max compile to <code>dadd</code>/<code>dcmp</code>, not a boxing{" "}
          <code>Numeric</code>/<code>Ordering</code>.
        </p>
        <JsonEx src="fuse-src-jagg" gen="fuse-json-agg" />
      </section>

      <footer className="page__foot">
        <a className="page__next" href="#/fusion">← Back to the optimizer</a>
        <p>
          Scanners are the verbatim post-typer expansion (<code>FuseDebug.show</code>), regenerated to current
          codegen and checked into <code>tests/snapshots/</code>. Numbers are JMH (<code>-prof gc</code>) from{" "}
          <code>benchmarks/JsonProjectionBenchmark</code>.
        </p>
      </footer>
    </article>
  );
}
