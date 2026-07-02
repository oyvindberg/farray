import Scorecard from "../components/Scorecard";

export default function Closing() {
  return (
    <section className="chapter" id="scoreboard">
      <h2>Where it all lands</h2>

      <p className="lede">
        That's the construction. Here's the scoreboard — the entire benchmark suite, every operation at every
        size, boiled down to one number per structure.
      </p>

      <Scorecard />

      <p>
        Not every nut and bolt is the fastest of its kind, and the chapters above were honest about the ties
        and the one or two places a bare array still edges it. But assembled — unboxed elementwise ops at raw
        array speed, structural ops in O(1), fusion when you ask for it, and a real immutable collection with
        value semantics on top — it adds up to the fastest of the field. Every bar on this page is a JMH
        measurement you can hover, and every chart will show you the exact benchmark that produced it. The
        full suite, chart by chart, lives on <a href="#/reference">the FArray benchmarks page</a>. And FArray
        no longer travels alone — <a href="#/fset">FSet</a>, the immutable unboxed set built on the same core,
        has <a href="#/setbench">a scorecard of its own</a>.
      </p>
    </section>
  );
}
