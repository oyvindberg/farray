import SideBySide from "../components/SideBySide";
import GeneratedCode from "../components/GeneratedCode";
import { FusionDiagram } from "../components/Diagrams";

export default function Intro() {
  return (
    <section className="chapter" id="intro">
      <h2>Ten years of waiting for the JVM</h2>

      <p className="lede">
        For most of a decade I've tried to write fast code in Scala, and spent most of it frustrated. The
        collections are a joy to write — and box every primitive into the bargain. The <code>while</code> loop
        that doesn't box is a joy to nobody. Everything in between was a promise with a version number.
      </p>

      <p>
        So I waited. For specialization that actually held up. For miniboxing. For the Dotty linker. For
        Valhalla — always, eventually, for Valhalla. Every release I'd read the bytecode, fight the inliner,
        and arrive at the same verdict: if it has to be fast, write it in something else. Usually Java. A
        genuinely terrible state of affairs for a language this good.
      </p>

      <p>
        I started FArray in 2022, mostly out of spite, and prototyped nearly everything you'll see here. Then
        it sat — the rest of the work was large and I was out of evenings. What finally moved it was having
        Claude to push the experiment the rest of the way.
      </p>

      <h2 className="turn">And then — boom</h2>

      <p>
        <strong>First, the unboxing worked.</strong> An <code>FArray[Int]</code> is a real <code>int[]</code>{" "}
        carrying the entire <code>IndexedSeq</code> API — <code>map</code>, <code>filter</code>,{" "}
        <code>fold</code>, <code>sort</code> — without a single <code>java.lang.Integer</code> anywhere. That
        much the 2022 prototype already did.
      </p>

      <p>
        <strong>Then, with a bit more work, the computations fused.</strong> A whole chain of{" "}
        <code>map</code> / <code>filter</code> / <code>flatMap</code> / <code>zip</code> / <code>fold</code>{" "}
        stopped allocating anything between its stages — the macro rewrites the lot, at compile time, into one
        pass over the backing array. This is the part I'd given up on.
      </p>

      <p>
        It's easiest to feel on something long and gnarly. Fourteen stages — <code>flatMap</code>,{" "}
        <code>filter</code>, <code>map</code>, <code>zip</code>, <code>zipWithIndex</code>,{" "}
        <code>foldLeft</code>, twice over — run identically on every collection in the race. The FArray
        version differs by a single word: <code>.fuse</code>. On the left, the code; on the right, what it
        does to everyone else.
      </p>

      <SideBySide
        snippet="fuse-pipeline"
        cls="LongMixedPipelineIntBenchmark"
        title="14-stage pipeline"
        caption={
          <>
            The identical transform across every collection, swept from empty to 100k elements. Hover any size
            to read throughput and the fused FArray's ratio against each rival — including the same pipeline
            run eagerly on FArray itself.
          </>
        }
      />

      <p>
        At a hundred thousand elements that lands at roughly <strong>31× the quickest competitor</strong>,{" "}
        <strong>88× List</strong>, and <strong>15× the very same pipeline run eagerly on FArray</strong> —
        the green bar is so far above the rest that the others flatten to slivers. Hover them for the exact
        numbers.
      </p>

      <p>
        The reason it runs away with it is that <code>.fuse</code> refuses to build any of the work-in-progress.
        Run those stages eagerly and each one allocates an array the next immediately consumes and throws away.
        Fusion collapses the whole chain into a single pass:
      </p>

      <FusionDiagram />

      <p>
        <code>.fuse</code> is a macro, not a runtime call. At compile time it reads the entire chain off the
        syntax tree and rewrites it into one <code>while</code>-loop over the backing array — the intermediate
        collections, the <code>Function1</code>s, the boxing, all gone. And this isn't a claim to take on faith:
        the lowering is checked into the repo as a golden test, regenerated on every build. Here is exactly what
        those fourteen stages compiled to.
      </p>

      <GeneratedCode name="fuse-generated" summary="The loop .fuse emitted — 14 stages, one pass" />

      <p>
        That the Scala 3 macro system can do this — read a call chain, work out which columns nobody reads, and
        emit a specialized loop, all at compile time — is, frankly, a little phenomenal. There's a whole page on{" "}
        <a href="#/fusion">how the macro pulls it off</a>. But fusion is the summit, and we're at the trailhead.
        The rest of this page is the climb: the Java core the loop runs over, the Scala that keeps it unboxed,
        and the cost model that makes the whole thing hang together.
      </p>
    </section>
  );
}
