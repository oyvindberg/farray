import SideBySide from "../components/SideBySide";
import GeneratedCode from "../components/GeneratedCode";
import { FusionDiagram } from "../components/Diagrams";

export default function Intro() {
  return (
    <section className="chapter" id="intro">
      <h2>Ten years of waiting for the JVM</h2>

      <p className="lede">
        For most of a decade I tried to write fast code in Scala, and spent most of it frustrated. The
        collections are a joy to write — and box every primitive into the bargain. The <code>while</code> loop
        that doesn't box is a joy to nobody. Everything in between was a promise with a version number.
      </p>

      <p>
        So I waited. For specialization that actually held up. For miniboxing. For the Dotty linker. For
        Valhalla — always, eventually, for Valhalla, and the <code>List&lt;int&gt;</code> it kept almost
        promising: generics specialized all the way down to flat, unboxed storage. That's the half of
        Valhalla that still hasn't shipped. Every release I'd read the bytecode, fight the inliner, and
        arrive at the same verdict: if it has to be fast, write it in something else. Usually Java. A
        genuinely terrible state of affairs for a language this good.
      </p>

      <h3>A thousand lines of spite</h3>

      <p>
        Eventually spite won. The initial commit lands in January 2023, at three in the morning: one
        thousand-line file, the whole <code>IndexedSeq</code> API hand-written as <code>inline</code>{" "}
        <code>while</code> loops over a single flat <code>Array[AnyRef]</code>, casts on the way out,{" "}
        <code>System.arraycopy</code> wherever shapes lined up. It worked — flat storage and straight-line
        loops beat the collection library, exactly as the profiler always said they would.
      </p>

      <p>
        But it was boxed. The readme of the time is honest about it: <em>“if you need specialized primitives
        this is not (at least yet) the library for you.”</em> And that “yet” was load-bearing — unboxing
        wasn't a feature you could bolt on, it wanted a different animal entirely: storage specialized per
        element kind, a core the inliner couldn't defeat, machinery to keep a user's lambda unboxed across a
        library boundary. I kept picking at it, and the experiments sprawled across an old machine, most of
        them never reaching the repo — whose only commit in the three years that followed bumps the version
        of the build tool. The shape of the thing was clear the whole time. The evenings weren't there.
      </p>

      <h2 className="turn">And then — boom</h2>

      <p>
        In June 2026 I picked it back up, this time with Claude doing the heavy lifting. Twelve days and
        nearly three hundred commits later, the roadmap I'd been quietly resequencing for a decade had stopped
        being sequential. The milestones didn't arrive one by one; they arrived <strong>on top of each
        other</strong>:
      </p>

      <ul className="points">
        <li>
          <strong>A working FArray.</strong> An <code>FArray[Int]</code> that <em>is</em> a real{" "}
          <code>int[]</code>, carrying the entire <code>IndexedSeq</code> API — <code>map</code>,{" "}
          <code>filter</code>, <code>fold</code>, <code>sort</code> — without a single{" "}
          <code>java.lang.Integer</code> anywhere. The thing the decade was spent waiting for.
        </li>
        <li>
          <strong>Working fusion.</strong> A whole chain of <code>map</code> / <code>filter</code> /{" "}
          <code>flatMap</code> / <code>zip</code> / <code>fold</code>, rewritten at compile time into one pass
          over the backing array, allocating nothing between stages. The part I'd given up on ever having —
          its design note is dated one day before the first three working phases.
        </li>
        <li>
          <strong>And a working Claude.</strong> The reason the other two exist. The redesign the prototype
          demanded — a generated Java core, per-kind specialization, a macro optimizer — was years of evenings
          I didn't have. It became twelve days, with the benchmark suite as referee on every commit.
        </li>
      </ul>

      <p>
        Everything on this page follows from those three landing together. And they stack — unboxed elements
        under structural laziness under fused pipelines — so the fairest way to show it is to stack them.
        Fourteen stages: <code>flatMap</code>, <code>filter</code>, <code>map</code>, <code>zip</code>,{" "}
        <code>zipWithIndex</code>, <code>foldLeft</code>, twice over — the kind of pipeline you'd write on any
        ordinary Tuesday and pay for on every element. Every collection in the race runs the identical code;
        the FArray version differs by a single word: <code>.fuse</code>. Before you look at the chart, pick
        the speedup you'd refuse to believe.
      </p>

      <SideBySide
        snippet="fuse-pipeline"
        cls="LongMixedPipelineIntBenchmark"
        title="14-stage pipeline"
        caption={
          <>
            The identical transform on every collection, swept from empty to 100k elements. Hover any size to
            read throughput and the fused FArray's ratio over each rival — including the same pipeline run
            eagerly on FArray itself.
          </>
        }
      />

      <p>
        At a hundred thousand elements: roughly <strong>36× the quickest competitor</strong>,{" "}
        <strong>83× List</strong> — and, the number that says the most, <strong>15× the very same pipeline
        run eagerly on FArray itself</strong>. Whatever factor you picked, the green bar probably clears it;
        it stands so far above the field that everyone else flattens to slivers. Hover them for the exact
        numbers.
      </p>

      <p>
        The reason it runs away with it: <code>.fuse</code> refuses to build any of the work-in-progress. Run
        those stages eagerly and each one allocates an array whose only purpose is to be consumed and thrown
        away by the next. Fusion collapses the whole chain into a single pass:
      </p>

      <FusionDiagram />

      <p>
        <code>.fuse</code> is a macro, not a runtime call. At compile time it reads the entire chain off the
        typed syntax tree and rewrites it into one <code>while</code> loop over the backing array — the
        intermediate collections, the <code>Function1</code>s, the boxing: gone. And this isn't a claim to
        take on faith. The lowering is checked into the repo as a golden test, regenerated on every build.
        Here is exactly what those fourteen stages compiled to.
      </p>

      <GeneratedCode name="fuse-generated" summary="The loop .fuse emitted — 14 stages, one pass" />

      <p>
        That the Scala 3 macro system can do this — read a call chain off the tree, work out which columns
        nobody downstream reads, and emit the specialized loop you'd have written by hand — is, frankly, a
        little phenomenal. There's a whole page on <a href="#/fusion">how the macro pulls it off</a>. But
        fusion is the summit, and we're at the trailhead. The rest of this page is the climb: the Java core
        the loop runs over, the Scala that keeps it unboxed, and the cost model that makes the whole thing
        hang together.
      </p>
    </section>
  );
}
