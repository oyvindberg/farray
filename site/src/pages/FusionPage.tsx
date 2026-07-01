import Snippet from "../components/Snippet";

function FuseEx({ src, gen }: { src: string; gen: string }) {
  return (
    <div className="fuseex">
      <p className="fuse-cap">you write</p>
      <Snippet name={src} hideFull />
      <p className="fuse-cap">the macro emits</p>
      <Snippet name={gen} />
    </div>
  );
}

export default function FusionPage() {
  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">how the macro pulls it off</p>
        <h1>The fuse optimizer</h1>
        <p className="page__lede">
          <code>xs.fuse.map(f).filter(p).&lt;terminal&gt;</code> is a macro, not a runtime data structure. At
          compile time it reads the <em>whole</em> chain off the typed syntax tree and emits one specialized{" "}
          <code>while</code> loop with your lambdas inlined into the body — no view nodes, no{" "}
          <code>Function1</code>, no boxing, no megamorphism. What follows is the loop you'd have written by
          hand, produced from the pipeline you'd actually want to write. Every emitted block is the verbatim
          post-typer expansion (<code>FuseDebug.show</code>), checked into the repo as a golden test.
        </p>
      </header>

      <section className="ex">
        <h2>One loop, no closures</h2>
        <p>
          Three stages, one <code>int[]</code> scan. <code>_ + 1</code> becomes the bytecode{" "}
          <code>iadd</code>; no <code>Function1</code> is allocated; the filter is an <code>if</code> that
          wraps everything downstream of it. Compare a <code>View</code>, which stores three{" "}
          <code>Function1</code>s and calls each through an interface, per element.
        </p>
        <FuseEx src="fuse-src-oneloop" gen="fuse-opt-oneloop" />
      </section>

      <section className="ex">
        <h2>Dead-column elimination</h2>
        <p>
          A tuple or case class is a set of independent <strong>columns</strong>. Here <code>map</code> builds
          three, then the pipeline reads only two of them. The optimizer never builds the tuple: column 2
          (<code>x * 13</code>) is read by nobody, so it's dead and simply absent from the loop; column 0
          (<code>x % 3</code>) is needed by the filter, so it's computed eagerly. A <code>View</code> allocates
          the 3-tuple — plus three boxed <code>Int</code>s — for every element, and throws most of it away.
        </p>
        <FuseEx src="fuse-src-dce" gen="fuse-opt-dce" />
      </section>

      <section className="ex">
        <h2>Compute-for-survivors — the sink</h2>
        <p>
          Look at <em>where</em> <code>expensive(x)</code> lands: inside the <code>if</code>. Column 1 is
          needed only by the final <code>map</code>, which sits downstream of the filter, so it's computed
          only for elements that <strong>survive</strong>. An expensive column behind a selective filter runs
          a fraction of the time — an optimization the lazy-collection model can't even express, because it
          has no idea which columns a later stage will read.
        </p>
        <FuseEx src="fuse-src-sink" gen="fuse-opt-sink" />
      </section>

      <section className="ex">
        <h2>Common-subexpression elimination</h2>
        <p>
          <code>x * x</code> is written twice but computed <strong>once</strong> — bound to one{" "}
          <code>val</code> and reused across both tuple components. Decomposition, dead-column elimination,
          the sink, and CSE all fall out of a single idea: model a product as independent lazy columns, and
          bind each at its first use.
        </p>
        <FuseEx src="fuse-src-cse" gen="fuse-opt-cse" />
      </section>

      <section className="ex">
        <h2>…and the decomposition reaches the fold's lambda</h2>
        <p>
          The same column analysis reaches into a <code>foldLeft</code>/<code>reduce</code> lambda. Here the
          fold reads only <code>s.score</code>, so the optimizer never builds the <code>Stat</code>: its other
          fields — and the object itself — are dead and absent, and the loop is just{" "}
          <code>acc = acc + (x * 100)</code>. Without this, the fold would rebuild the entire product per
          element just to read one field.
        </p>
        <FuseEx src="fuse-src-fold" gen="fuse-opt-fold" />
      </section>

      <footer className="page__foot">
        <a className="page__next" href="#/json">The JSON demo — the same optimizer, over bytes →</a>
        <p>
          Emitted blocks are the verbatim post-typer expansion (<code>FuseDebug.show</code>), regenerated to
          current codegen and checked into <code>tests/snapshots/</code>. Shown, never executed.
        </p>
      </footer>
    </article>
  );
}
