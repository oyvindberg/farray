import Snippet from "../components/Snippet";
import GeneratedCode from "../components/GeneratedCode";

export default function Inline() {
  return (
    <section className="chapter" id="inline">
      <h2>The inline layer</h2>

      <p className="lede">
        The Java core is fast, but it's generic: it only knows <code>FBase</code>. All the unboxing happens one
        level up, in the Scala API — and the trick is that the surface inlines away. Every combinator is{" "}
        <code>inline</code>, and resolves the element kind at <em>your</em> call site, leaving only a call into
        a shared, specialized loop.
      </p>

      <p>
        The mechanism is Scala 3's <code>summonFrom</code>: a compile-time match on which <code>Repr</code>{" "}
        evidence exists for the element type. Here is the actual generated <code>map</code> — one outer branch
        per input kind, one inner branch per result kind. It's a big, dumb, beautiful cross-product, and you'll
        never see most of it.
      </p>

      <GeneratedCode name="map-dispatch-real" summary="The real map dispatch — generated, input-kind × result-kind" />

      <p>
        When the input and result kinds line up — the overwhelmingly common case — the branch is a one-liner
        that calls a static, specialized leaf method. That method is where the loop actually lives:
      </p>

      <Snippet name="map-leaf" hideFull />

      <p>
        A genuine <code>while</code>-loop over <code>int[]</code>, and <code>f</code> is an{" "}
        <code>IntToIntFn</code> — a specialized SAM, not a boxed <code>Function1</code>. Now watch what happens
        at a concrete <code>FArray[Int]</code> call site. The compiler knows <code>A = Int</code>, so{" "}
        <code>summonFrom</code> keeps the <code>IntRepr</code> branch and deletes the other eighty. So this:
      </p>

      <p className="codeline"><code>xs.map(_ + 1)</code> &nbsp;<span className="codeline__where">where <code>xs: FArray[Int]</code></span></p>

      <p>
        compiles, at that line, to a single resolved leaf call — the eighty branches gone, your lambda lifted
        into one specialized SAM and handed to the loop:
      </p>

      <Snippet name="map-generated" hideFull />

      <p>Three things are worth pointing at, because together they are the entire pitch:</p>

      <ul className="points">
        <li>
          <code>val r: intRepr.type = intRepr</code> — the kind was resolved <strong>at compile time</strong>.
          The whole eighty-branch <code>summonFrom</code> collapsed to a single choice; the dispatch is gone
          before the program runs.
        </li>
        <li>
          <code>mapLeafIntInt(xs, …)</code> — a direct, <strong>monomorphic</strong> call into the shared loop
          specialized for <code>Int → Int</code>. The per-element work lives <em>there</em>, compiled once, so
          this call site stays tiny and the JIT has one hot method to inline instead of a fresh copy at every
          use.
        </li>
        <li>
          <code>{"(v: Int) => v + 1"}</code> — your lambda, materialized <strong>once</strong> as a specialized{" "}
          <code>IntToIntFn</code> SAM and passed to that loop. Typed <code>Int</code> throughout: no{" "}
          <code>Function1</code>, no <code>Integer</code> — the unboxing is in the type, not a JIT bet. Note
          what it is <em>not</em>: it isn't spliced into the loop body. It's a function value the loop calls.
        </li>
      </ul>

      <p>
        That last point is a consequence of how <code>inline</code> works here. <code>mapImpl</code> is{" "}
        <code>inline</code>, so the dispatch and your lambda are resolved and lifted at the call site;{" "}
        <code>mapLeafIntInt</code> is deliberately <em>not</em>, so the one heavy loop is compiled a single time
        and shared. The price of crossing that hand-off is that the lambda has to become a function value —
        hence the SAM. But it's a <em>specialized</em> SAM, so nothing boxes, and it's one allocation per{" "}
        <code>map</code> call, not per element; the JIT inlines its <code>apply</code> into the shared loop at
        runtime. In the bytecode it's a single <code>anonfun</code> lifted by one <code>invokedynamic</code> —
        emitted once, not twice.
      </p>

      <p>
        The version that splices your lambda straight into the <code>while</code>-loop, with no SAM at all, is{" "}
        <code>.fuse</code> — which is precisely why fusion is a separate gear. What the inline layer buys on its
        own is narrower but still decisive: compare a generic <code>map[B](f: A =&gt; B)</code>, where{" "}
        <code>A</code> and <code>B</code> erase to <code>Object</code>, <code>f</code> is a boxed{" "}
        <code>Function1</code>, and every element round-trips through <code>Integer</code>. FArray never writes
        that down. The specialization lives in the call site instead of the signature, which is why it holds even
        as the surrounding method grows and the JIT's inlining budget runs out.
      </p>
    </section>
  );
}
