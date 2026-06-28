import Snippet from "../components/Snippet";
import GeneratedCode from "../components/GeneratedCode";

export default function Inline() {
  return (
    <section className="chapter" id="inline">
      <h2>The inline layer</h2>

      <p className="lede">
        The Java core is fast, but it's generic: it only knows <code>FBase</code>. All the unboxing happens one
        level up, in the Scala API — and the trick is that the API isn't really there at runtime. Every
        combinator is <code>inline</code>, and it picks the element kind at <em>your</em> call site.
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

      <p>compiles, at that line, to exactly this — no dispatch, no fallback, nothing generic left over:</p>

      <Snippet name="map-generated" hideFull />

      <p>Three things are worth pointing at, because together they are the entire pitch:</p>

      <ul className="points">
        <li>
          <code>val r: intRepr.type = intRepr</code> — the kind was resolved <strong>at compile time</strong>.
          The whole eighty-branch <code>summonFrom</code> collapsed to a single choice; the dispatch is gone
          before the program runs.
        </li>
        <li>
          <code>mapLeafIntInt(xs, …)</code> — a <strong>static, monomorphic</strong> call into the method
          specialized for <code>Int → Int</code>. Nothing megamorphic for the JIT to give up on.
        </li>
        <li>
          <code>{"(v: Int) => v + 1"}</code> — your lambda, <strong>inlined</strong> and typed <code>Int</code>{" "}
          end to end. No <code>Function1</code>, no <code>Integer</code>. Unboxed by construction, not by the
          JIT's good graces.
        </li>
      </ul>

      <p>
        Compare a generic <code>map[B](f: A =&gt; B)</code>: <code>A</code> and <code>B</code> erase to{" "}
        <code>Object</code>, <code>f</code> is a boxed <code>Function1</code>, and every element makes the round
        trip through <code>Integer</code>. FArray doesn't optimize that away — it never writes it down. The
        specialization lives in the call site instead of the signature, which is why it holds even as the
        surrounding method grows and the JIT's inlining budget runs out.
      </p>
    </section>
  );
}
