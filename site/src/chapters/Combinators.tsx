import Snippet from "../components/Snippet";
import BenchChart from "../components/BenchChart";
import BenchPair from "../components/BenchPair";
import Complexity from "../components/Complexity";

export default function Combinators() {
  return (
    <section className="chapter" id="combinators">
      <h2>Two kinds of combinator</h2>

      <p className="lede">
        That split between the flavours is worth naming, because the whole cost model hangs off it. The{" "}
        <strong>structural combinators</strong> — <code>++</code>, <code>:+</code>, <code>take</code>,{" "}
        <code>reverse</code>, <code>slice</code> — change the <em>shape</em>: each builds a lazy node in O(1)
        and copies nothing. The <strong>elementwise combinators</strong> — <code>map</code>,{" "}
        <code>filter</code>, <code>fold</code>, anything that takes a lambda — run it over every element,{" "}
        <em>eagerly</em>, and hand back a flat, array-backed <code>FArray</code> (or a single value, for a
        fold). The first set defers; the second computes.
      </p>

      <p>
        That laziness is the trade. A bare array would be a hair quicker at a single indexed read — one
        type-check that <code>int[]</code> doesn't carry. In exchange it can't even compare or print itself:{" "}
        <code>IArray(1, 2, 3).toString</code> is <code>[I@5a07e868</code> and <code>==</code> is identity.
        FArray wraps the array in an object, so it gives that hair back and gets value <code>equals</code>,{" "}
        <code>hashCode</code> and <code>toString</code> — and a collection you can assemble any way you please,
        in constant time, paying for a flat array only when you ask for one. Here's the cost of each:
      </p>

      <Complexity />

      <p>
        The reason that table is almost all <code>O(1)</code> is that the structural nodes are{" "}
        <strong>lazy</strong>: a <code>Concat</code> or a <code>SliceNode</code> records an intention, it
        doesn't do the work. Nothing is computed until you read. So if you <code>++</code> twenty arrays
        together and then call <code>take(2)</code>, the traversal walks two elements into the tree and stops —
        the other nineteen concatenations are never constructed into anything. That's a lot of collections you
        simply don't build. You pay for what you read, not for what you wrote.
      </p>

      <p>
        And what you do pay <strong>amortizes</strong>. Cons a million elements on one at a time and you've
        built a million-deep tree of O(1) nodes; the first elementwise combinator — a <code>map</code>, a{" "}
        <code>sum</code>, a <code>foldLeft</code> — flattens the whole thing into one contiguous primitive array
        in a single pass, and every read after that is array-speed. The array is the preferred shape, and it
        reasserts itself the moment you compute.
      </p>

      <BenchChart
        cls="SlicingIntBenchmark"
        caption="Structural ops — slice, span, splitAt, grouped, sliding. Lazy nodes mean FArray produces these without copying; against the collections that must materialize each piece the gap runs from a couple× to, for grouped, thousands×. (Shown on Int, but structural ops touch no elements, so the String numbers are identical — there's nothing to box.)"
      />

      <h3>It keeps up with List at List's own game</h3>

      <p>
        Which leads to a result I still find faintly absurd. Because <code>+:</code> is O(1) and the tree is
        lazy, you can use FArray as a literal cons-list — build it with <code>::</code>, take it apart with{" "}
        <code>case h :: t</code>, recursively — and it keeps pace with <code>List</code>, the structure that
        exists for precisely that.
      </p>

      <Snippet name="cons-list" hideFull />

      <p>
        Character-for-character the same code you'd write for <code>List</code>. On pure tear-down recursion
        it's a dead heat; on the cons-build it's about twice as quick.
      </p>

      <BenchPair
        int="ListLikeScalingIntBenchmark"
        str="ListLikeScalingStrBenchmark"
        caption="FArray used as a cons-list — built by ::, summed and mapped by recursion — versus List, Int beside String. On Int it wins the build and the maps; on String it's a dead heat with List throughout. References were never boxed, so there's no gap to open: keeping pace with the cons-list at its own game is the whole win."
      />

      <p>
        One last pair, because it's the one you'd bet against. Sorting is where a raw array should win outright —
        FArray sorts directly on the materialized array (a run-detecting mergesort, no boxed indices):
      </p>

      <BenchPair
        int="SortIntBenchmark"
        str="SortStrBenchmark"
        caption="sortBy / sortWith / sorted, Int beside String. On Int the unboxed mergesort edges IArray (~1.9–2.2× on sortBy/sortWith) and buries the boxing collections. On String there's no unboxing to win, so it ties IArray on sortWith — and still takes sortBy (~3.7×) and sorted outright."
      />
    </section>
  );
}
