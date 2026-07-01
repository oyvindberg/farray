import Snippet from "../components/Snippet";
import BenchPair from "../components/BenchPair";
import { DispatchDiagram } from "../components/Diagrams";

export default function Inline() {
  return (
    <section className="chapter" id="inside-map">
      <h2>How map stays unboxed</h2>

      <p className="lede">
        An elementwise combinator is one <code>inline</code> hop to a generated <code>…Impl</code>, and that
        impl is where the element kind gets resolved. Take <code>map</code>. Its impl is the most over-built
        method in the library: one big <code>summonFrom</code> — a compile-time match on which <code>Repr</code>{" "}
        evidence exists for <code>A</code> — with an outer branch per input kind and, nested inside, one per
        result kind.
      </p>

      <p>
        Here's the shape of it — when the kinds line up (the common case) the branch is a one-liner into a
        specialized leaf method; when they don't, it's a generic loop. Toggle <em>all 81 branches</em> if you
        want to see the full cross-product you'll never write:
      </p>

      <Snippet name="map-dispatch-real" />

      <p>That leaf method is where the loop actually lives:</p>

      <Snippet name="map-leaf" hideFull />

      <p>
        A genuine <code>while</code>-loop over <code>int[]</code>, and <code>f</code> is an{" "}
        <code>IntToIntFn</code> — a specialized SAM, not a boxed <code>Function1</code>. The <code>IntArr</code>{" "}
        case is the fast path; a tree input falls through the <code>case _</code> to the shared traverser — the
        one direction-aware walk every op reuses. Now watch what happens at a concrete <code>FArray[Int]</code>{" "}
        call site. The compiler knows <code>A = Int</code>, so <code>summonFrom</code> keeps the{" "}
        <code>IntRepr</code> branch and deletes the other eighty. So this:
      </p>

      <p className="codeline"><code>xs.map(_ + 1)</code> &nbsp;<span className="codeline__where">where <code>xs: FArray[Int]</code></span></p>

      <p>resolves entirely in the compiler:</p>

      <DispatchDiagram />

      <p>
        And you don't have to take the diagram's word for it. Here's the actual lowering: your whole{" "}
        <code>xs.map(_ + 1)</code> — the dispatch, the kind resolution, all of it — became one line. (The dead
        bindings the expansion leaves behind are elided; toggle <em>raw expansion</em> for the verbatim tree.)
      </p>

      <Snippet name="map-generated" />

      <p>Three things are worth pointing at in that one line, because together they are the entire pitch:</p>

      <ul className="points">
        <li>
          <code>mapLeafInt<strong>Int</strong></code> — the resolution is in the name. <code>summonFrom</code>{" "}
          picked the <code>Int → Int</code> leaf <strong>at compile time</strong> and deleted the other eighty
          branches; the dispatch is gone before the program runs.
        </li>
        <li>
          It's a <strong>call</strong>, not the loop. The <code>while</code>-loop lives <em>inside</em>{" "}
          <code>mapLeafIntInt</code>, which is deliberately <em>not</em> <code>inline</code> — compiled once and
          shared, never copied into your code. That's why the loop isn't in the line above. (Inline it at every
          call site and you'd bloat straight into the JVM's method-size cliff — exactly the trade{" "}
          <code>.fuse</code> opts into on purpose.)
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

      <p>
        And the point of all that machinery, measured — <code>map</code> over a million elements, Int beside
        String. This is also where to be honest, because on its own <code>map</code> is no rout:
      </p>

      <BenchPair
        int="MapIntBenchmark"
        str="MapStrBenchmark"
        caption="map, swept to 100k. On Int (left) FArray edges a bare int[] and runs ~6–32× past everything that boxes the element — List, Vector, both Chunks. On String (right) there's nothing to unbox, so the gap closes to nothing: FArray ties IArray, and fs2/zio Chunk actually edge it by a few percent. When boxing isn't on the table, the wrapper and the SAM aren't free."
      />

      <h3>We're here for the crushes</h3>

      <p>
        Read those two charts honestly and it's a modest win on <code>Int</code> and a modest loss on{" "}
        <code>String</code>. That's the texture of a <em>micro</em>-benchmark — one operation over one array, in
        isolation, where a lone <code>Array.map</code> is a call the JIT already inlines to perfection and the
        opaque wrapper plus the specialized SAM are pure overhead it doesn't carry. FArray isn't built to win
        those by a percent. It's built for what happens the moment the benchmark stops being a micro-benchmark —
        and it doesn't take much.
      </p>

      <p>
        Take that very same <code>map</code> and write eight of them, eight <em>distinct</em> lambdas, in one
        method. IArray funnels all of them into its one shared <code>Array.map</code>, and that call site now
        sees eight different <code>Function1</code> types — well past the JIT's ≥3-type megamorphic threshold,
        so it stops devirtualising: every element pays a real virtual <code>apply</code>, and on{" "}
        <code>Int</code> it re-boxes through <code>Integer</code> on the way out. The bigger the method, the
        more distinct lambdas crowd that one site, the worse it gets. FArray has no shared site to poison — each
        lambda was already spliced into its own specialized loop at compile time, so eight maps are just eight
        tight <code>int[]</code> loops, indifferent to how many of them you wrote.
      </p>

      <BenchPair
        int="MapMegaIntBenchmark"
        str="MapMegaStrBenchmark"
        caption="eight distinct lambdas through map in one method. On Int (left) IArray's shared map site goes megamorphic and re-boxes; FArray has no shared site to poison, so it runs ahead — widening to ~3.3× at 100k. On String (right) there's no boxing to pay, so the same megamorphic dispatch costs almost nothing (~1.05×). Same mechanism; only the boxing tax differs."
      />
    </section>
  );
}
