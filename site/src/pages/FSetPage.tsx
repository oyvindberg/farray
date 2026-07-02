import Snippet from "../components/Snippet";
import GeneratedCode from "../components/GeneratedCode";
import BenchChart from "../components/BenchChart";
import BenchPair from "../components/BenchPair";
import Scorecard from "../components/Scorecard";

// Finiteness propagation — the per-operation transfer functions (docs/fset-design.md §1.2).
const FINITENESS: { op: string; finite: string; note: string }[] = [
  { op: "a ∪ b", finite: "both finite", note: "infinite if either side is" },
  { op: "a ∩ b", finite: "either finite", note: "infinite ∩ finite is always finite — the gem of the table" },
  { op: "a ∖ b", finite: "left finite", note: "removing anything from a finite set stays finite" },
  { op: "a ⊕ b", finite: "both finite", note: "infinite ⊕ infinite is unknown — unlike ∪" },
  { op: "¬a", finite: "never (from finite)", note: "complement of a finite set is infinite; of an infinite one, unknown" },
  { op: "a + x · a − x", finite: "same as a", note: "single-element updates preserve the class" },
];

function FinitenessTable() {
  return (
    <figure className="figure">
      <table className="cx">
        <thead>
          <tr><th className="cx-op">operation</th><th className="cx-cost">result is finite iff</th><th className="cx-b">why</th></tr>
        </thead>
        <tbody>
          {FINITENESS.map((r) => (
            <tr key={r.op}>
              <td className="cx-op">{r.op}</td>
              <td className="cx-cost cx-cost--free">{r.finite}</td>
              <td className="cx-b">{r.note}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </figure>
  );
}

export default function FSetPage() {
  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">the second collection</p>
        <h1>FSet</h1>
        <p className="page__lede">
          An immutable, unboxed set on the FArray playbook — a generated Java core, per-kind specialization,{" "}
          <code>inline</code> dispatch — and one contrarian bet on top. Every immutable set on the JVM pays
          the persistent-trie tax to make single-element updates cheap. FSet refuses to pay it, and builds an
          immutable set the way you actually use one: <strong>frozen flat leaves, queried hard, combined
          lazily</strong> — so you pay for the questions you ask, not the structure you didn't need.
          Everything on this page is a consequence of that one bet. And as everywhere on this site: every bar
          is a JMH measurement you can hover, every snippet is extracted from source that compiles.
        </p>
      </header>

      <section className="ex">
        <h2>The trie tax</h2>
        <p>
          Persistent sets — Scala's <code>immutable.Set</code>, Clojure's, CHAMP, all of them — are built
          around one trade: store the elements in a 32-way trie so that adding one element shares almost the
          whole structure. It's a beautiful trade <em>if your workload is single-element churn with old
          versions retained</em>. But it taxes everything else: every <code>contains</code> hops
          pointer-to-pointer down the trie, every primitive is boxed into a node, every bulk union walks and
          rebuilds tree structure, and the whole thing runs 4–15× the memory of the flat data. And the
          workload most immutable sets actually live is the opposite one —{" "}
          <strong>build once, then query and combine a lot</strong>.
        </p>
        <p>
          So FSet inverts the trade. A materialized set is a <strong>frozen flat leaf</strong> — a packed
          sorted primitive array, a frozen open-addressing hash table, a dense bitmap, or a 16-byte range —
          with no tree to hop and nothing boxed. And the persistent operations don't rebuild anything at all:{" "}
          <code>∪ ∩ ∖ ⊕</code>, <code>+</code> and <code>-</code> are <strong>O(1) lazy nodes</strong> that
          share both operands. No other set on the JVM has a lazy set algebra. Immutability stops being the
          thing you apologize for: a table that will never grow again can be laid out for pure probe speed,
          and the algebra comes free.
        </p>
        <Snippet name="set-node-tree" hideFull />
        <p>
          Same construction as FArray one tab over: generated, <code>final</code>, fields-and-methods Java,
          one class per shape per element kind, all behind{" "}
          <code>opaque type FSet[A] &lt;: AnyRef = SBase</code> — zero wrapper allocation, primitives never
          boxed.
        </p>
      </section>

      <section className="ex">
        <h2>A set is more than a HashSet</h2>
        <p>
          Mainstream languages ship one thing called "Set": a finite, enumerable hash collection. But that's
          the special case, not the concept. Mathematically a set of <code>A</code>s is an element of the{" "}
          <strong>Boolean algebra 2<sup>A</sup></strong> — something you can ask membership of, union,
          intersect, subtract, complement. "All the Ints above 5" is a perfectly good set; you just can't
          enumerate it. FSet models the whole algebra, and recovers the classical enumerable set as the{" "}
          <em>materialized special case</em>.
        </p>
        <p>
          The type system tracks which kind you're holding, as a small lattice of capabilities — built on a
          Scala 3 trick long believed impossible. The folklore says opaque types can't subtype each other;
          it's false. An opaque type's upper bound doesn't have to be spent on <code>AnyRef</code> — bound it
          against the <em>parent opaque type</em> and you get real, externally visible, zero-wrapper
          subtyping. Every level below is the same bare <code>SBase</code> at runtime; upcasts are free; an
          op defined once at the top is inherited by everything beneath it:
        </p>
        <Snippet name="set-lattice" hideFull />
        <p>
          Membership and the algebra live at the top. Traversals and transforms — <code>foreach</code>,{" "}
          <code>filter</code>, <code>map</code>, <code>flatMap</code> — live on <code>FSetFinite</code>:
          anything provably finite may enumerate (they materialize internally, memoized on the node), and
          since a transform builds a fresh materialized set anyway, demanding an explicit{" "}
          <code>.materialize</code> first would add a keyword but no information. The cheap observations and
          value equality — <code>size</code>, <code>iterator</code>, <code>sameElements</code> — need the
          frozen leaf itself and sit on <code>FSetMaterialized</code>. The compiler enforces the split:{" "}
          <code>(a ++ b).size</code> doesn't compile until you <code>.materialize</code>, and{" "}
          <code>FSet.above(5).size</code> doesn't compile at all. <code>FSetInfinite</code> is the honest
          outlier — an infinite set <em>is</em> a predicate, so it's contravariant like one, kept out of the
          invariant chain and bridged in by a <code>Conversion</code>. Ranges round it out:{" "}
          <code>FSet.range(0, 1_000_000_000)</code> is a finite, materializable set that costs 16 bytes.
        </p>
        <p>
          Here's the part with real depth: <strong>finiteness is never decided — it's propagated.</strong>{" "}
          Whether an arbitrary set is finite is undecidable (Rice's theorem), so FSet never asks. Finiteness
          is a syntactic property of which combinator built the set, and each operation has a transfer rule:
        </p>
        <FinitenessTable />
        <p>
          The gem is intersection: <em>anything</em> ∩ finite is finite, so filtering an infinite set by a
          finite one lands you safely back in enumerable territory — and the types know it statically. The
          cases the rules can't decide (infinite ∩ infinite, a set handed to you as bare <code>FSet</code>)
          fall to the top type, narrowable at runtime by <code>.finite: Option[FSetFinite[A]]</code> — a
          conservative structural inspection that never returns a false <code>Some</code>.
        </p>
        <p>
          Two more consequences of taking the algebra seriously. <strong>Emptiness is the single decision
          primitive</strong>: <code>a == b</code> is <code>(a ⊕ b).isEmpty</code>, <code>a subsetOf b</code>{" "}
          is <code>(a ∖ b).isEmpty</code>, disjointness is <code>(a ∩ b).isEmpty</code> — so the comparisons
          ride the same lazy algebra as everything else. And <strong>map is not a homomorphism</strong> —
          the image of a set doesn't distribute over ∩ or ¬ — which is precisely why <code>map</code> lives
          on the finite tier while <code>filter</code>, which <em>is</em> just ∩ with a predicate, could in
          principle stay symbolic. The capability lattice isn't API taste; it's the algebra's own structure,
          spelled as types.
        </p>
        <Snippet name="set-surface" hideFull />
      </section>

      <section className="ex">
        <h2>The crown op: contains</h2>
        <p>
          Everything above was designed backwards from one question: how fast can{" "}
          <code>xs.contains(x)</code> possibly be? The answer is one compiled-once loop per element kind —
          no recursion on the spine, no allocation, and a neat trick for negations: <code>SXor</code> and{" "}
          <code>SComplement</code> don't get evaluated, they just flip a parity bit that's applied to
          whatever the leaf answers. On the common leaf-only path <code>flip</code> is provably false and
          folds away.
        </p>
        <Snippet name="set-contains-int" hideFull />
        <p>
          Each arm is a leaf giving its best answer: the bitmap is a single bit-test, the range is two
          compares, the frozen hash is a one-hop probe, the small sorted leaf is an early-exit scan that
          stays in registers. And the algebra arms are why the lazy nodes are queryable at all —{" "}
          <code>contains</code> distributes over <code>∪ ∩ ∖ ⊕ ¬</code> as a Boolean homomorphism (evaluate
          membership at a point and the whole algebra collapses to Boolean logic), so membership in a
          combined set never materializes anything.
        </p>
        <BenchPair
          suite="fset"
          int="IntSetContainsHitBenchmark"
          str="StrSetContainsHitBenchmark"
          caption={
            <>
              Point-lookup hits, Int (left) and String (right), against every serious JVM set — mutable ones
              included. Int runs at the hardware floor — one mix, one load, one compare — and at 100k the
              frozen table is the fastest of the entire field. On String, FSet ties{" "}
              <code>scala.mutable</code> outright at 1000 and 100k and leads everything immutable.
            </>
          }
        />
        <p>
          For references there's no primitive trick to lean on, so the frozen leaf borrows the best idea in
          modern hash tables — Facebook's F14 / abseil's control bytes. Next to the key array sits a dense{" "}
          <code>byte[]</code> of 7-bit hash fingerprints; a probe reads <em>one cache-resident byte</em> and
          rejects almost every miss before touching a key object, and only a fingerprint match pays{" "}
          <code>.equals</code>:
        </p>
        <Snippet name="set-f14-probe" hideFull />
        <p>
          That's why <em>misses</em> — half of real membership traffic — are where the frozen layout shines:
          FSet's miss path is fastest-or-tied in the whole field at every size, mutable tables included. The
          honest gap
          is elsewhere: probe with a <em>scrambled mix</em> of hits and misses, defeating branch prediction,
          and the chained layout of <code>scala.mutable</code> and friends pulls 1.4–2× ahead on String.
          That loss is measured, understood, structural — and accepted; the section on{" "}
          <a href="#graveyard">the experiments that didn't survive</a> shows how hard it was probed before
          being accepted. It's the one tax paid for a layout that wins nearly everything else on this page.
        </p>
      </section>

      <section className="ex">
        <h2>Dense Int data wants to be a bitmap</h2>
        <p>
          Int sets in the wild are rarely random 32-bit noise — they're IDs, indices, year ranges, enum
          ordinals. Dense, in other words. The leaf builder measures density at construction and, when the
          value span is at most 64× the element count, skips hashing entirely: the set becomes{" "}
          <code>SIntBitmap</code>, a bare <code>long[]</code> where <code>contains</code> is one shift and
          one mask.
        </p>
        <Snippet name="set-bitmap-leaf" hideFull />
        <GeneratedCode
          name="set-density-router"
          summary="The density router — bitmap when span ≤ 64·n, else sorted/hash"
        />
        <p>
          The payoff compounds on bulk operations, because the algebra closes over the representation:
          bitmap ∪ bitmap is a word-parallel OR, intersection an AND, difference an AND-NOT — 64 elements
          per instruction, cardinality by <code>popcount</code>, and the merge kernels work region-split, so
          an intersection only touches the window where the two bitmaps overlap. That's the entire reason a
          general-purpose immutable set can stand next to <code>immutable.BitSet</code> and RoaringBitmap in
          their own niche — while also being a set of Strings tomorrow.
        </p>
        <GeneratedCode name="set-bitmap-merge" summary="The word-parallel bitmap merge — ∪ ∩ ∖ ⊕ at 64 elements per op" />
        <BenchChart
          suite="fset"
          cls="IntSetMergeUnionBenchmark"
          caption={
            <>
              Union two sets and materialize the result. A dead heat with <code>immutable.BitSet</code> at
              16, ~1.9× past it at 1000 and ~4× at 100k, with RoaringBitmap ~1.8× behind — and every
              hash-based set thousands of times back, because they must rehash every element FSet ORs in
              bulk.
            </>
          }
        />
        <p>
          One honest cell goes the other way: at size 16, Scala's <code>BitSet1</code>/<code>BitSet2</code>{" "}
          keep their entire population in one or two <em>fields</em> — no array, no indirection — and no
          array-backed bitmap can match a single-word update. Measured, accepted; it's a size-16 trophy.
        </p>
      </section>

      <section className="ex">
        <h2>The algebra is lazy, and materialize is a merge</h2>
        <p>
          Because <code>a ++ b</code> is just a node, the natural way to use FSet is to <em>build an algebra
          and query it</em>. When you do need the elements, <code>.materialize</code> folds the tree into one
          leaf — iteratively, spine-first, never JVM recursion (a loop-built chain of ten thousand{" "}
          <code>+</code> nodes must not be a StackOverflow) — and <strong>memoizes</strong> the result on the
          node, so a shared subtree is merged once ever. Even here the discipline is measured, not assumed:
          the first cut collected a spine list for <em>every</em> materialize and regressed small merges —
          shallow trees like a plain <code>a &amp; b</code> now take a fast path that allocates nothing but
          the answer:
        </p>
        <Snippet name="set-union-node" hideFull />
        <GeneratedCode name="set-materialize" summary="Iterative left-spine materialize — pairwise union rounds, memoized per node" />
      </section>

      <section className="ex">
        <h2>To materialize or not — two opposite answers</h2>
        <p>
          So when should you actually call <code>.materialize</code>? FSet's benchmarks give a beautifully
          clean answer, and it's <em>different per element kind</em> — which is itself the strongest argument
          for a lazy algebra: the right eagerness isn't a property of your code, it's a property of the data,
          so the library decides.
        </p>
        <p>
          <strong>For Int, merging is nearly free</strong> — a word-parallel OR — so the smart{" "}
          <code>++</code> constructor eagerly merges cheap pairs (two bitmaps, two tiny leaves) on the spot,
          and a chain of unions quietly collapses into a single bitmap. There is no lazy-versus-eager
          distinction left to care about:
        </p>
        <BenchChart
          suite="fset"
          cls="IntSetUnion3ContainsBenchmark"
          caption={
            <>
              The signature workload: union three sets, then probe the result <em>n</em> times. On Int the
              chain collapses to one bitmap — FSet edges past both BitSets from 1000 up (~1.1–1.2×), runs
              ~3.7× past Roaring at 100k, and the hash sets — which must eagerly rebuild the union — are
              25–670× behind.
            </>
          }
        />
        <p>
          <strong>For references, merging is the expensive thing</strong> — every String rehashed into a new
          table — so FSet keeps reference unions lazy and lets <code>contains</code> distribute over the
          tree. Every competitor must pay the full O(n) rebuild before answering a single query; FSet just
          answers:
        </p>
        <BenchChart
          suite="fset"
          cls="StrSetUnion3ContainsBenchmark"
          caption={
            <>
              The same workload on Strings, never materialized. Fastest at every size — ~3× the best mutable
              table at 100k, 6–9× fastutil, Guava, HPPC and CHAMP. This is the moat: no other JVM set can
              answer membership in a union it hasn't built.
            </>
          }
        />
        <p>
          And the kicker, straight from the data: at <em>n</em> queries over the union, the lazy tree beats{" "}
          <strong>FSet's own materialized path</strong> — by ~2.7× at 16, ~2× at 1000, ~1.2× at 100k.
          Deferring the merge wins even when you query as many times as there are elements; you'd
          materialize a reference union only when you intend to query it <em>far</em> more than that. And
          when you do, you still come out ahead:
        </p>
        <BenchChart
          suite="fset"
          cls="StrSetUnion3MatContainsBenchmark"
          caption={
            <>
              Union three String sets, <code>.materialize</code> once, then probe <em>n</em> times —
              build-once-query-many. The cached-hash merge keeps FSet fastest here too: ~3× the best mutable
              table and ~4.9× fastutil at 100k.
            </>
          }
        />
      </section>

      <section className="ex">
        <h2>References merge on cached hashes</h2>
        <p>
          What makes that materialized-String row fast is that the bulk algebra for reference elements has
          its own unboxed column: every materialized Ref leaf carries its elements alongside a parallel{" "}
          <code>int[]</code> of cached hashes, kept <em>hash-sorted</em>. Union, intersection, difference and
          xor then become two-pointer merges over plain <code>int</code> comparisons — cache-sequential,
          allocation-tight, with <code>.equals</code> paid only inside a hash-collision run:
        </p>
        <Snippet name="set-merge-ref" hideFull />
        <p>
          Even the <em>build</em> is arranged around the freeze. Constructing a String set dedups through the
          F14 table and records the hashes as it goes, deferring the hash-sort to a memoized view that only
          exists if the algebra ever asks for it. The result is the part that should sting the most for a
          mutable table: <strong>building the frozen, immutable set is faster than building any mutable
          one</strong>:
        </p>
        <BenchChart
          suite="fset"
          cls="StrSetBuildBenchmark"
          caption={
            <>
              Build a String set from an array. Fastest at every size — ~1.2× <code>scala.mutable</code> at
              16 rising to ~2.5× at 100k, ~2× fastutil, and 33× the immutable CHAMP Set, which pays trie
              construction per element.
            </>
          }
        />
        <p>
          The same two sorted hash columns make value equality embarrassingly fast: two pointers, int
          compares, done — no probing one set with the other's elements. <code>sameElements</code> on String
          sets is the clearest structural win in the whole suite:
        </p>
        <BenchChart
          suite="fset"
          cls="StrSetEqualsBenchmark"
          caption={
            <>
              Set equality on Strings. One merge-shaped pass over cached hashes versus probe-per-element:
              fastest at every size — ~5× the mutable table at 100k, ~17× fastutil, ~23× the immutable Set.
              Value equality is CHAMP's documented weak spot; here it's a headline.
            </>
          }
        />
      </section>

      <section className="ex">
        <h2>Even map stays in bitmap-land</h2>
        <p>
          Transforms are where set semantics usually force a detour — collect values, sort, dedup, rebuild.
          FSet's <code>map</code> on an Int set instead word-scans the source bitmap and ORs each{" "}
          <code>f(e)</code> straight into a growable bitmap accumulator that re-bases on both ends. Dense in,
          dense out: no intermediate array, no sort, dedup for free — and if the output turns out sparse, the
          builder degrades once to the ordinary path and loses nothing.
        </p>
        <GeneratedCode name="set-bitmap-builder" summary="The growable bitmap accumulator behind map — dense outputs never leave bitmap form" />
        <BenchChart
          suite="fset"
          cls="IntSetMapBenchmark"
          caption={
            <>
              map an Int set through a function and rebuild the set. The fused scan-into-bitmap build beats
              even <code>immutable.BitSet</code> from 1000 up (~1.2–1.3×) and runs ~19× past fastutil at
              100k; at size 16 the BitSet's single-word build still wins.
            </>
          }
        />
        <p>
          <code>flatMap</code> gets the same treatment, split by write kind. Each element's result set is
          already a materialized leaf, so for Int the leaves fold together with pairwise word-OR union
          rounds; for references each part's raw element column is appended — a bulk{" "}
          <code>System.arraycopy</code>, no iterator, no boxing — and the deduplicating one-pass build runs{" "}
          <em>once</em> at the end. That last choice was measured, not assumed: the first cut merged Ref
          leaves pairwise too, re-allocating at every round, and lost to <code>immutable.Set</code>;
          append-once turned it into a win.
        </p>
        <BenchPair
          suite="fset"
          int="IntSetFlatMapBenchmark"
          str="StrSetFlatMapBenchmark"
          caption={
            <>
              flatMap with a 2× expansion (<code>x → {"{x, x+1}"}</code> / <code>s → {"{s, s+\"x\"}"}</code>).
              fastutil has no flatMap, so its bar is the raw hand-written double-add loop — the strongest
              possible baseline, allocating no inner sets at all. At small sizes that raw loop wins, as it
              should; at 100k FSet passes even it — ~2.6× on Int, ~1.3× on String.
            </>
          }
        />
      </section>

      <section className="ex" id="graveyard">
        <h2>The experiments that didn't survive</h2>
        <p>
          A performance claim on this site has to survive measurement, and most ideas don't. This section
          exists because negative results are where the real understanding lives — and because it's the
          proof that the losses documented above are structural, not unexplored. Five plausible
          optimizations, each with an obvious story, each killed by JMH:
        </p>
        <p>
          <strong>Eager single-element updates.</strong> Surely <code>set + x</code> on a small bitmap should
          just copy the words and set the bit? Measured: 0.4× the lazy node. Scala's <code>BitSet1</code>{" "}
          keeps its word in a <em>field</em> — an array-backed copy can't match it, and the O(1) lazy node is
          the better persistent-update shape at every size. The "obvious" eager path lost to FSet's own
          laziness.
        </p>
        <p>
          <strong>Dropping the F14 control bytes for small tables.</strong> Hypothesis: the fingerprint byte
          is a pure tax on in-cache hits — an extra load before the key load you'll do anyway. Measured: hits
          didn't move (the byte was never the hit bottleneck), and <em>misses regressed 17%</em> — the
          control byte rejects a miss more cheaply than a key load plus a failed <code>.equals</code> even
          when everything is in L1. The ctrl byte earns its keep at every size.
        </p>
        <p>
          <strong>SWAR Swiss-table probing.</strong> First, the methodological find: a benchmark that probes
          one fixed key flatters whoever got lucky with that key's placement, so a varied-probe benchmark
          (1024 scrambled hits and misses per op, defeating branch prediction) was built — and it exposed
          that some single-probe "wins" were placement luck. SWAR group probing — pack eight fingerprints in
          a <code>long</code>, match branch-free — was the textbook fix. Measured on GraalVM: slower.
          The branch-collapse crossover the literature cites needs ~32k+ distinct probe targets; below that,
          the fixed per-group SWAR cost outweighs the branches it saves.
        </p>
        <p>
          <strong>A cheap multiplicative hash, verified at build time.</strong> The frozen table's superpower
          is that it can <em>check</em> a layout before shipping it, so: replace the ~9-cycle murmur mix with
          a 3-cycle Fibonacci multiply and reseed at build until max displacement is small. Measured: dead —
          misses down 33–46%. The mechanism is the interesting part: a multiplicative-only hash is{" "}
          <em>linear</em>, so key families differing by a constant land as a constant <em>shift</em> of the
          stored population, and miss probes walk straight through stored clusters. Build-time verification
          can't save it, because it only sees stored keys — miss cost depends on the correlation between
          probes and storage, and only a nonlinear avalanche kills that. That's what murmur was buying all
          along.
        </p>
        <p>
          <strong>Higher load factors via fastrange.</strong> The fast C++ tables (abseil, F14, hashbrown)
          run load factor 0.875; FSet runs 0.5. Copy them? Measured: a big loss. Their high load factor and
          their SIMD group probing are a <em>package</em> — the SIMD tolerates the crowded table — and the
          SIMD half is exactly what died in the previous experiment. Adopting half a recipe means longer
          probe chains with nothing to absorb them.
        </p>
        <p>
          What survived is small and precise: a <strong>capacity-banded hash mix</strong>. Tiny tables keep
          the full avalanche; mid-band tables (2¹⁰–2¹⁵) use a cheap Fibonacci fold that doubled String hit
          throughput at 1000; large tables use a nonlinear xorshift-first mix worth another 10–14% at 100k.
          The lever was the hash function's cost, never the probe layout. And with inlining explicitly ruled
          out too (the probe fully inlines, <code>String.equals</code> devirtualized — verified via Graal's
          inlining traces), the remaining scattered-String gap against chained mutable tables is simply the
          execution profile of two flat arrays versus pointer chains under random access. That's the whole
          tax. Everything else on this page is what it bought.
        </p>
      </section>

      <section className="ex">
        <h2>Where it lands</h2>
        <p>
          The same leaderboard treatment as FArray's: every operation at every size, each structure scored by
          the geometric mean of its distance from the fastest-in-cell. The bet is visible right in the table
          — the scattered-probe and size-16 cells documented above go to the specialists, and everything
          structural — build, merge, equality, transforms, the algebra itself — goes to FSet, usually by
          multiples.
        </p>
        <Scorecard suite="fset" />
        <p>
          The full record — every chart, every competitor, with the exact <code>@Benchmark</code> source
          behind each card — is on the <a href="#/setbench">FSet benchmarks page</a>.
        </p>
      </section>

      <footer className="page__foot">
        <a className="page__next" href="#/setbench">The FSet benchmarks — the complete scorecard →</a>
        <p>
          Charts are this commit's <code>docs/set-bench-results.json</code>; snippets are extracted verbatim
          from the generated <code>FSetOps</code> and the hand-written <code>FSet.scala</code>. Nothing on
          this page is a mock-up.
        </p>
      </footer>
    </article>
  );
}
