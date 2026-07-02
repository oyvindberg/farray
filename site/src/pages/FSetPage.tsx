import Snippet from "../components/Snippet";
import GeneratedCode from "../components/GeneratedCode";
import BenchChart from "../components/BenchChart";
import BenchPair from "../components/BenchPair";
import Scorecard from "../components/Scorecard";

export default function FSetPage() {
  return (
    <article className="page">
      <header className="page__head">
        <p className="page__eyebrow">the second collection</p>
        <h1>FSet</h1>
        <p className="page__lede">
          An immutable, unboxed set on the FArray playbook: a generated Java core, per-kind specialization,
          <code>inline</code> dispatch — and one contrarian bet. Every immutable set on the JVM pays the
          persistent-trie tax to make updates cheap. FSet refuses to pay it, and builds an immutable set the
          way you actually use one: <strong>frozen flat leaves, queried hard, combined lazily.</strong> As
          everywhere on this site: every bar is a JMH measurement you can hover, every snippet is extracted
          from source that compiles.
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
          rebuilds tree structure. And the workload most immutable sets actually live is the opposite one —
          <strong> build once, then query and combine a lot</strong>.
        </p>
        <p>
          So FSet inverts the trade. A materialized set is a <strong>frozen flat leaf</strong> — a packed
          sorted primitive array, a frozen open-addressing hash table, a dense bitmap, or a 16-byte range —
          with no tree to hop and nothing boxed. And the persistent operations don't rebuild anything at
          all: <code>∪ ∩ ∖ ⊕</code>, <code>+</code> and <code>-</code> are <strong>O(1) lazy nodes</strong>{" "}
          that share both operands. Immutability makes the leaves <em>better</em> than their mutable
          counterparts — a table that will never grow again can be laid out for pure probe speed — and makes
          the algebra free.
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
          the special case, not the concept. A set is anything you can ask membership of — and FSet models
          that as a small lattice of <em>capabilities</em>, using real opaque-type subtyping (an opaque
          type's bound can be another opaque type; zero wrappers anywhere):
        </p>
        <Snippet name="set-lattice" hideFull />
        <p>
          Membership and the algebra live at the top and are inherited by everything below. Traversals and
          transforms — <code>foreach</code>, <code>filter</code>, <code>map</code>, <code>flatMap</code> —
          live on <code>FSetFinite</code>: anything provably finite may enumerate (they materialize
          internally, memoized on the node), and since a transform builds a fresh materialized set anyway,
          demanding an explicit <code>.materialize</code> first would add a keyword but no information. The
          cheap observations and value equality — <code>size</code>, <code>iterator</code>,{" "}
          <code>sameElements</code> — need the frozen leaf itself and sit on <code>FSetMaterialized</code>.
          The compiler enforces the split: <code>(a ++ b).size</code> doesn't compile until you{" "}
          <code>.materialize</code>, and <code>FSet.above(5).size</code> doesn't compile at all.{" "}
          <code>FSetInfinite</code> is the honest outlier — an infinite set <em>is</em> a predicate, so it's
          contravariant like one, kept out of the invariant chain and bridged in by a <code>Conversion</code>.
          Ranges round it out: <code>FSet.range(0, 1_000_000_000)</code> is a finite, materializable set that
          costs 16 bytes.
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
          <code>contains</code> distributes over <code>∪ ∩ ∖ ⊕ ¬</code> as a Boolean homomorphism, so
          membership in a combined set never materializes anything.
        </p>
        <BenchPair
          suite="fset"
          int="IntSetContainsHitBenchmark"
          str="StrSetContainsHitBenchmark"
          caption={
            <>
              Point-lookup hits, Int (left) and String (right), against every serious JVM set — mutable ones
              included. The Int side races specialized primitive hash sets and both BitSets; the String side
              is where the F14 table below earns its keep.
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
          Honesty requires saying the quiet part: on scattered String point-reads at large n, the fastest
          mutable tables (<code>scala.mutable</code>, Guava) still win by 1.2–1.5× — a mutable table can
          re-organize on every insert, and String hashing gives a frozen layout nothing extra to exploit.
          That loss is measured, documented, and accepted; it buys the wins everywhere below. Sweep the
          mixed hit/miss probe chart above's siblings on the{" "}
          <a href="#/setbench">FSet benchmarks page</a> for the full picture.
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
          per instruction, cardinality by <code>popcount</code>. That's the entire reason a general-purpose
          immutable set can stand next to <code>immutable.BitSet</code> and RoaringBitmap in their own
          niche — while also being a set of Strings tomorrow.
        </p>
        <GeneratedCode name="set-bitmap-merge" summary="The word-parallel bitmap merge — ∪ ∩ ∖ ⊕ at 64 elements per op" />
        <BenchChart
          suite="fset"
          cls="IntSetMergeUnionBenchmark"
          caption={
            <>
              Union two sets and materialize the result. FSet's word-merge against Roaring, both BitSets,
              and the rebuild cost of every hash-based set.
            </>
          }
        />
      </section>

      <section className="ex">
        <h2>The algebra is lazy, and materialize is a merge</h2>
        <p>
          Because <code>a ++ b</code> is just a node, the natural way to use FSet is to <em>build an algebra
          and query it</em>. When you do need the elements, <code>.materialize</code> folds the tree into
          one leaf — iteratively, spine-first, never JVM recursion (a loop-built chain of ten thousand{" "}
          <code>+</code> nodes must not be a StackOverflow) — and <strong>memoizes</strong> the result on
          the node, so a shared subtree is merged once ever:
        </p>
        <Snippet name="set-union-node" hideFull />
        <GeneratedCode name="set-materialize" summary="Iterative left-spine materialize — pairwise union rounds, memoized per node" />
        <BenchChart
          suite="fset"
          cls="IntSetUnion3ContainsBenchmark"
          caption={
            <>
              The realistic composite: union three sets, then probe the result n times. FSet answers through
              the lazy tree; every competitor must eagerly build the union first.
            </>
          }
        />
        <BenchChart
          suite="fset"
          cls="StrSetUnion3MatContainsBenchmark"
          caption={
            <>
              The same shape for Strings, materialized before probing — build-once-query-many, which is the
              workload immutable sets actually live. At 100k FSet is 2× the fastest mutable table and 3×
              fastutil.
            </>
          }
        />
      </section>

      <section className="ex">
        <h2>References merge on cached hashes</h2>
        <p>
          The bulk algebra for reference elements has no bitmap to lean on, so FSet gives references their
          own unboxed column: every materialized Ref leaf carries its elements alongside a parallel{" "}
          <code>int[]</code> of cached hashes, kept <em>hash-sorted</em>. Union, intersection, difference
          and xor then become two-pointer merges over plain <code>int</code> comparisons — cache-sequential,
          allocation-tight, with <code>.equals</code> paid only inside a hash-collision run:
        </p>
        <Snippet name="set-merge-ref" hideFull />
        <p>
          The same two sorted hash columns make value equality embarrassingly fast: two pointers, int
          compares, done — no probing one set with the other's elements. <code>sameElements</code> on
          String sets is the clearest structural win in the whole suite:
        </p>
        <BenchChart
          suite="fset"
          cls="StrSetEqualsBenchmark"
          caption={
            <>
              Set equality on Strings. One merge-shaped pass over cached hashes versus probe-per-element:
              fastest at every size, 3× the mutable table and an order of magnitude over the immutable Set
              at 100k.
            </>
          }
        />
      </section>

      <section className="ex">
        <h2>Even map stays in bitmap-land</h2>
        <p>
          Transforms are where set semantics usually force a detour — collect values, sort, dedup, rebuild.
          FSet's <code>map</code> on an Int set instead word-scans the source bitmap and ORs each{" "}
          <code>f(e)</code> straight into a growable bitmap accumulator that re-bases on both ends. Dense
          in, dense out: no intermediate array, no sort, dedup for free — and if the output turns out
          sparse, the builder degrades once to the ordinary path and loses nothing.
        </p>
        <GeneratedCode name="set-bitmap-builder" summary="The growable bitmap accumulator behind map — dense outputs never leave bitmap form" />
        <BenchChart
          suite="fset"
          cls="IntSetMapBenchmark"
          caption={<>map an Int set through a function and rebuild the set — fused source scan into bitmap build.</>}
        />
        <p>
          <code>flatMap</code> gets the same treatment, split by write kind. Each element's result set is
          already a materialized leaf, so for Int the leaves fold together with pairwise word-OR union
          rounds; for references each part's raw element column is appended — a bulk{" "}
          <code>System.arraycopy</code>, no iterator, no boxing — and the deduplicating one-pass build runs{" "}
          <em>once</em> at the end. That last choice was measured, not assumed: the first cut merged Ref
          leaves pairwise too, re-allocating at every round, and lost to <code>immutable.Set</code>;
          append-once turned it into a win at every size.
        </p>
        <BenchPair
          suite="fset"
          int="IntSetFlatMapBenchmark"
          str="StrSetFlatMapBenchmark"
          caption={
            <>
              flatMap with a 2× expansion (<code>x → {"{x, x+1}"}</code> / <code>s → {"{s, s+\"x\"}"}</code>).
              fastutil has no flatMap, so its bar is the raw hand-written double-add loop — the strongest
              possible baseline, allocating no inner sets at all; FSet passes even that at 100k on both
              element kinds.
            </>
          }
        />
      </section>

      <section className="ex">
        <h2>Where it lands</h2>
        <p>
          The same leaderboard treatment as FArray's: every operation at every size, each structure scored
          by the geometric mean of its distance from the fastest-in-cell. The trade FSet makes is visible
          right in the table — a couple of point-read cells go to the mutable specialists, and everything
          structural (build, merge, equality, the algebra) goes the other way, often by multiples.
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
