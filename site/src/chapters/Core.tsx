import Snippet from "../components/Snippet";
import BenchChart from "../components/BenchChart";
import Complexity from "../components/Complexity";

export default function Core() {
  return (
    <section className="chapter" id="core">
      <h2>Paper cuts, and a Java core</h2>

      <p className="lede">
        Now the unglamorous part. Scala is a wonderful language to write and a frustrating one to make tight.
        Getting a method to stay static, getting a field read to be a field read, getting the compiler not to
        allocate a closure or a boxed <code>Integer</code> or an intermediate <code>Seq</code> — none of it is
        guaranteed, and most of it you find out by reading bytecode.
      </p>

      <p>
        After enough paper cuts you stop fighting. So the core data structures are written in{" "}
        <strong>Java</strong> — plain, <code>final</code>, fields-and-methods Java, where “this is a static
        call” and “this is a field access” mean exactly that. On top sits a thin Scala layer: the opaque type,
        the <code>inline</code> combinators, the API you actually touch.
      </p>

      <p>
        To keep that core tight, it's <strong>generated</strong>. No clever shared abstraction soaking up a
        virtual call — one hand-shaped class per node shape, per element kind. An <code>IntArr</code> and a{" "}
        <code>LongArr</code> and a <code>DoubleArr</code> and a <code>RefArr</code>; an <code>IntAppend</code>,
        a <code>LongAppend</code>, on and on. Massive, deliberate duplication: every class stays monomorphic
        and allocates nothing it doesn't have to. It's all straightforward — there's just a lot of it.
      </p>

      <Snippet name="node-tree" hideFull />

      <p>
        Two kinds of node. <strong>Leaves</strong> — a <code>{"${K}Arr"}</code>, a genuine primitive array —
        hold the data. Everything else is a <strong>lazy structural node</strong> that just points at its
        children: <code>Concat</code> for <code>++</code>, <code>Append</code>/<code>Prepend</code> for{" "}
        <code>:+</code>/<code>+:</code>, <code>SliceNode</code> for <code>take</code>/<code>drop</code>. Building
        one is O(1) and copies nothing.
      </p>

      <h3>Build it however you like</h3>

      <p>
        This is the trade. A bare array is a hair quicker at a single indexed read — one type-check that{" "}
        <code>int[]</code> doesn't carry. In exchange it can't even compare or print itself:{" "}
        <code>IArray(1, 2, 3).toString</code> is <code>[I@5a07e868</code> and <code>==</code> is identity.
        FArray wraps the array in an object, so it gives that hair back and gets value <code>equals</code>,{" "}
        <code>hashCode</code> and <code>toString</code> — and a collection you can assemble any way you please,
        in constant time, paying for a flat array only when you ask for one.
      </p>

      <Complexity />

      <p>
        The reason that table is almost all <code>O(1)</code> is that the nodes are <strong>lazy</strong>: a{" "}
        <code>Concat</code> or a <code>SliceNode</code> records an intention, it doesn't do the work. Nothing is
        computed until you read. So if you <code>++</code> twenty arrays together and then call{" "}
        <code>take(2)</code>, the traversal walks two elements into the tree and stops — the other nineteen
        concatenations are never constructed into anything. That's a lot of collections you simply don't build.
        You pay for what you read, not for what you wrote.
      </p>

      <p>
        And what you do pay <strong>amortizes</strong>. Cons a million elements on one at a time and you've
        built a million-deep tree of O(1) nodes; the first real traversal — a <code>map</code>, a{" "}
        <code>sum</code>, a <code>foldLeft</code> — flattens the whole thing into one contiguous primitive array
        in a single pass, and every read after that is array-speed. The array is the preferred shape, and it
        reasserts itself the moment you compute.
      </p>

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

      <BenchChart
        cls="ListLikeScalingIntBenchmark"
        caption="FArray vs List, used identically as a cons-list — built by ::, summed and mapped by recursion. Swept to 100k. Tie on sum, win on build and mapSum."
      />
    </section>
  );
}
