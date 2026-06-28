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
        This is the trade. A bare array would be a hair quicker at a couple of things — a single indexed read
        carries one type-check that <code>int[]</code> doesn't. In return you get a collection you can assemble
        any way you please, in constant time, and you only pay for a flat array when you ask for one.
      </p>

      <Complexity />

      <p>
        The array is the preferred shape, and it reasserts itself the moment you compute: run a <code>map</code>{" "}
        or a <code>filter</code> and the whole lazy tree collapses back into one contiguous primitive array. You
        can cons a million elements on one at a time, <code>map</code> once, and you're holding an{" "}
        <code>int[]</code> again.
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

      <h3>Why not just IArray?</h3>

      <p>
        One last reason the array has to be wrapped at all. A collection has to be able to compare and print
        itself — <code>a == b</code> by contents, a <code>toString</code> you can read, a <code>hashCode</code>{" "}
        that agrees with <code>List</code>'s. A bare array can't, and that's exactly what <code>IArray</code> is:{" "}
        <code>IArray(1, 2, 3).toString</code> is <code>[I@5a07e868</code>, and <code>==</code> compares
        identities. FArray's nodes each override <code>equals</code>, <code>hashCode</code> and{" "}
        <code>toString</code>, so it behaves like a value — checked element-for-element against <code>List</code>{" "}
        in the tests. Unboxed <em>and</em> a real collection; you don't have to choose.
      </p>
    </section>
  );
}
