import Snippet from "../components/Snippet";
import GeneratedCode from "../components/GeneratedCode";

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
        call” and “this is a field access” mean exactly that.
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

      <p>
        One more piece makes the core legible: how you <em>read</em> a tree. A single direction-aware
        traversal does it — a forward driver and a backward driver that mirror each other and swap roles
        at every <code>ReverseNode</code> — handing each leaf's backing array to the consumer in bulk (a{" "}
        <code>System.arraycopy</code> where the shapes line up). It's one <code>final</code>, compiled-once
        method, not a copy inlined at every call site, so it never trips the JVM's method-size limit.
        Materializing to a flat leaf happens only when something demands it — before a sort, say.
      </p>

      <p>
        Here is that walk made concrete, and it's worth seeing once because it pins down a subtlety that
        recurs later. The lambda-taking combinators — <code>map</code>, <code>fold</code>, the rest we'll get
        to — stay <code>inline</code> right up to this boundary. But the walk is the <em>one shared,
        compiled-once</em> method, so the lambda can't be inlined into it; it crosses in as a value instead — a
        SAM whose type we generate to match the traversal. For a <code>map</code> from <code>Int</code> to a
        reference type that's an <code>IntToRefFn</code> (<code>RO apply(int)</code>). The shared per-kind entry
        runs the simple shapes itself and hands a tree to the walker:
      </p>

      <Snippet name="map-leaf-ref" hideFull />

      <p>
        And the walker is the direction-aware traversal itself — an explicit stack, no per-node allocation, one
        branch per node shape. A <code>Concat</code> pushes its right child and goes left; a <code>Prepend</code> emits its
        element then continues down its base; an <code>Append</code> stashes a one-element node for later; a{" "}
        <code>ReverseNode</code> flips to the backward mirror, <code>buildBwdIntRef</code>. Expand it once, to
        watch a whole tree get read:
      </p>

      <GeneratedCode name="map-traverser" summary="buildFwdIntRef — the forward tree walk, every node shape" />

      <p>
        That's the whole of the Java. Flat primitive arrays, a handful of lazy node shapes, and one walk that
        reads them. Everything you actually <em>type</em> lives a layer up, in Scala — which is next.
      </p>
    </section>
  );
}
