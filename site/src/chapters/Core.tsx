import Snippet from "../components/Snippet";
import { RepresentationDiagram } from "../components/Diagrams";

export default function Core() {
  return (
    <section className="chapter" id="core">
      <h2>Paper cuts, and a Java core</h2>

      <p className="lede">
        To understand the speed you have to start at the bottom, with the data. And the data lives in Java —
        because Scala, wonderful as it is to write, cuts you in a hundred small ways when you need a method to
        stay static, a field read to be a field read, and nothing to allocate a closure or a boxed{" "}
        <code>Integer</code> behind your back. Most of those cuts you only ever discover by reading bytecode.
      </p>

      <p>
        So the core is plain, <code>final</code>, fields-and-methods Java, where “static call” and “field
        access” mean exactly what they say — and it's <strong>generated</strong>: one hand-shaped class per
        node shape, per element kind. No shared abstraction soaking up a virtual call — an <code>IntArr</code>{" "}
        and a <code>LongArr</code> and a <code>RefArr</code>, an <code>IntAppend</code> and a{" "}
        <code>LongAppend</code>, on and on. Deliberate, wholesale duplication so every class stays
        monomorphic. It's simple; there's just a lot of it.
      </p>

      <h3>The shape of an FArray</h3>

      <p>
        An <code>FArray</code> is a small tree of two kinds of node. <strong>Leaves</strong> hold the data — a{" "}
        <code>{"${K}Arr"}</code> is a genuine primitive array. Everything else is a <strong>lazy structural
        node</strong> that holds no data at all: it points at its children and records an intention. Building
        one is O(1) and copies nothing:
      </p>

      <RepresentationDiagram />

      <p>
        That's the whole trick of the structure, and every structural operation is one of these nodes:{" "}
        <code>Concat</code> for <code>++</code>, <code>Append</code>/<code>Prepend</code> for <code>:+</code>/
        <code>+:</code>, <code>SliceNode</code> for <code>take</code>/<code>drop</code>, <code>ReverseNode</code>{" "}
        for <code>reverse</code>. Here's the full cast — a flat listing, one class each:
      </p>

      <Snippet name="node-tree" hideFull />

      <p>
        Reading a tree is <em>one</em> direction-aware walk, shared by every operation: a forward driver and a
        backward driver, mirror images that swap roles at each <code>ReverseNode</code>, handing each leaf's
        backing array to the consumer in bulk — a <code>System.arraycopy</code> where the shapes line up. It's
        a single <code>final</code>, compiled-once method, so it never trips the JIT's huge-method cliff, and
        it flattens the tree into one contiguous array only when something genuinely demands one — before a
        sort, say.
      </p>

      <p>
        That's the entire Java layer: flat primitive arrays, a handful of lazy node shapes, and one walk that
        reads them. Fast, but almost untyped — to the core, everything is an <code>FBase</code>. Everything
        you actually <em>type</em>, and everything that stays unboxed, lives one layer up, in Scala.
      </p>
    </section>
  );
}
