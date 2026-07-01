import Snippet from "../components/Snippet";

export default function Syntax() {
  return (
    <section className="chapter" id="syntax">
      <h2>The Scala on top</h2>

      <p className="lede">
        The Java core is fast but almost <strong>untyped</strong>: to it, every value is just an{" "}
        <code>FBase</code> — a primitive array under a few structural wrappers, with no notion of whether it
        holds <code>Int</code>s or <code>String</code>s. All the typing arrives one layer up, in a sheet of
        Scala 3 so thin that most of it isn't there at runtime. Three features carry it: the opaque type,
        extension methods, and <code>inline</code>.
      </p>

      <p>
        An <strong>opaque type</strong> gives <code>FArray[A]</code> its own identity at compile time while{" "}
        <em>being</em> a plain <code>FBase</code> at runtime — no wrapper object, no allocation, nothing to
        escape-analyze away. The whole API then hangs off it as <strong>extension methods</strong>, and they
        come in two flavours. The difference between them is the whole story of the next section:
      </p>

      <Snippet name="surface" />

      <p>
        The <strong>structural</strong> ones — <code>reverse</code>, <code>++</code> — just call the matching{" "}
        <code>FBase</code> method. Because an <code>FArray</code> already <em>is</em> its core, that's a
        direct call into the lazy O(1) node-builders from the last section; nothing clever, nothing
        specialized.
      </p>

      <p>
        The interesting ones are marked <code>inline</code>, and they take their lambda <code>inline</code>{" "}
        too — <code>inline def map[B](inline f: A =&gt; B)</code>. At every concrete call site the method body
        is spliced in, your lambda with it, and the whole thing forwards to a generated <code>…Impl</code>.
        That forward is where the unboxing gets decided — but hold that thought. First, the split those two
        flavours create, because the entire cost model hangs off it.
      </p>
    </section>
  );
}
