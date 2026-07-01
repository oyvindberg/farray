import Snippet from "../components/Snippet";

export default function Syntax() {
  return (
    <section className="chapter" id="syntax">
      <h2>The Scala on top</h2>

      <p className="lede">
        The Java core is fast but almost <strong>untyped</strong>: to it, every value is just an{" "}
        <code>FBase</code> — a primitive array under a few structural wrappers, with no notion of whether it
        holds <code>Int</code>s or <code>String</code>s. All the typing is supplied one layer up, in a thin
        sheet of Scala 3 that, for the most part, isn't there at runtime. Three features carry it: the opaque
        type, extension methods, and <code>inline</code>.
      </p>

      <p>
        An <strong>opaque type</strong> gives <code>FArray[A]</code> its own identity at compile time while{" "}
        <em>being</em> a plain <code>FBase</code> at runtime — no wrapper object, no allocation. The whole API
        then hangs off it as <strong>extension methods</strong>. They come in two flavours, and the difference
        is the whole story of the next section:
      </p>

      <Snippet name="surface" />

      <p>
        The <strong>structural</strong> ones — <code>reverse</code>, <code>++</code> — just call the matching{" "}
        <code>FBase</code> method. Because an <code>FArray</code> already <em>is</em> its core, that's a direct
        call to the lazy, O(1) node-builder from the last section; nothing clever, nothing specialized.
      </p>

      <p>
        The interesting ones are marked <code>inline</code>, and they take their lambda <code>inline</code> too
        — <code>inline def map[B](inline f: A =&gt; B)</code>. At every concrete call site the method body is
        spliced in and your lambda with it, then the whole thing forwards to a generated <code>…Impl</code>.
        That forwarding is where the unboxing is decided — but hold that thought. First, the split those two
        flavours create, because the entire cost model hangs off it.
      </p>
    </section>
  );
}
