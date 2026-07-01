import Snippet from "../components/Snippet";
import BenchChart from "../components/BenchChart";
import BenchPair from "../components/BenchPair";

export default function FlatMap() {
  return (
    <section className="chapter" id="inside-flatmap">
      <h2>flatMap, the op that fights back</h2>

      <p className="lede">
        <code>map</code> is a friendly contract: one element in, one element out, output length known before
        the loop starts. <code>flatMap</code> promises nothing. Every element produces a whole collection —
        of any size, including zero — so the output length is unknowable up front, and each inner collection
        is an allocation <em>somebody</em> already paid for. The naive move, and it's what most libraries do,
        is to copy every inner into a growing buffer and throw the inners away. FArray tries to be smarter
        twice: once about the copying, and once about the JIT.
      </p>

      <h3>Don't copy what you can keep</h3>

      <p>
        The inner collections <code>f</code> returns are already backed by arrays — FArray arrays. Copying
        them into one big buffer discards perfectly good, already-materialized storage. So{" "}
        <code>flatMap</code> looks at the <em>first</em> inner it produces and makes a bet. If inners are{" "}
        <strong>wide</strong>, it doesn't copy at all: it keeps each inner's backing array as a{" "}
        <em>segment</em> and wraps them in a dedicated node — an array of arrays plus a prefix-sum index, one
        more citizen of the lazy core:
      </p>

      <Snippet name="node-tree" hideFull />

      <p>
        Traversal walks the segments back to back; indexed access binary-searches the offsets. If inners are{" "}
        <strong>narrow</strong>, the bet flips: a two-element segment costs more in pointer-chasing than it
        saves in copying, so narrow inners are flattened into one contiguous leaf the classic way. The
        crossover is measured, not guessed — and it's different per element kind (references keep the node
        from width 8; primitives, whose flat arrays vectorize so well that locality is worth more, only from
        width 32). Tiny inners don't even use <code>System.arraycopy</code>: below eight elements the stub
        call costs more than the copy, so they move with a plain loop.
      </p>

      <Snippet name="flatmap-flat" hideFull />

      <h3>The chapter where the JIT writes the plot twist</h3>

      <p>
        The first version of this op didn't look like that. It was <em>fully inline</em> — the whole
        node-or-flatten loop, both paths, spliced into your method at every <code>flatMap</code> call site,
        the way <code>.fuse</code> does deliberately. And in isolation it was excellent: with the loop and
        your lambda in one compilation unit, Graal's escape analysis deleted the per-element inner{" "}
        <code>FArray(a, b)</code> wrappers entirely. Benchmarks looked great. Then we appended one method
        call.
      </p>

      <p className="codeline">
        <code>xs.flatMap(s =&gt; FArray(s, s)).take(n)</code>
      </p>

      <p>
        That chain ran <strong>35× slower</strong> than the same flatMap alone. Not a measurement glitch —
        74 ops/s where a bare array managed 1,400, reproducibly, and only when a structural call like{" "}
        <code>take</code>, <code>drop</code> or <code>reverse</code> trailed the loop <em>in the same
        method</em>. Method size was innocent (a double filter, much bigger, was fine). Deoptimization logs
        were empty. The tell was allocation: the healthy chain allocated 3.2&nbsp;MB per op, the poisoned one
        7.2&nbsp;MB — escape analysis had silently given up on the spliced loop, and every per-element wrapper
        it used to delete became real garbage on a slow path. The fastest code in the library was one virtual
        call away from being the slowest, and the trigger was something any user would write in their first
        five minutes.
      </p>

      <p>
        The fix is the same shape <code>map</code> already taught us: stop splicing the loop, share it. The
        inline surface now resolves kinds at compile time, wraps your lambda and the unboxed source read into
        one specialized SAM, and calls a driver that is compiled <em>once</em>, in its own method, whose
        compilation the caller cannot poison:
      </p>

      <Snippet name="flatmap-shared" hideFull />

      <p>
        Because the inners are <code>FArray</code>s regardless of element type, one driver per <em>output</em>{" "}
        kind covers all eighty-one kind combinations — the entry is small enough for the JIT to inline into a
        hot caller (recovering the escape analysis when it's profitable), and when it doesn't, the loop still
        compiles well instead of catastrophically. The poisoned chain went from 74 ops/s to beating the bare
        array; plain <code>flatMap</code> got faster too, because "compiles well unconditionally" beats
        "compiles brilliantly if the caller cooperates".
      </p>

      <BenchChart
        cls="FlatMapFilterTakeStrBenchmark"
        caption="flatMap → filter → take, the chain that used to collapse. The trailing take is O(1) — a lazy SliceNode — so FArray does strictly less work than anyone that materializes the prefix, and with the shared driver it now leads the field at every size. This exact cell measured 0.055× of IArray before the rewrite."
      />

      <p>And the op on its own, Int beside String — the honest baseline:</p>

      <BenchPair
        int="FlatMapIntBenchmark"
        str="FlatMapStrBenchmark"
        caption="flatMap(x => FArray(x, x + 1)) then a fold, Int beside String. Narrow inners, so this is the flatten path: FArray runs ~1.5–2× past IArray and the boxing collections on Int, and leads or ties on String. Wide inners (the segment node) win bigger on allocation — the node stores the inner arrays instead of copying them."
      />

      <p>
        One honest asterisk: a <em>chain</em> of flatMaps with tiny inners pays the SAM hand-off more than
        once, and sits a few percent behind where the old spliced loop did on its best day. That's the same
        trade <code>map</code> made — and it's precisely the pipeline shape <code>.fuse</code> exists for,
        where the whole chain becomes one loop and the question disappears.
      </p>
    </section>
  );
}
