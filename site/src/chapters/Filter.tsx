import Snippet from "../components/Snippet";
import BenchChart from "../components/BenchChart";

export default function Filter() {
  return (
    <section className="chapter" id="inside-filter">
      <h2>filter, and the branch that isn't there</h2>

      <p className="lede">
        <code>filter</code> is the simplest loop in the library — read, test, maybe keep — which makes it the
        clearest place to watch three small decisions compound. Here is the whole generated Int leaf, all
        three visible at once:
      </p>

      <Snippet name="filter-leaf-int" hideFull />

      <p>
        First, the exit: <code>if (o == n) xs</code>. A filter that kept everything returns{" "}
        <em>the same object</em> — reference identity, zero allocation. Filters that keep everything are
        common enough (validation passes, defensive cleanups) that this is free money. Second, the output
        buffer is allocated once at full size and handed over with its logical length — no trim copy — unless
        the filter kept under a quarter of the input, in which case it trims rather than pin four hundred
        kilobytes behind a two-element result.
      </p>

      <p>
        Third, and the interesting one: look at the write. There is no{" "}
        <code>if (p(e)) &#123; out(o) = e; o += 1 &#125;</code>. The element is stored{" "}
        <em>unconditionally</em>, and the predicate only decides whether the cursor moves — a rejected element
        is simply overwritten by the next one. That looks like extra work, and on paper it is. What it removes
        is the <strong>branch</strong>.
      </p>

      <p>
        A predictable predicate — keep everything over some threshold in sorted data, keep the evens of{" "}
        <code>0, 1, 2, 3…</code> — costs a branchy loop nothing; the predictor learns the pattern and the
        branch is free. But filters exist to make <em>data-dependent</em> decisions, and on genuinely
        unpredictable data every surprise costs a pipeline flush. The branchless form has no branch to
        mispredict — and better, with the loop body straight-line, the compiler vectorizes it. Measured on
        random ints, keep-the-evens: the branchy loop managed 3.3&nbsp;thousand ops/s at 100k elements; the
        branchless one does 21.5&nbsp;thousand — six and a half times — and on <em>predictable</em> data it's
        faster still, because a vector unit doesn't care whether the pattern was learnable.
      </p>

      <BenchChart
        cls="FilterIntBenchmark"
        caption="filter on RANDOM ints — keep the even half, so every element is a coin flip the branch predictor cannot learn. Everyone else pays a mispredict per surprise (or boxes, or both); FArray's branchless prim loop has no branch and vectorizes."
      />

      <p>
        And then the caveat that keeps this honest: the branchless trick is <em>primitive-only</em>, and not
        because references were forgotten. Storing a reference — even one about to be overwritten — isn't just
        a store: the garbage collector's write barrier fires on every one, rejected or not. We measured the
        branchless form on <code>String</code> and it lost by double digits. So the generator emits the
        branchless write for the eight primitive kinds and keeps the guarded write for references — the same
        per-kind specialization that unboxes elements also gets to pick a different loop shape per kind, which
        is not a card a generic <code>filter[A]</code> gets to play.
      </p>

      <BenchChart
        cls="FilterRandStrBenchmark"
        caption="The same coin-flip filter on reference elements — random-content strings, keep by hash parity. Here everyone runs the same guarded loop and everyone eats the same mispredicts; this is the field the branchy write was kept for."
      />

      <p>
        Same predicate shape, two different loops, each measured against the other's — which is the quiet
        thesis of the whole generator: there is no single best <code>filter</code>, only a best filter{" "}
        <em>per element kind</em>, and a library that resolves the kind at compile time gets to ship both.
      </p>
    </section>
  );
}
