import { useStore } from "./data/store";
import Intro from "./chapters/Intro";
import Core from "./chapters/Core";
import Syntax from "./chapters/Syntax";
import Combinators from "./chapters/Combinators";
import Inline from "./chapters/Inline";
import Closing from "./chapters/Closing";

function Hero() {
  return (
    <header className="hero">
      <h1 className="wordmark">FArray</h1>
      <p className="hero__lede">
        An immutable, unboxed Scala&nbsp;3 sequence with the whole <code>IndexedSeq</code> API — and a
        decade-old grudge against the heap, finally settled.
      </p>
      <p className="hero__sub">
        Every number here is a JMH measurement you can hover; every snippet is extracted from source that
        compiles. If it's on the page, it ran.
      </p>
    </header>
  );
}

function Status() {
  const { error, ready } = useStore();
  if (error) return <div className="status status--err">couldn't load benchmark data — {error}</div>;
  if (!ready) return <div className="status">loading measurements…</div>;
  return null;
}

function Footer() {
  return (
    <footer className="foot">
      Charts are this commit's <code>docs/bench-results.json</code>; snippets are extracted verbatim from
      compiled source; lowerings are golden tests, regenerated on every build. Nothing on this page is a
      mock-up.
    </footer>
  );
}

export default function MainStory() {
  return (
    <>
      <Hero />
      <Status />
      <main>
        <Intro />
        <Core />
        <Syntax />
        <Combinators />
        <Inline />
        <Closing />
      </main>
      <Footer />
    </>
  );
}
