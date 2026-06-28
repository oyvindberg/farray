import { DataProvider, useStore } from "./data/store";
import Intro from "./chapters/Intro";
import Core from "./chapters/Core";

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
        compiles.
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
      compiled source. If it's on the page, it ran.
    </footer>
  );
}

export default function App() {
  return (
    <DataProvider>
      <div className="wrap">
        <Hero />
        <Status />
        <main>
          <Intro />
          <Core />
        </main>
        <Footer />
      </div>
    </DataProvider>
  );
}
