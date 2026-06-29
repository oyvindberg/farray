import { createContext, useContext, useEffect, useMemo, useState, type ReactNode } from "react";
import { buildCharts, buildScorecard, type Chart, type Scorecard, type Slim } from "./bench";

export interface SnippetData {
  name: string; file: string; lang: string;
  code: string; html: string;
  full: string | null; fullHtml: string | null;
  fullLabel?: string;
}

/** A single @Benchmark method's verbatim source, parsed at build time from the real .scala file. */
export interface BenchSource { code: string; html: string }
/** class name -> method name -> its source. Keyed exactly like the JMH benchmark identifiers. */
export type BenchSources = Record<string, Record<string, BenchSource>>;

interface Store {
  charts: Chart[];
  scorecard: Scorecard | null;
  snippets: Record<string, SnippetData>;
  benchSources: BenchSources;
  ready: boolean;
  error: string | null;
}

const Ctx = createContext<Store | null>(null);
const BASE = import.meta.env.BASE_URL; // "./" in build, "/" in dev — keeps fetch paths correct under base

export function DataProvider({ children }: { children: ReactNode }) {
  const [bench, setBench] = useState<Slim[] | null>(null);
  const [snippets, setSnippets] = useState<Record<string, SnippetData> | null>(null);
  const [benchSources, setBenchSources] = useState<BenchSources | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([
      fetch(`${BASE}data/bench.json`).then((r) => r.json()),
      fetch(`${BASE}data/snippets.json`).then((r) => r.json()),
      fetch(`${BASE}data/bench-sources.json`).then((r) => r.json()),
    ])
      .then(([b, s, bs]) => { setBench(b); setSnippets(s); setBenchSources(bs); })
      .catch((e) => setError(String(e)));
  }, []);

  const value = useMemo<Store>(() => {
    const charts = bench ? buildCharts(bench) : [];
    return {
      charts,
      scorecard: bench ? buildScorecard(charts) : null,
      snippets: snippets ?? {},
      benchSources: benchSources ?? {},
      ready: !!bench && !!snippets && !!benchSources,
      error,
    };
  }, [bench, snippets, benchSources, error]);

  return <Ctx.Provider value={value}>{children}</Ctx.Provider>;
}

export function useStore(): Store {
  const s = useContext(Ctx);
  if (!s) throw new Error("useStore must be used within <DataProvider>");
  return s;
}
