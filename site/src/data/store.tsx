import { createContext, useContext, useEffect, useMemo, useState, type ReactNode } from "react";
import { buildCharts, buildScorecard, type Chart, type Scorecard, type Slim } from "./bench";

export interface SnippetData {
  name: string; file: string; lang: string;
  code: string; html: string;
  full: string | null; fullHtml: string | null;
  fullLabel?: string;
}

interface Store {
  charts: Chart[];
  scorecard: Scorecard | null;
  snippets: Record<string, SnippetData>;
  ready: boolean;
  error: string | null;
}

const Ctx = createContext<Store | null>(null);
const BASE = import.meta.env.BASE_URL; // "./" in build, "/" in dev — keeps fetch paths correct under base

export function DataProvider({ children }: { children: ReactNode }) {
  const [bench, setBench] = useState<Slim[] | null>(null);
  const [snippets, setSnippets] = useState<Record<string, SnippetData> | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([
      fetch(`${BASE}data/bench.json`).then((r) => r.json()),
      fetch(`${BASE}data/snippets.json`).then((r) => r.json()),
    ])
      .then(([b, s]) => { setBench(b); setSnippets(s); })
      .catch((e) => setError(String(e)));
  }, []);

  const value = useMemo<Store>(() => {
    const charts = bench ? buildCharts(bench) : [];
    return {
      charts,
      scorecard: bench ? buildScorecard(charts) : null,
      snippets: snippets ?? {},
      ready: !!bench && !!snippets,
      error,
    };
  }, [bench, snippets, error]);

  return <Ctx.Provider value={value}>{children}</Ctx.Provider>;
}

export function useStore(): Store {
  const s = useContext(Ctx);
  if (!s) throw new Error("useStore must be used within <DataProvider>");
  return s;
}
