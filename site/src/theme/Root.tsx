import type { ReactNode } from "react";
import { DataProvider } from "../data/store";
import "@fontsource-variable/jetbrains-mono";
import "@fontsource-variable/source-serif-4";

// Wraps the entire app (Docusaurus theme hook) so every doc page can reach the bench data,
// snippets and @Benchmark sources through useStore().
export default function Root({ children }: { children: ReactNode }) {
  return <DataProvider>{children}</DataProvider>;
}
