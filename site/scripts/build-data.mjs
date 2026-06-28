#!/usr/bin/env node
/**
 * Produces the two JSON files the SPA consumes, into site/public/data/:
 *
 *   bench.json     — the JMH results slimmed to {b:benchmark, p:params, s:score}.
 *                    The raw docs/bench-results.json is ~7.8 MB of percentiles + raw samples;
 *                    the report only ever needs name + params + primary score, so we drop the rest.
 *
 *   snippets.json  — every `//start:NAME … //stop:NAME` region found in the real, compiled source
 *                    (farray / codegen / benchmarks / tests). Snippets are extracted, never typed —
 *                    if it is in the page, it compiles. Keyed by NAME -> {name, file, lang, code, full}.
 *
 * Dev: re-run `npm run data` to refresh without restarting Vite. Prod: `vite build` bakes public/ into dist/.
 */
import { readFileSync, writeFileSync, mkdirSync, readdirSync, statSync } from "node:fs";
import { dirname, resolve, relative, extname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createHighlighter } from "shiki";

// Highlight at BUILD time with Shiki (TextMate grammars -> accurate Scala/Java). Theme tuned to the
// page's deep code island; the resulting HTML ships in snippets.json, so the client highlights nothing.
const THEME = "vitesse-dark";
const highlighter = await createHighlighter({ themes: [THEME], langs: ["scala", "java"] });
const hl = (code, lang) =>
  code == null ? null : highlighter.codeToHtml(code, { lang, theme: THEME });

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const OUT = resolve(HERE, "../public/data");
mkdirSync(OUT, { recursive: true });

// ---------------------------------------------------------------- bench.json
function buildBench() {
  const raw = JSON.parse(readFileSync(resolve(REPO, "docs/bench-results.json"), "utf8"));
  const slim = [];
  let skipped = 0;
  for (const b of raw) {
    const pm = b.primaryMetric;
    if (!pm || pm.scoreUnit !== "ops/s") {
      skipped++;
      continue;
    }
    slim.push({ b: b.benchmark, p: b.params || {}, s: pm.score });
  }
  writeFileSync(resolve(OUT, "bench.json"), JSON.stringify(slim));
  console.log(`bench.json: ${slim.length} entries (${skipped} non-ops/s skipped)`);
}

// ---------------------------------------------------------------- snippets.json
const SOURCE_ROOTS = ["farray/src", "codegen/src", "benchmarks/src", "tests/src"];
const LANG = { ".scala": "scala", ".java": "java" };
const START = /\/\/\s*start:([\w.-]+)/;
const STOP = /\/\/\s*stop:([\w.-]+)/;
const ANY_MARKER = /\/\/\s*(?:start|stop):[\w.-]+/;

function walk(dir, acc) {
  for (const name of readdirSync(dir)) {
    const p = join(dir, name);
    const st = statSync(p);
    if (st.isDirectory()) walk(p, acc);
    else if (LANG[extname(p)]) acc.push(p);
  }
  return acc;
}

function dedent(lines) {
  let min = Infinity;
  for (const l of lines) {
    if (!l.trim()) continue;
    min = Math.min(min, l.match(/^(\s*)/)[1].length);
  }
  if (min === Infinity) min = 0;
  return lines.map((l) => l.slice(min)).join("\n").replace(/^\n+|\n+$/g, "");
}

function buildSnippets() {
  const files = [];
  for (const root of SOURCE_ROOTS) {
    try {
      walk(resolve(REPO, root), files);
    } catch {
      /* root may not exist; skip */
    }
  }
  const out = {};
  for (const file of files) {
    const text = readFileSync(file, "utf8");
    const lines = text.split("\n");
    const rel = relative(REPO, file);
    const lang = LANG[extname(file)];
    // "show entire file" only makes sense for human-sized files — skip dumping huge generators
    const full = lines.length <= 400 ? dedent(lines.filter((l) => !ANY_MARKER.test(l))) : null;
    // collect regions; supports nested/overlapping by tracking open starts
    const open = new Map(); // name -> collecting lines
    for (const line of lines) {
      const s = line.match(START);
      const e = line.match(STOP);
      if (s) {
        open.set(s[1], []);
        continue;
      }
      if (e) {
        const name = e[1];
        const body = open.get(name);
        if (body === undefined) throw new Error(`${rel}: //stop:${name} with no matching //start`);
        open.delete(name);
        if (out[name]) throw new Error(`duplicate snippet name "${name}" (${out[name].file} and ${rel})`);
        const code = dedent(body);
        out[name] = { name, file: rel, lang, code, html: hl(code, lang), full, fullHtml: hl(full, lang) };
        continue;
      }
      for (const body of open.values()) body.push(line);
    }
    for (const name of open.keys()) throw new Error(`${rel}: //start:${name} with no matching //stop`);
  }

  // golden snapshot files: the post-typer lowering of fused pipelines, checked into git by the tests.
  // Ingested whole (they are pure generated code, no markers) so the page can show what `.fuse` emits.
  const GOLDENS = [
    { name: "fuse-generated", file: "tests/snapshots/fuse-long-pipeline.snap" },
    { name: "fuse-collect-generated", file: "tests/snapshots/fuse-collect-zip.snap" },
  ];
  for (const g of GOLDENS) {
    const path = resolve(REPO, g.file);
    const code = readFileSync(path, "utf8").replace(/^\n+|\n+$/g, "");
    out[g.name] = { name: g.name, file: g.file, lang: "scala", code, html: hl(code, "scala"), full: null, fullHtml: null };
  }

  writeFileSync(resolve(OUT, "snippets.json"), JSON.stringify(out, null, 2));
  console.log(`snippets.json: ${Object.keys(out).length} snippets [${Object.keys(out).join(", ")}]`);
}

buildBench();
buildSnippets();
