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
import { readFileSync, writeFileSync, mkdirSync, readdirSync, statSync, existsSync } from "node:fs";
import { dirname, resolve, relative, extname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createHighlighter } from "shiki";

// Highlight at BUILD time with Shiki. Dual-theme: each token carries both --shiki-light and --shiki-dark,
// and the page's [data-theme] picks which to show, so code follows the site's light/dark mode.
const THEMES = { light: "vitesse-light", dark: "vitesse-dark" };

// Schematic of the core, faithful to the real FBase `permits` clause but collapsed across element kinds.
const NODE_TREE = `// An FArray[A] is one of these. Each carrying-data node exists once per element
// kind (Int, Long, Double, … , Ref) — generated, not hand-written.
sealed abstract class FBase

// leaves — the data actually lives here:
final class IntArr(arr: Array[Int])                       extends FBase  // a real int[]  (also Long/Double/…/Ref)
final class IntOne(value: Int)                            extends FBase  // a single element
case object Empty                                         extends FBase

// lazy structural nodes — O(1) to build; they just point at their children:
final class Concat(left: FBase, right: FBase)             extends FBase  // ++
final class IntAppend(base: FBase, elem: Int)             extends FBase  // :+
final class IntPrepend(elem: Int, base: FBase)            extends FBase  // +:
final class SliceNode(base: FBase, off: Int, len: Int)    extends FBase  // take / drop / slice
final class ReverseNode(base: FBase)                      extends FBase  // reverse
final class IntPad(base: FBase, fill: Int, len: Int)      extends FBase  // padTo
final class IntUpdated(base: FBase, i: Int, elem: Int)    extends FBase  // updated
final class RangeNode(start: Int, step: Int, count: Int)  extends FBase  // FArray.range — elements never built`;

// Extract a brace-balanced `def` verbatim from generated source, starting at the line matching `sigRe`.
function extractBraceDef(text, sigRe) {
  const lines = text.split("\n");
  const i = lines.findIndex((l) => sigRe.test(l));
  if (i < 0) throw new Error(`generated def not found: ${sigRe}`);
  let depth = 0, started = false;
  const out = [];
  for (let j = i; j < lines.length; j++) {
    out.push(lines[j]);
    for (const ch of lines[j]) {
      if (ch === "{") { depth++; started = true; }
      else if (ch === "}") depth--;
    }
    if (started && depth === 0) break;
  }
  return dedent(out);
}
const highlighter = await createHighlighter({ themes: [THEMES.light, THEMES.dark], langs: ["scala", "java"] });
const hl = (code, lang) =>
  code == null ? null : highlighter.codeToHtml(code, { lang, themes: THEMES, defaultColor: false });

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

// Strip the opaque-type `$proxy` bookkeeping (provably dead-code-eliminated — the golden header says so),
// so the default view shows the loop, not the encoding. The verbatim version stays behind the toggle.
function stripProxy(code) {
  const lines = code.split("\n");
  const kept = [];
  for (let i = 0; i < lines.length; i++) {
    const t = lines[i].trim();
    if (/^val [\w$]*\$proxy: FArray\$package \{$/.test(t)) {
      while (i < lines.length && lines[i].trim() !== "}]") i++; // skip the multiline proxy-type decl
      continue;
    }
    if (/^val FArray\$package\$_this/.test(t)) continue;
    if (/^val \w*\$proxy:.*\$asInstanceOf\$/.test(t)) continue; // `val xs$proxy = ints.$asInstanceOf$[…]`
    kept.push(lines[i]);
  }
  let s = kept.join("\n");
  s = s.replace(/(\w+)\$proxy/g, "$1"); // xs$proxy -> xs, a$proxy -> a
  s = s.replace(/\.\$asInstanceOf\$\[(?:[^[\]]|\[[^\]]*\])*\]/g, ""); // drop opaque-boundary casts (1-level nesting)
  s = s.replace(/\(\(\{/g, "{").replace(/\}: FBase\): FArray\[\w+\]\)/g, "}"); // collapse the wrapper ascription
  let out = dedent(s.split("\n")).replace(/\n{3,}/g, "\n\n");
  // drop a redundant outermost brace pair if everything is wrapped in one
  const ol = out.split("\n");
  if (ol[0].trim() === "{" && ol[ol.length - 1].trim() === "}") out = dedent(ol.slice(1, -1));
  return out.trim();
}

// Reduce the 81-branch dispatch to one representative path, shown verbatim with honest elisions: the
// matching-kind fast path (-> mapLeafIntInt), one cross-kind loop, and the Ref path.
function firstOuterBranch(code) {
  const L = code.split("\n");
  const find = (re) => L.find((l) => re.test(l));
  return [
    L[0], // signature + outer `summonFrom {`
    find(/case r: IntRepr\[A\] => summonFrom \{/),
    find(/case rb: IntRepr\[B\] =>/), //  Int -> Int : the static leaf method
    find(/case rb: LongRepr\[B\] =>/), // Int -> Long: a generic out-loop (one per mismatched kind)
    "          // … DoubleRepr[B] … BooleanRepr[B]: the same loop, one per result kind",
    find(/case rb: RefRepr\[B\] =>/),
    "        }",
    "    // … LongRepr[A], DoubleRepr[A], … RefRepr[A]: eight more input kinds — 81 branches in all",
    "    case _ => /* friendly \"specialize, or fail to compile\" error */",
    "  }",
    "}",
  ].join("\n");
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
    { name: "map-generated", file: "tests/snapshots/map-inline.snap" },
  ];
  for (const g of GOLDENS) {
    const path = resolve(REPO, g.file);
    const raw = readFileSync(path, "utf8").replace(/^\n+|\n+$/g, "");
    if (g.name === "map-generated") {
      // default to the proxy-stripped loop; the verbatim expansion is one toggle away
      const short = stripProxy(raw);
      out[g.name] = {
        name: g.name, file: g.file, lang: "scala",
        code: short, html: hl(short, "scala"), full: raw, fullHtml: hl(raw, "scala"), fullLabel: "raw expansion",
      };
    } else {
      out[g.name] = { name: g.name, file: g.file, lang: "scala", code: raw, html: hl(raw, "scala"), full: null, fullHtml: null };
    }
  }

  // Schematic illustrations — not extracted from a single file (the real hierarchy is ~60 generated Java
  // classes, one per shape × element kind), so they carry a label instead of a path. Kept faithful to the
  // real FBase `permits` clause, collapsed across element kinds.
  const ILLUSTRATIONS = {
    "node-tree": {
      file: "the core — schematic (the real hierarchy is generated Java, one final class per shape × kind)",
      lang: "scala",
      code: NODE_TREE,
    },
  };
  for (const [name, v] of Object.entries(ILLUSTRATIONS)) {
    out[name] = { name, file: v.file, lang: v.lang, code: v.code, html: hl(v.code, v.lang), full: null, fullHtml: null };
  }

  // Real op source extracted VERBATIM from the generated FArrayOps, so the dispatch shown on the page is
  // exactly what compiles — no hand-written stand-in. Requires a prior farray build (.bleep present).
  const GEN_OPS = resolve(REPO, ".bleep/generated-sources/farray/farray.GenCores/farray/FArrayOps.scala");
  if (!existsSync(GEN_OPS)) throw new Error(`generated FArrayOps not found at ${GEN_OPS} — build farray first (e.g. \`bleep compile farray\`)`);
  const genOps = readFileSync(GEN_OPS, "utf8");
  const EXTRACTS = [
    { name: "map-dispatch-real", sig: /inline def mapImpl\[A, B\]/, file: "farray/FArrayOps.scala · generated · inline def mapImpl" },
    { name: "map-leaf", sig: /^\s*def mapLeafIntInt\(/, file: "farray/FArrayOps.scala · generated · def mapLeafIntInt" },
  ];
  for (const e of EXTRACTS) {
    const code = extractBraceDef(genOps, e.sig);
    if (e.name === "map-dispatch-real") {
      const short = firstOuterBranch(code);
      out[e.name] = {
        name: e.name, file: e.file, lang: "scala",
        code: short, html: hl(short, "scala"), full: code, fullHtml: hl(code, "scala"), fullLabel: "all 81 branches",
      };
    } else {
      out[e.name] = { name: e.name, file: e.file, lang: "scala", code, html: hl(code, "scala"), full: null, fullHtml: null };
    }
  }

  writeFileSync(resolve(OUT, "snippets.json"), JSON.stringify(out, null, 2));
  console.log(`snippets.json: ${Object.keys(out).length} snippets [${Object.keys(out).join(", ")}]`);
}

buildBench();
buildSnippets();
