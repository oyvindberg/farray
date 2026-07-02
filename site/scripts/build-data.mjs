#!/usr/bin/env node
/**
 * Produces the JSON files the SPA consumes, into site/public/data/:
 *
 *   bench.json     — the JMH results slimmed to {b:benchmark, p:params, s:score}.
 *                    The raw docs/bench-results.json is ~7.8 MB of percentiles + raw samples;
 *                    the report only ever needs name + params + primary score, so we drop the rest.
 *   setbench.json  — the same slimming applied to docs/set-bench-results.json (the FSet suite).
 *
 *   snippets.json  — every `//start:NAME … //stop:NAME` region found in the real, compiled source
 *                    (farray / codegen / fset / gensets / benchmarks / tests). Snippets are extracted,
 *                    never typed — if it is in the page, it compiles. Keyed by NAME -> {name, file, lang, code, full}.
 *
 *   bench-sources.json / setbench-sources.json — every @Benchmark body, verbatim, per suite.
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
final class RangeNode(start: Int, step: Int, count: Int)  extends FBase  // FArray.range — elements never built
final class IntFlatMap(segs: Array[Array[Int]],           //  flatMap with wide inners — one segment per
                       offs: Array[Int])                  extends FBase  //  inner, their arrays kept, not copied`;

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

// ---------------------------------------------------------------- bench.json / setbench.json
function buildBench(srcRel, outName) {
  const raw = JSON.parse(readFileSync(resolve(REPO, srcRel), "utf8"));
  const slim = [];
  let skipped = 0;
  for (const b of raw) {
    const pm = b.primaryMetric;
    if (!pm) { skipped++; continue; }
    // Normalize to a higher-is-faster score. Throughput is already ops/s; SingleShotTime (the cold
    // benchmarks) reports time/op — invert it to invocations/sec so it charts like everything else.
    let s;
    const u = pm.scoreUnit;
    if (u === "ops/s") s = pm.score;
    else if (u === "s/op") s = 1 / pm.score;
    else if (u === "ms/op") s = 1e3 / pm.score;
    else if (u === "us/op") s = 1e6 / pm.score;
    else if (u === "ns/op") s = 1e9 / pm.score;
    else { skipped++; continue; }
    slim.push({ b: b.benchmark, p: b.params || {}, s });
  }
  writeFileSync(resolve(OUT, outName), JSON.stringify(slim));
  console.log(`${outName}: ${slim.length} entries (${skipped} non-ops/s skipped)`);
  return slim;
}

// ---------------------------------------------------------------- snippets.json
const SOURCE_ROOTS = ["farray/src", "codegen/src", "benchmarks/src", "tests/src", "fset/src", "gensets/src", "setbenchmarks/src", "fset-tests/src"];
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

// From a FuseDebug.show golden, keep just the `({ … })` block the macro actually emits — i.e. drop the
// `$proxy`/`Fuse_this` marker preamble (dead-code-eliminated at runtime) and show the loop.
function fuseLoop(code) {
  const lines = code.split("\n");
  const start = lines.findIndex((l) => l.trim() === "({");
  if (start < 0) return code;
  let depth = 0, started = false;
  const out = [];
  for (let j = start; j < lines.length; j++) {
    out.push(lines[j]);
    for (const ch of lines[j]) {
      if (ch === "{") { depth++; started = true; }
      else if (ch === "}") depth--;
    }
    if (started && depth === 0) break;
  }
  return dedent(out);
}

// Make a generated block read like normal Scala (ports farray.json.JsonDemo.clean's surface rewrites):
// method-form operators back to infix, drop backticks, `.apply(`→`(`, `.update(`→`.set(`, `x.unary_!`→`!x`.
function polish(s) {
  return s
    .replace(/`/g, "")
    .replaceAll(".+(", " + (").replaceAll(".-(", " - (").replaceAll(".*(", " * (").replaceAll(".%(", " % (")
    .replaceAll(".<(", " < (").replaceAll(".>(", " > (").replaceAll(".==(", " == (").replaceAll(".!=(", " != (")
    .replaceAll(".&&(", " && (").replaceAll(".||(", " || (")
    .replaceAll(".apply(", "(").replaceAll(".update(", ".set(")
    .replace(/(\w+)\.unary_!/g, "!$1");
}

// The JSON scanners nest a few dead wrapper blocks; mirror JsonDemo.clean — drop the proxy/placeholder
// preamble (down to the first real accumulator/statement), strip noise, balance, then polish.
function cleanJson(code) {
  const lines = code.split("\n");
  const isPreamble = (l) => {
    const t = l.trim().replace(/`/g, "");
    return t === "" || t === "{" || t === "({" ||
      t.startsWith("val NdjsonSource_this") || t.startsWith("val Fuse_this") ||
      t.includes("val src0: FBase = null") || t.includes("val n0: Int = 16") ||
      t.includes("$proxy") || t.includes("FArray$package") || t.startsWith("type FArray") || t === "}]";
  };
  let i = 0;
  while (i < lines.length && isPreamble(lines[i])) i++;
  const body = lines.slice(i).filter((l) => {
    const t = l.trim();
    return !(
      l.includes("$proxy") || l.includes("asInstanceOf$") || l.includes("FArray$package") ||
      t.startsWith("type FArray") || t === "}]" || t === "" ||
      l.includes("val src0: FBase = null") || l.includes("val n0: Int = 16")
    );
  }).map((l) => l
    .replace(/\.asInstanceOf\[[^\]]*(?:\[[^\]]*\])?\]/g, "")
    .replace(/inline\$(buf|until|from)\$i1\([^)]*\)/g, "src.$1"));
  // we dropped the outer wrapper openers, so the body has more closers than openers; pop the trailing
  // dangling closer-lines until it balances (keeps top-level accumulators and the real inner block).
  const net = (ls) => ls.join("").split("").reduce((d, c) => d + (c === "{" || c === "(" ? 1 : c === "}" || c === ")" ? -1 : 0), 0);
  while (net(body) < 0 && body.length) {
    const t = body[body.length - 1].trim();
    if (t[0] === "}" || t[0] === ")") body.pop();
    else break;
  }
  return polish(dedent(body));
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
      // A readable rendering: the inline desugaring of `_ + 1` collapsed to `v + 1`, and the dead bindings
      // the expansion leaves behind (`val n`, and the resolved-repr `val r`, neither of which is read on the
      // Int→Int leaf path) dropped — leaving the punchline. Note the loop is NOT here: `mapLeafIntInt` is a
      // plain (non-inline) call. The verbatim post-typer tree is one toggle away in `full`.
      const short = "mapLeafIntInt(xs, (v: Int) => v + 1)";
      out[g.name] = {
        name: g.name, file: g.file, lang: "scala",
        code: short, html: hl(short, "scala"), full: raw, fullHtml: hl(raw, "scala"), fullLabel: "raw expansion",
      };
    } else {
      out[g.name] = { name: g.name, file: g.file, lang: "scala", code: raw, html: hl(raw, "scala"), full: null, fullHtml: null };
    }
  }

  // The fuse-optimizer demos for the "intro to fusion" page: each shows the emitted loop by default
  // (the marker preamble stripped) with the verbatim expansion one toggle away.
  const FUSE_OPT = ["oneloop", "dce", "sink", "cse", "fold"];
  for (const key of FUSE_OPT) {
    const file = `tests/snapshots/fuse-opt-${key}.snap`;
    const raw = readFileSync(resolve(REPO, file), "utf8").replace(/^\n+|\n+$/g, "");
    const short = polish(fuseLoop(raw));
    out[`fuse-opt-${key}`] = {
      name: `fuse-opt-${key}`, file, lang: "scala",
      code: short, html: hl(short, "scala"), full: raw, fullHtml: hl(raw, "scala"), fullLabel: "full expansion",
    };
  }
  // The fused-JSON scanners for the "Fused JSON" page — the per-record byte scanner the macro emits,
  // cleaned to the runtime loop (proxy/placeholder noise dropped) with the verbatim expansion behind a toggle.
  const FUSE_JSON = ["sum", "cat", "count", "wide", "agg"];
  for (const key of FUSE_JSON) {
    const file = `tests/snapshots/fuse-json-${key}.snap`;
    const raw = readFileSync(resolve(REPO, file), "utf8").replace(/^\n+|\n+$/g, "");
    const short = cleanJson(raw);
    out[`fuse-json-${key}`] = {
      name: `fuse-json-${key}`, file, lang: "scala",
      code: short, html: hl(short, "scala"), full: raw, fullHtml: hl(raw, "scala"), fullLabel: "full expansion",
    };
  }
  // the "you write" pipeline for each demo (verbatim from the snapshot tests / JsonDemo).
  const FUSE_OPT_SRC = {
    "fuse-src-oneloop": "xs.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).run",
    "fuse-src-dce": "xs.fuse.map(x => (x % 3, x * 7, x * 13)).filter(_._1 == 0).map(_._2).run",
    "fuse-src-sink": "xs.fuse.map(x => (x % 2, expensive(x))).filter(_._1 == 0).map(_._2).sum",
    "fuse-src-cse": "xs.fuse.map(x => (x*x + 1, x*x + 2)).map(t => t._1 + t._2).run",
    "fuse-src-fold": "xs.fuse.map(x => Stat(x, x * 100, x * 1000)).foldLeft(0)((acc, s) => acc + s.score)",
    "fuse-src-jsum": "Json.ndjson[Event](src).stream.filter(_.amount > 150).map(_.amount).foldLeft(0.0)(_ + _)",
    "fuse-src-jcat": "Json.ndjson[Event](src).stream.filter(_.amount > 150).map(_.category).toList",
    "fuse-src-jcount": 'Json.ndjson[Event](src).stream.filter(_.status == "active").map(_.category).count',
    "fuse-src-jwide": "Json.ndjson[Wide](src).stream.filter(_.key > 90).map(_.payload).count",
    "fuse-src-jagg":
      'Json.ndjson[Event](src).stream.filter(_.status == "active")\n  .aggTo(Stats.apply)(Agg.sum(_.amount), Agg.count, Agg.max1(_.score))',
  };
  for (const [name, code] of Object.entries(FUSE_OPT_SRC)) {
    out[name] = { name, file: "you write", lang: "scala", code, html: hl(code, "scala"), full: null, fullHtml: null };
  }
  // the rival code each fuse-JSON table beats (verbatim from JsonDemo).
  const RIVALS = {
    "rival-sum":
      `// jsoniter-scala — hand-written reader, the strongest baseline (no object, folds into a var):
val len = in.readKeyAsCharBuf()                 // still decodes EVERY key into a char[] + hashes it
if in.isCharBufEqualsTo(len, "amount") then     // …then compares — even for the 19 fields we skip
  amount = in.readDouble()
else in.skip()                                  // walks the value's bytes; no predicate early-out

// jawn — typelevel's well-regarded parser. No projection: it builds the WHOLE AST per record.
val j = JParser.parseFromByteBuffer(ByteBuffer.wrap(buf, start, len)).get   // JObject of all 20 fields,
val a = j.get("amount").asDouble                                            // every value boxed as a JValue

// Jackson — databind tree model. Same story: a full JsonNode tree per record, then read one field.
val node = mapper.readTree(buf, start, len)     // builds & boxes all 20 fields,
val a = node.get("amount").asDouble             // to read exactly one`,
    "rival-cat":
      `// a case class with ONLY the read fields; the macro-generated codec skips the rest.
final case class Narrow(amount: Double, category: String)
given JsonValueCodec[Narrow] = JsonCodecMaker.make
// per line:
val r = readFromSubArray[Narrow](buf, start, end)   // allocates a Narrow object every record,
if r.amount > 150 then r.category                   // and decodes \`category\` for ALL survivors`,
  };
  for (const [name, code] of Object.entries(RIVALS)) {
    out[name] = { name, file: "what we beat", lang: "scala", code, html: hl(code, "scala"), full: null, fullHtml: null };
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
    { name: "map-leaf-ref", sig: /^\s*def mapLeafIntRef\[RO/, file: "farray/FArrayOps.scala · generated · def mapLeafIntRef" },
    { name: "flatmap-shared", sig: /^\s*def flatMapSharedInt\(/, file: "farray/FArrayOps.scala · generated · def flatMapSharedInt — the compiled-once driver" },
    { name: "flatmap-flat", sig: /^\s*def flatMapFlatInt\(/, file: "farray/FArrayOps.scala · generated · def flatMapFlatInt — the narrow-inner flatten loop" },
    { name: "filter-leaf-int", sig: /^\s*def filterLeafInt\(/, file: "farray/FArrayOps.scala · generated · def filterLeafInt" },
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

  // The generated forward tree-walk (Java), for map Int -> Ref — the one shared, compiled-once traverser.
  const GEN_TRAV = resolve(REPO, ".bleep/generated-sources/farray/farray.GenCores/farray/Traversers.java");
  if (existsSync(GEN_TRAV)) {
    const trav = extractBraceDef(readFileSync(GEN_TRAV, "utf8"), /static <RO> int buildFwdIntRef\(/);
    out["map-traverser"] = {
      name: "map-traverser", file: "farray/Traversers.java · generated · buildFwdIntRef (map Int → Ref)",
      lang: "java", code: trav, html: hl(trav, "java"), full: null, fullHtml: null,
    };
  }

  // ---- FSet: real op source extracted VERBATIM from the generated FSetOps / core, same rules as above.
  // Requires a prior fset build (`bleep compile fset`) so the generated sources exist.
  const GEN_SETS = resolve(REPO, ".bleep/generated-sources/fset/farray.GenSets/farray");
  if (!existsSync(join(GEN_SETS, "FSetOps.scala")))
    throw new Error(`generated FSetOps not found under ${GEN_SETS} — build fset first (e.g. \`bleep compile fset\`)`);
  const genSetOps = readFileSync(join(GEN_SETS, "FSetOps.scala"), "utf8");
  const SET_EXTRACTS = [
    { name: "set-contains-int", sig: /^\s*def containsLeafInt\(/, file: "fset/FSetOps.scala · generated · def containsLeafInt" },
    { name: "set-f14-probe", sig: /^\s*case h: SRefHash =>/, file: "fset/FSetOps.scala · generated · the SRefHash arm of containsLeafRef" },
    { name: "set-merge-ref", sig: /^\s*def mergeUnionRef\(/, file: "fset/FSetOps.scala · generated · def mergeUnionRef" },
    { name: "set-bitmap-merge", sig: /^\s*def bitmapMergeInt\(/, file: "fset/FSetOps.scala · generated · def bitmapMergeInt" },
    { name: "set-materialize", sig: /^\s*def materializeTreeInt\(/, file: "fset/FSetOps.scala · generated · def materializeTreeInt" },
    { name: "set-bitmap-builder", sig: /^private\[farray\] final class IntBitmapBuilder/, file: "fset/FSetOps.scala · generated · class IntBitmapBuilder" },
    { name: "set-density-router", sig: /^\s*def buildSortedInt\(/, file: "fset/FSetOps.scala · generated · def buildSortedInt" },
  ];
  for (const e of SET_EXTRACTS) {
    const code = extractBraceDef(genSetOps, e.sig);
    out[e.name] = { name: e.name, file: e.file, lang: "scala", code, html: hl(code, "scala"), full: null, fullHtml: null };
  }
  // two of the generated Java core files, small enough to show whole (a leaf and a lazy algebra node).
  for (const [name, f] of [["set-bitmap-leaf", "SIntBitmap.java"], ["set-union-node", "SUnion.java"]]) {
    const code = readFileSync(join(GEN_SETS, f), "utf8").replace(/^package farray;\n+/, "").trim();
    out[name] = {
      name, file: `fset/${f} · generated`, lang: "java", code, html: hl(code, "java"), full: null, fullHtml: null,
    };
  }
  // Schematic of the set core, faithful to the real SBase `permits` clause, collapsed across element kinds.
  const SET_NODE_TREE = `// An FSet[A] is one of these. Each carrying-data leaf exists once per element
// kind (Int, Long, Double, Ref) — generated, not hand-written.
sealed abstract class SBase

// materialized leaves — frozen, deduplicated, enumerable:
case object SEmpty                                          extends SMaterialized
final class SIntOne(elem: Int)                              extends SMaterialized  // a single element
final class SIntSorted(arr: Array[Int])                     extends SMaterialized  // packed sorted int[]
final class SIntHash(arr: Array[Int], index: Array[Int])    extends SMaterialized  // frozen open-addressing
final class SRefHash(arr: Array[AnyRef], hashes: Array[Int],
                     ctrl: Array[Byte], keys: Array[AnyRef]) extends SMaterialized // F14 ctrl + keys table
final class SIntBitmap(base: Int, card: Int,
                       words: Array[Long])                  extends SMaterialized  // dense-Int bitmap
final class SIntRange(lo: Int, hi: Int)                     extends SMaterialized  // 16 bytes, any span

// lazy algebra nodes — O(1) to build; they share their operands and memoize their merge:
final class SUnion(left: SBase, right: SBase)               extends SView  // ++ ∪   (volatile memo)
final class SInter(left: SBase, right: SBase)               extends SView  // &  ∩
final class SDiff(left: SBase, right: SBase)                extends SView  // &~ ∖
final class SXor(left: SBase, right: SBase)                 extends SView  // ^  ⊕
final class SComplement(inner: SBase)                       extends SView  // ~  ¬ — membership-only`;
  out["set-node-tree"] = {
    name: "set-node-tree",
    file: "the set core — schematic (the real hierarchy is generated Java, one final class per shape × kind)",
    lang: "scala", code: SET_NODE_TREE, html: hl(SET_NODE_TREE, "scala"), full: null, fullHtml: null,
  };

  // The opaque type + a few of its extension methods, assembled from VERBATIM lines of FArray.scala
  // (picked by signature so it survives edits) — every line is real, it's just a selection.
  const faLines = readFileSync(resolve(REPO, "farray/src/scala/farray/FArray.scala"), "utf8").split("\n");
  const pick = (re) => {
    const l = faLines.find((x) => re.test(x));
    if (l == null) throw new Error(`surface: no line matched ${re}`);
    return l.trim();
  };
  const surface = [
    pick(/^opaque type FArray\[\+A\]/),
    "",
    pick(/^\s*extension \[A\]\(xs: FArray\[A\]\)/),
    "  " + pick(/structural \(tree-aware FBase virtuals\)/),
    "  " + pick(/^\s*def reverse: FArray\[A\]/),
    "  " + pick(/^\s*def \+\+\[B >: A\]/),
    "",
    "  " + pick(/specialized element ops \(lambda inlined/),
    "  " + pick(/^\s*inline def map\[B\]/),
    "  " + pick(/^\s*inline def filter\(inline p/),
    "  " + pick(/^\s*inline def foldLeft\[Z\]/),
  ].join("\n");
  out["surface"] = {
    name: "surface", file: "farray/FArray.scala · the opaque type and a few of its extension methods",
    lang: "scala", code: surface, html: hl(surface, "scala"), full: null, fullHtml: null,
  };

  writeFileSync(resolve(OUT, "snippets.json"), JSON.stringify(out, null, 2));
  console.log(`snippets.json: ${Object.keys(out).length} snippets [${Object.keys(out).join(", ")}]`);
}

// ---------------------------------------------------------------- bench-sources.json
// Parse the REAL .scala benchmark files and extract every `@Benchmark def …` body verbatim, keyed
// by enclosing class then method. This is the exact source that compiles and runs under JMH, so the
// "what was measured" reveal on every chart is the truth, not a hand-written stand-in. (lihaoyi's
// `sourcecode` only captures expression text at a single call site — it can't bulk-extract every
// @Benchmark body without rewriting each benchmark and breaking JMH's `def` signatures — so we parse.)
const leadingWs = (line) => line.match(/^(\s*)/)[1].length;

// Bracket-depth delta of one line, ignoring brackets inside strings/char-literals/comments.
// `st` carries cross-line state for block comments and triple-quoted strings.
function scanDepth(line, st) {
  let delta = 0;
  for (let i = 0; i < line.length; i++) {
    const c = line[i], c2 = line[i + 1];
    if (st.inBlockComment) { if (c === "*" && c2 === "/") { st.inBlockComment = false; i++; } continue; }
    if (st.inTriple) { if (c === '"' && c2 === '"' && line[i + 2] === '"') { st.inTriple = false; i += 2; } continue; }
    if (c === "/" && c2 === "/") break; // line comment runs to EOL
    if (c === "/" && c2 === "*") { st.inBlockComment = true; i++; continue; }
    if (c === '"' && c2 === '"' && line[i + 2] === '"') { st.inTriple = true; i += 2; continue; }
    if (c === '"') { // double-quoted string
      i++;
      while (i < line.length && line[i] !== '"') { if (line[i] === "\\") i++; i++; }
      continue;
    }
    if (c === "'") { // char literal: 'x' or '\n'
      const j = line[i + 1] === "\\" ? i + 3 : i + 2;
      if (line[j] === "'") { i = j; continue; }
      // not a char literal — fall through (treat ' as ordinary)
    }
    if (c === "(" || c === "[" || c === "{") delta++;
    else if (c === ")" || c === "]" || c === "}") delta--;
  }
  return delta;
}

// Extract every @Benchmark method from one file's text -> { cls: { method: code } }.
function extractFromFile(text) {
  const lines = text.split("\n");
  const classRe = /^\s*(?:final )?class (\w+)/;
  const out = {}; // cls -> { method -> code }
  let currentClass = null;
  let i = 0;
  while (i < lines.length) {
    const cm = lines[i].match(classRe);
    if (cm) { currentClass = cm[1]; i++; continue; }
    if (!currentClass || !/@Benchmark\b/.test(lines[i])) { i++; continue; }

    const start = i; // include the @Benchmark annotation line
    let defIdx = i;
    while (defIdx < lines.length && !/\bdef\b/.test(lines[defIdx])) defIdx++;
    const nm = lines[defIdx]?.match(/\bdef\s+(\w+)/);
    if (!nm) { i++; continue; }
    const method = nm[1];
    const defIndent = leadingWs(lines[defIdx]);

    // Capture from the annotation through the end of the body. Body ends when, at bracket-depth 0
    // and past the `def`, the next non-blank line is indented no deeper than the `def` (significant
    // indentation, dot-chains AND `{…}` blocks all fall out of this single rule).
    const st = { inBlockComment: false, inTriple: false };
    let depth = 0, k = start;
    const captured = [];
    for (; k < lines.length; k++) {
      captured.push(lines[k]);
      depth += scanDepth(lines[k], st);
      if (k < defIdx) continue; // still on the annotation line(s)
      if (depth > 0 || st.inBlockComment || st.inTriple) continue;
      let n = k + 1;
      while (n < lines.length && lines[n].trim() === "") n++;
      if (n >= lines.length) break;
      if (leadingWs(lines[n]) > defIndent) continue; // body continues
      break; // body complete
    }
    const code = dedent(captured);
    (out[currentClass] ??= {})[method] = code;
    i = k + 1;
  }
  return out;
}

function buildBenchSources(rootRel, outName, benchClasses) {
  const files = walk(resolve(REPO, rootRel), []);
  const byClass = {}; // cls -> { method -> { code, html } }
  let methodCount = 0;
  for (const file of files) {
    const extracted = extractFromFile(readFileSync(file, "utf8"));
    for (const [cls, methods] of Object.entries(extracted)) {
      const dst = (byClass[cls] ??= {});
      for (const [method, code] of Object.entries(methods)) {
        if (dst[method]) continue; // first definition wins (none collide in practice)
        dst[method] = { code, html: hl(code, "scala") };
        methodCount++;
      }
    }
  }
  // ColdPipelineIntBenchmark defines no @Benchmark methods of its own — it inherits them from
  // LongMixedPipelineIntBenchmark and only flips the JMH mode to cold. Its per-card source is the parent's.
  if (byClass.LongMixedPipelineIntBenchmark && !byClass.ColdPipelineIntBenchmark) {
    byClass.ColdPipelineIntBenchmark = byClass.LongMixedPipelineIntBenchmark;
  }
  writeFileSync(resolve(OUT, outName), JSON.stringify(byClass));
  console.log(`${outName}: ${methodCount} methods across ${Object.keys(byClass).length} classes`);

  // Coverage: every benchmark class that shows up in the slimmed data should have extracted source.
  const missing = [...benchClasses].filter((c) => !byClass[c]).sort();
  if (missing.length) console.warn(`  ⚠ ${missing.length} class(es) with NO source: ${missing.join(", ")}`);
  else console.log(`  ✓ all ${benchClasses.size} benchmark classes have source`);
}

const classesOf = (slim) => new Set(slim.map((e) => e.b.split(".").at(-2)));
const slim = buildBench("docs/bench-results.json", "bench.json");
const setSlim = buildBench("docs/set-bench-results.json", "setbench.json");
buildSnippets();
buildBenchSources("benchmarks/src/scala/farray", "bench-sources.json", classesOf(slim));
buildBenchSources("setbenchmarks/src/scala/farray", "setbench-sources.json", classesOf(setSlim));
