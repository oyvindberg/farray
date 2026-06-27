package farray.json

import farray.FuseDebug

/** An EDUCATIONAL, runnable tour of the fused JSON projection macro.
 *
 *  Run console:  `bleep run farray --class farray.json.JsonDemo`
 *  Write HTML:   `bleep run farray --class farray.json.JsonDemo -- --html docs/fused-json-demo.html`
 *                (or just `--html`, which writes ./fused-json-demo.html)
 *
 *  The one idea to take away:
 *
 *      A JSON object is a PRODUCT whose fields are COLUMNS, sourced from byte ranges.
 *
 *  `fuse` already optimizes products of columns for in-memory data — it decides which columns to compute, and
 *  when (dead columns are never computed; a column used only past a filter is computed only for survivors).
 *  This demo shows that the SAME optimizer, unchanged, drives a JSON parser: the columns are now "decode field
 *  K from the record's bytes," so the optimizer's decisions become parsing decisions. Each lesson shows the
 *  query, what the optimizer does and why, the result, and the ACTUAL generated code where it matters.
 */
object JsonDemo:

  /** A fat record: 20 fields. Our queries touch 1-2 of them — the rest must cost as little as possible. */
  final case class Event(
      id: Long, ts: Long, userId: Long, sessionId: Long,
      amount: Double, score: Double, lat: Double, lon: Double,
      age: Int, rank: Int, count: Int, flags: Int,
      name: String, category: String, region: String, status: String,
      country: String, device: String, source: String, label: String
  )

  /** A tiny NDJSON sample (one JSON object per line). In production this is a memory-mapped file or a stream. */
  val sample: Array[Byte] =
    """{"id":1,"ts":1700000001,"userId":42,"sessionId":7,"amount":250.50,"score":88.0,"lat":59.9,"lon":10.7,"age":34,"rank":2,"count":5,"flags":1,"name":"Ada","category":"books","region":"eu","status":"active","country":"NO","device":"ios","source":"web","label":"vip"}
{"id":2,"ts":1700000002,"userId":43,"sessionId":8,"amount":12.00,"score":40.0,"lat":51.5,"lon":-0.1,"age":51,"rank":9,"count":1,"flags":0,"name":"Lin","category":"music","region":"eu","status":"inactive","country":"GB","device":"web","source":"ads","label":"std"}
{"id":3,"ts":1700000003,"userId":44,"sessionId":9,"amount":999.99,"score":95.0,"lat":40.7,"lon":-74.0,"age":29,"rank":1,"count":12,"flags":1,"name":"Mateo","category":"books","region":"us","status":"active","country":"US","device":"android","source":"web","label":"vip"}
{"id":4,"ts":1700000004,"userId":45,"sessionId":10,"amount":75.25,"score":60.0,"lat":35.7,"lon":139.7,"age":40,"rank":4,"count":3,"flags":0,"name":"Yuki","category":"games","region":"jp","status":"active","country":"JP","device":"ios","source":"web","label":"std"}
{"id":5,"ts":1700000005,"userId":46,"sessionId":11,"amount":5.00,"score":20.0,"lat":48.8,"lon":2.3,"age":62,"rank":12,"count":1,"flags":0,"name":"Ola","category":"music","region":"eu","status":"inactive","country":"FR","device":"web","source":"ads","label":"std"}
""".getBytes(java.nio.charset.StandardCharsets.UTF_8)

  /** A lesson: heading, the prose explanation, the query, its result, and an optional generated-code excerpt. */
  final case class Lesson(title: String, prose: String, query: String, result: String, code: Option[String])

  // ── the model statement (intro) ──────────────────────────────────────────────────────────────────────
  val model: String =
    """A JSON object is a product whose fields are columns, sourced from byte ranges.
      |You write the same `.fuse.filter(...).map(...).<terminal>` pipeline you'd write over an in-memory FArray.
      |The macro reads the whole pipeline plus the record type, works out which fields the pipeline actually
      |touches, and emits ONE per-record byte scanner. None of the byte-level code is hand-written — it is all
      |generated from the record's fields and the fields your query reads, and driven by the SAME optimizer
      |that fuses in-memory pipelines."""

  // ── the five lessons (results computed at runtime; code captured at compile time) ─────────────────────
  def lessons(): List[Lesson] = List(
    Lesson(
      "1. Projection pushdown — read 1 field of 20, skip the rest at the byte level",
      """The pipeline reads only `amount`. The optimizer's live-set is therefore { amount }; every other field
        |is a DEAD column. In the generated scanner a dead field never gets a `var` slot — it is handed to
        |`skipValue`, which advances past its bytes without decoding or allocating anything. So a 20-field
        |record costs one number-parse and 19 byte-skips, and no `Event` object is ever built.""".stripMargin,
      "Json.ndjson[Event](src).fuse.filter(_.amount > 100).map(_.amount).foldLeft(0.0)(_ + _)",
      Json.ndjson[Event](sample).fuse.filter(_.amount > 100).map(_.amount).foldLeft(0.0)(_ + _).toString + "   (250.50 + 999.99)",
      Some(focus(aggCode, "var v", 20))
    ),
    Lesson(
      "2. Lazy decode — a projected String is built only for survivors",
      """`category` is read, so it gets a slot — but a STRING slot is just (start, len) into the buffer, not a
        |decoded String. `amount` decodes (the filter needs it); `category` stays a byte slice and becomes a
        |real String only INSIDE the survivor `if`. The rejected rows never allocate a String. This is the
        |"compute-for-survivors" sink — the exact mechanism fuse uses for in-memory columns, now over bytes.""".stripMargin,
      "Json.ndjson[Event](src).fuse.filter(_.amount > 100).map(_.category).toList",
      Json.ndjson[Event](sample).fuse.filter(_.amount > 100).map(_.category).toList.toString,
      Some(focus(catCode, "if (`v", 9))
    ),
    Lesson(
      "3. Discard-DCE — count ignores the element, so the projection is dead",
      """`count` discards the element value entirely, so `map(_.category)` produces a value nobody reads — it is
        |DEAD. The optimizer doesn't even decode the category strings (the slice is captured but never turned
        |into a String). The same applies to `isEmpty` / `nonEmpty`, and to `map(expensive).count` — `expensive`
        |never runs. Dead-column elimination extended all the way to the terminal.""".stripMargin,
      """Json.ndjson[Event](src).fuse.filter(_.status == "active").map(_.category).count""",
      Json.ndjson[Event](sample).fuse.filter(_.status == "active").map(_.category).count.toString + "   (3 active — and ZERO category Strings decoded)",
      None
    ),
    Lesson(
      "4. Predicate-fail early-out — abandon a record the moment its filter fails",
      """The filter field `amount` is decoded DURING the scan. The instant it's known, the predicate is checked
        |INLINE. If it fails, the record is abandoned right there — the scanner never looks for `label` (which
        |comes later) and never decodes it. With a selective filter over fat records this is the biggest win:
        |rejected records cost only "scan to the predicate field, compare, stop". (Benchmarked at 2.4x jsoniter
        |when the predicate field is early, the projected field is a long string at the end, and 90% are
        |rejected.)""".stripMargin,
      "Json.ndjson[Event](src).fuse.filter(_.amount > 900).map(_.label).toList",
      Json.ndjson[Event](sample).fuse.filter(_.amount > 900).map(_.label).toList.toString + "   (only event 3's 999.99 clears 900)",
      None
    ),
    Lesson(
      "5. Short-circuit — find / head stop reading the stream at the first hit",
      """`headOption` only wants the first survivor. The shared `done` flag — the same one that powers `take`
        |and `find` for in-memory pipelines — breaks the line loop the moment a match is produced, so later
        |lines of the file are never even scanned.""".stripMargin,
      "Json.ndjson[Event](src).fuse.filter(_.amount > 500).map(_.name).headOption",
      Json.ndjson[Event](sample).fuse.filter(_.amount > 500).map(_.name).headOption.toString + "   (stops after event 3)",
      None
    )
  )

  val takeaway: String =
    """One optimizer, five wins, all from the question "which columns does the pipeline read, and when?":
      |projection pushdown (only read fields are scanned), lazy decode (strings built only for survivors),
      |discard-DCE (a value the terminal ignores is never computed), predicate-fail early-out (a rejected
      |record is abandoned mid-scan), and short-circuit (find/head stop at the first hit) — all at near-zero
      |allocation, no per-record object, benchmarked at ~1.7–2.4x hand-tuned jsoniter."""

  def main(args: Array[String]): Unit =
    val ls = lessons()
    args.toList match
      case "--html" :: rest =>
        val path = rest.headOption.getOrElse("fused-json-demo.html")
        java.nio.file.Files.writeString(java.nio.file.Path.of(path), renderHtml(ls))
        println(s"wrote $path")
      case _ =>
        renderConsole(ls)

  // ── console renderer ──────────────────────────────────────────────────────────────────────────────────
  private def renderConsole(ls: List[Lesson]): Unit =
    def head(t: String): Unit = { println("\n" + "=" * 96); println("  " + t); println("=" * 96) }
    head("THE MODEL"); model.stripMargin.linesIterator.foreach(l => println("  " + l))
    ls.foreach { L =>
      head(L.title)
      L.prose.linesIterator.foreach(l => println("  " + l))
      println("\n  query:  " + L.query)
      println("  result: " + L.result)
      L.code.foreach(c => { println("\n  generated:"); c.linesIterator.foreach(l => println("    " + l)) })
    }
    head("THE TAKEAWAY"); takeaway.stripMargin.linesIterator.foreach(l => println("  " + l))

  // ── HTML renderer (self-contained, inline CSS) ───────────────────────────────────────────────────────
  private def renderHtml(ls: List[Lesson]): String =
    val lessonsHtml = ls.map { L =>
      val code = L.code.map(c => s"""<p class="cap">generated per-record scanner (verbatim, abridged):</p><pre class="gen">${esc(c)}</pre>""").getOrElse("")
      s"""<section class="lesson">
         |  <h2>${esc(L.title)}</h2>
         |  <p class="prose">${prose(L.prose)}</p>
         |  <p class="cap">query</p><pre class="scala">${highlightScala(L.query)}</pre>
         |  <p class="cap">result</p><pre class="result">${esc(L.result)}</pre>
         |  $code
         |</section>""".stripMargin
    }.mkString("\n")
    s"""<!doctype html>
       |<html lang="en"><head><meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1">
       |<title>Inside the fused JSON parser</title>
       |<style>$css</style></head>
       |<body>
       |<header>
       |  <h1>Inside the fused JSON parser</h1>
       |  <p class="lede">A <code>.fuse.filter(…).map(…)</code> pipeline over JSON bytes, compiled by the same
       |  optimizer that fuses in-memory pipelines — into one per-record byte scanner.</p>
       |</header>
       |<section class="model"><h2>The model</h2><p>${prose(model.stripMargin)}</p></section>
       |$lessonsHtml
       |<section class="takeaway"><h2>The takeaway</h2><p>${prose(takeaway.stripMargin)}</p></section>
       |<footer>Generated by <code>farray.json.JsonDemo --html</code>. The code blocks are the verbatim
       |post-typer expansion (via <code>FuseDebug.show</code>), shown, never executed.</footer>
       |</body></html>""".stripMargin

  /** light prose markup: `code` spans → <code>, blank lines → paragraph breaks. */
  private def prose(s: String): String =
    val withCode = "`([^`]+)`".r.replaceAllIn(esc(s), m => s"<code>${m.group(1)}</code>")
    withCode.replace("\n\n", "</p><p>").replace("\n", " ")

  /** very small Scala token highlighter for the one-line query — enough to read pleasantly, not a real lexer. */
  private def highlightScala(s: String): String =
    var h = esc(s)
    for kw <- List("filter", "map", "foldLeft", "fuse", "toList", "count", "headOption", "ndjson") do
      h = h.replace(kw, s"""<span class="kw">$kw</span>""")
    h

  private def esc(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private val css =
    """:root{--bg:#0f1117;--fg:#e6e6e6;--mut:#9aa4b2;--acc:#7dd3fc;--ok:#86efac;--code:#161a23;--bd:#262c38}
      |*{box-sizing:border-box}
      |body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.6 -apple-system,Segoe UI,Roboto,sans-serif;
      |  max-width:980px;margin:0 auto;padding:2rem 1.25rem 4rem}
      |header h1{font-size:2rem;margin:.2rem 0}
      |.lede{color:var(--mut);font-size:1.1rem}
      |h2{font-size:1.15rem;margin:0 0 .5rem;color:var(--acc)}
      |section{border:1px solid var(--bd);border-radius:12px;padding:1.1rem 1.25rem;margin:1.25rem 0;background:#12151d}
      |.model{background:#11161f;border-color:#2b3a4a}
      |.takeaway{background:#11201a;border-color:#234534}
      |.prose,.model p,.takeaway p{color:var(--fg)}
      |.cap{color:var(--mut);font-size:.78rem;text-transform:uppercase;letter-spacing:.06em;margin:.9rem 0 .25rem}
      |pre{background:var(--code);border:1px solid var(--bd);border-radius:8px;padding:.7rem .9rem;overflow:auto;
      |  font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,monospace;margin:0}
      |pre.scala{color:#dbe7ff} pre.result{color:var(--ok)} pre.gen{color:#c8d0dc;max-height:380px}
      |.kw{color:#c792ea;font-weight:600}
      |code{background:var(--code);border:1px solid var(--bd);border-radius:4px;padding:.05rem .3rem;
      |  font:13px ui-monospace,Menlo,monospace;color:#ffd9a8}
      |footer{color:var(--mut);font-size:.85rem;margin-top:2rem}""".stripMargin

  // ── generated-code captures (compile-time; shown, never executed) ─────────────────────────────────────
  private inline def aggCode: String =
    FuseDebug.show(Json.ndjson[Event](sample).fuse.filter(_.amount > 100).map(_.amount).foldLeft(0.0)(_ + _))
  private inline def catCode: String =
    FuseDebug.show(Json.ndjson[Event](sample).fuse.filter(_.amount > 100).map(_.category).toList)

  /** excerpt: from the first line containing `from`, take `afterLines` lines, eliding the rest. */
  private def focus(code: String, from: String, afterLines: Int): String =
    val lines = code.linesIterator.toVector
    val i = lines.indexWhere(_.contains(from))
    if i < 0 then code
    else (lines.slice(i, math.min(i + afterLines, lines.length)) :+ "… (rest of the loop elided)").mkString("\n")

end JsonDemo
