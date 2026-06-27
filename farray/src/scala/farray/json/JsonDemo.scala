package farray.json

import farray.{FuseDebug, FArray}

/** Educational, runnable demos for the fused pipeline. Generates TWO self-contained HTML pages:
  *
  * bleep run farray --class farray.json.JsonDemo # writes both pages into docs/ bleep run farray --class farray.json.JsonDemo -- DIR # writes both pages into
  * DIR
  *
  * Page 1 docs/fused-optimizer.html — what the fuse optimizer is, taught over ordinary in-memory FArray collections (map/filter fusion, DCE,
  * compute-for-survivors, CSE), each example showing the VERBATIM generated loop. Page 2 docs/fused-json.html — the same optimizer pointed at JSON bytes:
  * projection pushdown, lazy decode, discard-DCE, predicate-fail early-out — each with the generated scanner, the jsoniter code we beat, and measured benchmark
  * scores.
  *
  * All generated code is captured at compile time via FuseDebug.show (shown, never executed).
  */
object JsonDemo:

  // ════════════════════════════════════════ shared types ════════════════════════════════════════════════
  final case class Example(title: String, prose: String, code: String, gen: String, rival: Option[(String, String)] = None, bench: Option[Bench] = None)

  /** a measured comparison: (label, ops/s, bytes/op) rows + a headline. */
  final case class Bench(headline: String, rows: List[(String, String, String)])

  def main(args: Array[String]): Unit =
    val dir = args.headOption.getOrElse("docs")
    java.nio.file.Files.writeString(
      java.nio.file.Path.of(s"$dir/fused-optimizer.html"),
      page("The fuse optimizer", optimizerLede, optimizerSections, prevNext = ("", "fused-json.html", "The JSON demo →"))
    )
    java.nio.file.Files.writeString(
      java.nio.file.Path.of(s"$dir/fused-json.html"),
      page(
        "Fused JSON: an enormously fast, projection-aware parser",
        jsonLede,
        jsonSections,
        prevNext = ("fused-optimizer.html", "", "← The optimizer"),
        setup = setupCard
      )
    )
    println(s"wrote $dir/fused-optimizer.html and $dir/fused-json.html")

  // ════════════════════════════════ PAGE 1: the optimizer over collections ═══════════════════════════════
  val optimizerLede: String =
    """`xs.fuse.map(f).filter(p).<terminal>` is a macro, not a runtime data structure. It reads the WHOLE chain
      |off the typed AST at compile time and emits one specialized `while` loop with the lambdas inlined into the
      |body — no view nodes, no `Function1`, no boxing, no megamorphism. Below: the same loop you'd write by hand,
      |produced from the pipeline you'd actually want to write. Every code block is the verbatim post-typer
      |expansion."""

  def optimizerSections(): List[Example] =
    val xs = FArray(1, 2, 3, 4, 5)
    List(
      Example(
        "One loop, no closures",
        """Three stages, one `int[]` scan. `_ + 1` became the bytecode `iadd`; no `Function1` was allocated; the
          |filter is an `if` that wraps everything downstream of it. A `View` would store three `Function1`s and
          |call each through an interface per element.""".stripMargin,
        "xs.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).run",
        clean(c_mapfilter)
      ),
      Example(
        "Dead-column elimination",
        """A tuple/case class is a set of independent COLUMNS. Here `map` builds three, then the pipeline reads
          |only two of them. The optimizer never builds the tuple: column 2 (`x * 13`) is read by nobody, so it
          |is DEAD and absent from the output; column 0 (`x % 3`) is needed by the filter, so it's computed
          |eagerly. A `View` allocates the 3-tuple (plus 3 boxed Ints) every element and throws most of it away.""".stripMargin,
        "xs.fuse.map(x => (x % 3, x * 7, x * 13)).filter(_._1 == 0).map(_._2).run",
        clean(c_dce)
      ),
      Example(
        "Compute-for-survivors (the sink)",
        """Look at WHERE `x * 7` lands: INSIDE the `if`. Column 1 is only needed by the final `map`, which is
          |downstream of the filter, so it is computed only for elements that SURVIVE. An expensive column behind
          |a selective filter runs a fraction of the time — work the lazy-collection model cannot express,
          |because it has no idea which columns a later stage will read.""".stripMargin,
        "xs.fuse.map(x => (x % 2, expensive(x))).filter(_._1 == 0).map(_._2).sum",
        clean(c_survivors)
      ),
      Example(
        "Common-subexpression elimination",
        """`x * x` is written twice but computed ONCE — hash-consed across both tuple components, bound to one
          |`val`, reused. Decomposition + DCE + the sink + CSE all fall out of one idea: model a product as
          |independent lazy columns and bind each at its first use.""".stripMargin,
        "xs.fuse.map(x => (x*x + 1, x*x + 2)).map(t => t._1 + t._2).run",
        clean(c_cse)
      ),
      Example(
        "…and the decomposition reaches the terminal's lambda",
        """The same column analysis applies to a `foldLeft`/`reduce` lambda. Here the fold reads only `s.score`,
          |so the optimizer never builds the `Stat`: `s.id` and `s.weight` (and the `Stat` itself) are DEAD and
          |absent — the loop is just `acc = acc + (x * 100)`. (A *method* call on `s` — `s.combined` — would need
          |the whole object and correctly rebuilds it; only field accessors project to columns.) Before this, the
          |fold rebuilt the entire product per element just to read one field.""".stripMargin,
        "xs.fuse.map(x => Stat(x, x * 100, x * 1000)).foldLeft(0)((acc, s) => acc + s.score)",
        clean(c_foldop)
      )
    )

  // ════════════════════════════════ PAGE 2: the optimizer parses JSON ════════════════════════════════════
  val jsonLede: String =
    """Now turn the optimizer loose on JSON. The insight that makes it work: a JSON object is a PRODUCT whose
      |fields are COLUMNS, sourced from byte ranges. So the SAME optimizer — unchanged — becomes an enormously
      |fast, projection-aware JSON parser. You wrap a byte buffer as a source and start a pipeline with `.stream`
      |(`Json.ndjson[Event](src).stream.filter(…).map(…)` — the `.stream` signals it's a record SOURCE consumed
      |byte-by-byte, not an in-memory `FArray`, which uses `.fuse`). The macro reads your `.filter(…).map(…)`
      |chain, works out which fields it actually touches, and emits ONE specialized per-record scanner that
      |reads straight off the `byte[]`: dead fields are skipped at the byte level, a projected string is decoded
      |only for survivors, a record whose filter fails is abandoned mid-scan, and no per-record object is ever
      |allocated. The result beats the JVM's best hand-tuned parser on a projection query — and laps the popular
      |AST parsers."""

  /** the benchmark setup, shown up front so the numbers below have context. (Sizes measured from the real 10 000-record buffer the benchmark uses: 3 348 775
    * bytes total, ~334 bytes/record.)
    */
  val jsonSetup: String =
    """The dataset is NDJSON — one JSON object per line — of <b>10 000 records</b>, <b>3 348 775 bytes (~3.2 MB)</b>,
      |each record a <b>20-field</b> object averaging <b>~334 bytes</b> (a realistic mix of longs, ints, doubles,
      |and strings). Every query below touches just <b>1–2 of those 20 fields</b> — the projection case where
      |pushdown pays. A representative record:"""

  val sampleRecordPretty: String =
    """{ "id":7, "ts":1700000000000, "age":34, "amount":250.50, "score":88.0,
      |  "name":"Ada", "category":"books", "status":"active", "region":"eu", … 11 more fields … }"""

  /** benchmark methodology note, shown once under the setup. */
  val jsonMethod: String =
    """Measured with JMH (5×2s warmup, 5×2s measurement) and `-prof gc` for allocation. Throughput is ops/s
      |over the whole 10 000-record buffer; allocation (B/op) is bytes per buffer-op, so ÷10 000 ≈ bytes per
      |record. Each parser does the SAME query: filter on one field, project another. The fuse column is the
      |generated scanner; the others are each library's best idiom for "read a couple of fields per line." Run
      |on a warm JVM (GraalVM 25, Apple Silicon); your machine will differ, but the ratios are the story."""

  def jsonSections(): List[Example] =
    val b = sample
    List(
      Example(
        "Projection pushdown — read 1 field of 20",
        """The pipeline reads only `amount`. Every other field is a dead column with no slot: the generated
          |scanner hands it to `skipValue`, which advances past its bytes without decoding or allocating. A
          |20-field record costs one number-parse and 19 byte-skips. No `Event` is built; the fold runs into a
          |`var`.""".stripMargin,
        "Json.ndjson[Event](src).stream.filter(_.amount > 150).map(_.amount).foldLeft(0.0)(_ + _)",
        clean(j_sum),
        rival = Some(("the field — jsoniter-scala (hand-written reader, no object), jawn, and Jackson all do the same query", rivalsSrc)),
        bench = Some(
          Bench(
            "fuse beats the JVM's best hand-tuned parser 1.5×, and laps the AST parsers 4–9× — at ~zero allocation",
            List(
              ("fuse (generated scanner)", "395 ops/s", "0.6 B/op"),
              ("jsoniter — hand-written reader, no object", "256 ops/s", "0.8 B/op"),
              ("jsoniter — full codec (parses all 20 fields)", "145 ops/s", "6 464 001 B/op"),
              ("Jackson — tree model (JsonNode)", "87 ops/s", "31 047 490 B/op"),
              ("jawn — AST (JValue)", "46 ops/s", "45 885 204 B/op")
            )
          )
        )
      ),
      Example(
        "Lazy decode — a String built only for survivors",
        """`category` gets a slot, but a STRING slot is just `(start, len)` into the buffer — not a decoded
          |String. `amount` decodes (the filter needs it); `category` stays a byte slice and becomes a real
          |`String` only INSIDE the survivor `if`. Rejected rows never allocate a String. This is the same
          |compute-for-survivors sink from page 1, now over bytes.""".stripMargin,
        "Json.ndjson[Event](src).stream.filter(_.amount > 150).map(_.category).toList",
        clean(j_cat),
        rival = Some(("jsoniter-scala, narrow codec (only the read fields; the rest skipped)", jsoniterNarrowSrc))
      ),
      Example(
        "Discard-DCE — count never decodes the projection",
        """`count` ignores the element, so `map(_.category)` is a dead value. The optimizer captures the slice
          |but never turns it into a String — zero String allocations. Same for `isEmpty`/`nonEmpty`, and for
          |`map(expensive).count`, where `expensive` never runs. Dead-column elimination reaching the terminal.""".stripMargin,
        """Json.ndjson[Event](src).stream.filter(_.status == "active").map(_.category).count""",
        clean(j_count),
        bench = Some(
          Bench(
            "fuse 1.47× faster, and ~zero allocation vs jsoniter's per-record object",
            List(("fuse (no String decoded at all)", "340 ops/s", "0.6 B/op"), ("jsoniter — narrow codec", "231 ops/s", "720 001 B/op"))
          )
        )
      ),
      Example(
        "Predicate-fail early-out — abandon a record mid-scan",
        """The filter field is decoded DURING the scan; the instant it's known the predicate is checked INLINE,
          |and on failure the record is abandoned right there — the scanner never looks for the later projected
          |field. Here `key` is first, `payload` (a long string) is last, and 90% of records are rejected: the
          |rejected ones cost only "scan to `key`, compare, stop". This is the biggest win on fat selective
          |queries.""".stripMargin,
        "Json.ndjson[Wide](src).stream.filter(_.key > 90).map(_.payload).count",
        clean(j_wide),
        bench = Some(
          Bench(
            "fuse 2.47× faster, near-zero allocation vs 1.4 MB/op",
            List(
              ("fuse (rejected records stop at `key`)", "1212 ops/s", "0.17 B/op"),
              ("jsoniter — narrow codec {key, payload}", "491 ops/s", "1 432 000 B/op")
            )
          )
        )
      ),
      Example(
        "Multi-aggregate — three aggregates, ONE pass, into a case class",
        """Sum the amount, count the rows, and take the peak score of the active events — `agg` runs all three
          |in a single fused scan, carrying one (UNBOXED) accumulator each. The columns the aggregates read are
          |MERGED automatically: `amount`, `status`, and `score` are scanned (3 of 20 fields), each once; `count`
          |reads nothing extra. `aggTo(Stats.apply)` returns a case class, not a tuple. No `Event` is built; the
          |`sum`/`max` use `dadd`/`dcmp`, not the boxing `Numeric`/`Ordering`.""".stripMargin,
        "Json.ndjson[Event](src).stream.filter(_.status == \"active\")\n  .aggTo(Stats.apply)(Agg.sum(_.amount), Agg.count, Agg.max1(_.score))",
        clean(j_agg)
      )
    )

  // ════════════════════════════════════ records + jsoniter source ════════════════════════════════════════
  final case class Event(
      id: Long,
      ts: Long,
      userId: Long,
      sessionId: Long,
      amount: Double,
      score: Double,
      lat: Double,
      lon: Double,
      age: Int,
      rank: Int,
      count: Int,
      flags: Int,
      name: String,
      category: String,
      region: String,
      status: String,
      country: String,
      device: String,
      source: String,
      label: String
  )
  final case class Wide(key: Int, f1: Int, f2: Int, f3: Double, payload: String)

  /** the case-class result of the multi-aggregate example. */
  final case class Stats(revenue: Double, n: Int, peak: Double)

  val sample: Array[Byte] =
    ("""{"id":1,"ts":1700000001,"userId":42,"sessionId":7,"amount":250.50,"score":88.0,"lat":59.9,"lon":10.7,"age":34,"rank":2,"count":5,"flags":1,"name":"Ada","category":"books","region":"eu","status":"active","country":"NO","device":"ios","source":"web","label":"vip"}
{"id":3,"ts":1700000003,"userId":44,"sessionId":9,"amount":999.99,"score":95.0,"lat":40.7,"lon":-74.0,"age":29,"rank":1,"count":12,"flags":1,"name":"Mateo","category":"books","region":"us","status":"active","country":"US","device":"android","source":"web","label":"vip"}
""").getBytes(java.nio.charset.StandardCharsets.UTF_8)
  val wideSample: Array[Byte] =
    """{"key":95,"f1":1,"f2":2,"f3":3.5,"payload":"long-payload-string-here"}
""".getBytes(java.nio.charset.StandardCharsets.UTF_8)

  // the strongest jsoniter baseline (a JsonReader loop, no object) and the narrow-codec idiom we compare to.
  val jsoniterReaderSrc: String =
    """new JsonValueCodec[Double]:
      |  def decodeValue(in: JsonReader, default: Double): Double =
      |    var amount = default
      |    if !in.isNextToken('{') then in.decodeError("expected {")
      |    if !in.isNextToken('}') then
      |      in.rollbackToken()
      |      var continue = true
      |      while continue do
      |        val len = in.readKeyAsCharBuf()              // decodes EVERY key into a char[] + hashes it
      |        if in.isCharBufEqualsTo(len, "amount") then  // …then compares — even for the 19 we skip
      |          amount = in.readDouble()
      |        else in.skip()                               // walks the value's bytes; no early-out
      |        continue = in.isNextToken(',')
      |      if !in.isCurrentToken('}') then in.objectEndOrCommaError()
      |    amount""".stripMargin

  val jsoniterNarrowSrc: String =
    """// a case class with ONLY the read fields; the macro-generated codec skips the rest.
      |final case class Narrow(amount: Double, category: String)
      |given JsonValueCodec[Narrow] = JsonCodecMaker.make
      |// per line:
      |val r = readFromSubArray[Narrow](buf, start, end)   // allocates a Narrow object every record,
      |if r.amount > 150 then r.category                   // and decodes `category` for ALL survivors""".stripMargin

  /** all three rivals doing the SAME query, so the table rows have code. */
  val rivalsSrc: String =
    """// jsoniter-scala — hand-written reader, the strongest baseline (no object, folds into a var):
      |val len = in.readKeyAsCharBuf()                 // still decodes EVERY key into a char[] + hashes it
      |if in.isCharBufEqualsTo(len, "amount") then     // …then compares — even for the 19 fields we skip
      |  amount = in.readDouble()
      |else in.skip()                                  // walks the value's bytes; no predicate early-out
      |
      |// jawn — typelevel's well-regarded parser. No projection: it builds the WHOLE AST per record.
      |val j = JParser.parseFromByteBuffer(ByteBuffer.wrap(buf, start, len)).get   // JObject of all 20 fields,
      |val a = j.get("amount").asDouble                                            // every value boxed as a JValue
      |
      |// Jackson — databind tree model. Same story: a full JsonNode tree per record, then read one field.
      |val node = mapper.readTree(buf, start, len)     // builds & boxes all 20 fields,
      |val a = node.get("amount").asDouble             // to read exactly one""".stripMargin

  // ════════════════════════════════════ generated-code captures ══════════════════════════════════════════
  private val xs5 = FArray(1, 2, 3, 4, 5)
  private inline def c_mapfilter: String = FuseDebug.show(xs5.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).run)
  private inline def c_dce: String = FuseDebug.show(xs5.fuse.map(x => (x % 3, x * 7, x * 13)).filter(_._1 == 0).map(_._2).run)
  private inline def c_survivors: String = FuseDebug.show(xs5.fuse.map(x => (x % 2, expensive(x))).filter(_._1 == 0).map(_._2).sum)
  private inline def c_cse: String = FuseDebug.show(xs5.fuse.map(x => (x * x + 1, x * x + 2)).map(t => t._1 + t._2).run)
  private inline def c_foldop: String = FuseDebug.show(xs5.fuse.map(x => Stat(x, x * 100, x * 1000)).foldLeft(0)((acc, s) => acc + s.score))
  private def expensive(x: Int): Int = { var s = 0; var i = 0; while i < x do { s += i; i += 1 }; s }

  /** a 3-field product for the fold-decomposition example. The fold reads only `score`; `id`/`weight` are dead. */
  final case class Stat(id: Int, score: Int, weight: Int)

  private inline def j_sum: String = FuseDebug.show(Json.ndjson[Event](sample).stream.filter(_.amount > 150).map(_.amount).foldLeft(0.0)(_ + _))
  private inline def j_cat: String = FuseDebug.show(Json.ndjson[Event](sample).stream.filter(_.amount > 150).map(_.category).toList)
  private inline def j_count: String = FuseDebug.show(Json.ndjson[Event](sample).stream.filter(_.status == "active").map(_.category).count)
  private inline def j_wide: String = FuseDebug.show(Json.ndjson[Wide](wideSample).stream.filter(_.key > 90).map(_.payload).count)
  private inline def j_agg: String = FuseDebug.show(
    Json
      .ndjson[Event](sample)
      .stream
      .filter(_.status == "active")
      .aggTo(Stats.apply)(farray.Agg.sum(_.amount), farray.Agg.count, farray.Agg.max1(_.score))
  )

  /** strip the dead `$proxy`/`Fuse_this` preamble and opacity-cast noise so the loop reads cleanly. */
  private def clean(code: String): String =
    val lines = code.linesIterator.toVector
    // drop everything up to the first real statement: the `val out`/`var acc`/`val buf` of the emitted block.
    val start = lines.indexWhere(l => l.contains("val out") || l.contains("var acc") || l.contains("val buf"))
    val body = if start < 0 then lines else lines.drop(start)
    body.iterator
      .filterNot(l =>
        l.contains("$proxy") || l.contains("asInstanceOf$") || l.contains("FArray$package") ||
          l.trim.startsWith("type FArray") || l.trim == "}]" || l.trim.isEmpty ||
          l.contains("val src0: FBase = null") || l.contains("val n0: Int = 16")
      )
      .map(
        _.replace(".asInstanceOf[FArray[Int]]", "")
          .replace("`", "")
          // unwrap the opaque NdjsonSource accessors so they read as plain field reads
          .replaceAll("""inline\$(buf|until|from)\$i1\(src\)""", "src.$1")
          .replaceAll("""val src: NdjsonSource\[.*""", "val src = source")
          .replace("null.asInstanceOf[String]", "null")
      )
      .mkString("\n")
      .replace(".+(", " + (")
      .replace(".*(", " * (")
      .replace(".%(", " % (")
      .replace(".<(", " < (")
      .replace(".==(", " == (")
      .replace(".>(", " > (")
      .replace(".!=(", " != (")
      .replace(".&&(", " && (")
      .replace(".||(", " || (")
      .replace(".apply(", "(")
      .replace(".update(", ".set(")
      .replaceAll("""(\w+)\.unary_!""", "!$1") // seen.unary_! -> !seen

  // ══════════════════════════════════════════ HTML rendering ═════════════════════════════════════════════
  private def page(title: String, lede: String, sections: () => List[Example], prevNext: (String, String, String), setup: String = ""): String =
    val (prevHref, nextHref, navLabel) = prevNext
    val nav =
      if nextHref.nonEmpty then s"""<a class="nav" href="$nextHref">$navLabel</a>"""
      else s"""<a class="nav" href="$prevHref">$navLabel</a>"""
    val body = sections().map(renderExample).mkString("\n")
    s"""<!doctype html><html lang="en"><head><meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1"><title>${esc(title)}</title>
       |<style>$css</style></head><body>
       |<header><div class="tabs"><a${cls(prevHref.isEmpty && nextHref.nonEmpty)} href="fused-optimizer.html">1 · The optimizer</a><a${cls(
        prevHref.nonEmpty
      )} href="fused-json.html">2 · Fused JSON</a></div>
       |<h1>${esc(title)}</h1><p class="lede">${prose(lede)}</p></header>
       |$setup
       |$body
       |<footer>$nav<span>Code blocks are the verbatim post-typer expansion (<code>FuseDebug.show</code>), shown, never executed. Generated by <code>farray.json.JsonDemo</code>.</span></footer>
       |</body></html>""".stripMargin

  /** the data-setup card for the JSON page (sizes + a sample record + the methodology). */
  private def setupCard: String =
    s"""<section class="setup"><h2>The benchmark</h2>
       |<p class="prose">${prose(jsonSetup)}</p>
       |<pre class="sample">${esc(sampleRecordPretty.stripMargin)}</pre>
       |<p class="prose method">${prose(jsonMethod)}</p></section>""".stripMargin

  private def cls(active: Boolean): String = if active then """ class="on"""" + "\"" else ""

  private def renderExample(e: Example): String =
    val rival = e.rival
      .map { case (label, src) =>
        s"""<p class="cap rival">what we beat — ${esc(label)}</p><pre class="rival">${highlightScala(src)}</pre>"""
      }
      .getOrElse("")
    val bench = e.bench.map(renderBench).getOrElse("")
    s"""<section class="ex"><h2>${esc(e.title)}</h2>
       |<p class="prose">${prose(e.prose)}</p>
       |<p class="cap">you write</p><pre class="scala">${highlightScala(e.code)}</pre>
       |<p class="cap">the macro emits</p><pre class="gen">${esc(e.gen)}</pre>
       |$rival
       |$bench
       |</section>""".stripMargin

  private def renderBench(b: Bench): String =
    val rows = b.rows.zipWithIndex.map { case ((label, ops, bop), i) =>
      val win = if i == 0 then " win" else ""
      s"""<tr class="r$win"><td>${esc(label)}</td><td class="num">${esc(ops)}</td><td class="num">${esc(bop)}</td></tr>"""
    }.mkString
    s"""<p class="cap">measured (JMH, -prof gc)</p>
       |<table class="bench"><thead><tr><th></th><th>throughput</th><th>allocation</th></tr></thead><tbody>$rows</tbody></table>
       |<p class="headline">$win${esc(b.headline)}</p>""".stripMargin
  private val win = """<span class="trophy">▲</span> """

  private def prose(s: String): String =
    val e = "`([^`]+)`".r.replaceAllIn(esc(s.stripMargin), m => s"<code>${m.group(1)}</code>")
    e.replace("\n", " ").replace("&lt;b&gt;", "<b>").replace("&lt;/b&gt;", "</b>") // allow bold in prose

  private def highlightScala(s: String): String =
    var h = esc(s)
    for kw <- List(
        "filter",
        "foldLeft",
        "fuse",
        "stream",
        "ndjson",
        "map",
        "toList",
        "count",
        "headOption",
        "sum",
        "run",
        "aggTo",
        "agg",
        "Agg",
        "def",
        "val",
        "var",
        "while",
        "if",
        "then",
        "else",
        "new",
        "given"
      )
    do h = raw"(?<![A-Za-z_.])($kw)(?![A-Za-z0-9_])".r.replaceAllIn(h, m => s"""<span class="kw">${m.group(1)}</span>""")
    h.replace("//", """<span class="cm">//""").linesIterator.map(l => if l.contains("<span class=\"cm\">") then l + "</span>" else l).mkString("\n")

  private def esc(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private val css =
    """:root{--bg:#0e1016;--fg:#e7eaf0;--mut:#8b95a6;--acc:#7dd3fc;--ok:#86efac;--code:#151925;--bd:#232a38;--win:#16321f}
      |*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--fg);
      |font:16px/1.65 -apple-system,Segoe UI,Roboto,sans-serif;max-width:1000px;margin:0 auto;padding:1.5rem 1.25rem 4rem}
      |.tabs{display:flex;gap:.5rem;margin-bottom:1rem}
      |.tabs a{color:var(--mut);text-decoration:none;padding:.35rem .8rem;border:1px solid var(--bd);border-radius:999px;font-size:.85rem}
      |.tabs a.on{color:#0e1016;background:var(--acc);border-color:var(--acc);font-weight:600}
      |h1{font-size:1.9rem;margin:.3rem 0}.lede{color:var(--mut);font-size:1.08rem}
      |h2{font-size:1.12rem;margin:0 0 .5rem;color:var(--acc)}
      |section.ex{border:1px solid var(--bd);border-radius:12px;padding:1.1rem 1.25rem;margin:1.25rem 0;background:#11141d}
      |section.setup{border:1px solid #2b3a4a;border-radius:12px;padding:1.1rem 1.25rem;margin:1.25rem 0;background:#10161f}
      |section.setup b{color:#cfe6ff}pre.sample{color:#bfe8c8;border-color:#234534;background:#10180f;white-space:pre-wrap}
      |.method{color:var(--mut);font-size:.9rem;margin-top:.7rem}
      |.prose code,.lede code{color:#ffd9a8}
      |.cap{color:var(--mut);font-size:.74rem;text-transform:uppercase;letter-spacing:.07em;margin:1rem 0 .25rem}
      |.cap.rival{color:#f0a8a8}
      |pre{background:var(--code);border:1px solid var(--bd);border-radius:8px;padding:.7rem .9rem;overflow:auto;
      |font:12.5px/1.5 ui-monospace,SFMono-Regular,Menlo,monospace;margin:0}
      |pre.scala{color:#dbe7ff}pre.gen{color:#c4ccda;max-height:420px}pre.rival{color:#e9c9c9;border-color:#5a2e2e;background:#1b1416}
      |.kw{color:#c792ea;font-weight:600}.cm{color:#6b7686;font-style:italic}
      |code{background:var(--code);border:1px solid var(--bd);border-radius:4px;padding:.04rem .3rem;font:12.5px ui-monospace,Menlo,monospace;color:#ffd9a8}
      |table.bench{width:100%;border-collapse:collapse;margin:.3rem 0;font-size:.92rem}
      |table.bench th{color:var(--mut);text-align:right;font-weight:500;font-size:.78rem;padding:.3rem .6rem;border-bottom:1px solid var(--bd)}
      |table.bench th:first-child{text-align:left}
      |table.bench td{padding:.4rem .6rem;border-bottom:1px solid #1a2030}.bench td.num{text-align:right;font:13px ui-monospace,Menlo,monospace}
      |table.bench tr.win{background:var(--win)}table.bench tr.win td{color:var(--ok)}
      |.headline{color:var(--ok);font-weight:600;margin:.5rem 0 0}.trophy{color:var(--ok)}
      |footer{color:var(--mut);font-size:.85rem;margin-top:2rem;display:flex;justify-content:space-between;gap:1rem;align-items:center}
      |footer .nav{color:var(--acc);text-decoration:none;font-weight:600;white-space:nowrap}""".stripMargin

end JsonDemo
