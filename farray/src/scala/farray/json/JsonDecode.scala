package farray.json

import scala.quoted.*
import farray.{Ast, DecomposedInput, RecordColumns, Column}

/** The JSON byte decoder, lifted ENTIRELY out of the fusion engine. Given a [[DecomposedInput]] — the record type,
  * the runtime [[ByteRecordSource]], the pipeline's field-reading lambdas, and the engine's `continue` callback — it
  * emits a per-record projection scanner: only the live fields are scanned (projection pushdown / dead-column
  * elimination), a leading filter is evaluated at the byte level and the record abandoned if it fails (predicate
  * pushdown), a projected String is decoded lazily only for survivors (compute-for-survivors), and no per-record
  * object is allocated for reduce/project pipelines.
  *
  * It speaks to the engine ONLY through the SPI: it never names the engine's `Shape`/`Tup`/`Ctx`/`Stage`/
  * `continueShape`. Per record it produces a [[RecordColumns]] (Shape-free column reads) and hands it to
  * `in.continue`, which the engine turns into its own decomposed `Shape` and rejoins to the shared optimizer. The
  * byte primitives (`skipValue`/`keyEquals`/`scanStringEnd`/`readIntAt`/…) live in [[JsonScanner]]; the pure AST
  * walks (`collectPaths`/`decomposeLambda`/…) live in [[Ast]].
  *
  * `lower` and `planString` are the only entry points; everything else is nested inside them so a single `Quotes`
  * (`q`) is shared across all the codegen helpers — no path-dependent-type threading.
  */
private[farray] object JsonDecode:

  /** the scanner kind for a field type — what the slot holds and how the column reads it. */
  private enum JKind { case JInt, JLong, JDouble, JString }

  /** drive the [[ByteRecordSource]] contract — an OUTER chunk loop (`nextChunk`) wrapping an INNER record loop
    * (`nextRecord`), both `done`-gated, in `try/finally close()`. Each complete record is decoded to its live-field
    * columns and handed to the engine's continuation; the framer behind the contract owns boundary stitching, so
    * this loop only ever sees complete, contiguous frames.
    */
  def lower(using q: Quotes)(in: DecomposedInput[q.type]): Expr[Unit] =
    import q.reflect.*

    def jkindOf(t: TypeRepr): JKind =
      if t =:= TypeRepr.of[Int] then JKind.JInt
      else if t =:= TypeRepr.of[Long] then JKind.JLong
      else if t =:= TypeRepr.of[Double] then JKind.JDouble
      else if t =:= TypeRepr.of[String] then JKind.JString
      else report.errorAndAbort(s"fuse-json (v1): unsupported field type ${t.show} (only Int/Long/Double/String).")

    /** a typed default for a dead (never-read) field's placeholder column. */
    def defaultOf(t: TypeRepr): Term = jkindOf(t) match
      case JKind.JInt    => '{ 0 }.asTerm
      case JKind.JLong   => '{ 0L }.asTerm
      case JKind.JDouble => '{ 0.0 }.asTerm
      case JKind.JString => '{ null.asInstanceOf[String] }.asTerm

    /** a live field's mutable scan slots: the lazy value read (the `v` var, or a String decode), a `seen` flag, a
      * `fill(valuePos, end): newPos` quote the scan-pass runs when this field's key matches, and `rawRead` — the
      * DIRECT value term used to evaluate a leading predicate inline (the slots are already bound, no `letBind`).
      */
    final case class JSlot(idx: Int, name: String, jk: JKind, read: (Term => Term) => Term, seen: Expr[Boolean], fill: (Expr[Int], Expr[Int]) => Expr[Int], rawRead: Term)

    val (fields, liveSet) = liveFields(in)
    val liveFs = fields.zipWithIndex.filter { case ((n, _), _) => liveSet.contains(n) }
    val notDone = in.notDone

    // declare the live fields' slot vars (nested quotes), bind their scan/read, and hand the slot list to `k`.
    def withSlots(buf: Expr[Array[Byte]], rem: List[((String, TypeRepr), Int)], acc: List[JSlot])(k: List[JSlot] => Term): Term =
      rem match
        case Nil => k(acc.reverse)
        case ((name, tpe), idx) :: rest =>
          jkindOf(tpe) match
            // numeric fills decode the value into the slot var + the new position into `pNext` — NO tuple returned
            // (a `(Double,Int)` return boxed, costing the macro path its alloc edge).
            case JKind.JInt =>
              '{
                var v: Int = 0; var pNext: Int = 0; var seen: Boolean = false
                ${
                  val s = JSlot(idx, name, JKind.JInt, kk => kk('{ v }.asTerm), '{ seen },
                    (p, end) => '{ v = JsonScanner.readIntAt($buf, $p, $end); pNext = JsonScanner.numEnd; seen = true; pNext }, '{ v }.asTerm)
                  withSlots(buf, rest, s :: acc)(k).asExpr
                }
              }.asTerm
            case JKind.JLong =>
              '{
                var v: Long = 0L; var pNext: Int = 0; var seen: Boolean = false
                ${
                  val s = JSlot(idx, name, JKind.JLong, kk => kk('{ v }.asTerm), '{ seen },
                    (p, end) => '{ v = JsonScanner.readLongAt($buf, $p, $end); pNext = JsonScanner.numEnd; seen = true; pNext }, '{ v }.asTerm)
                  withSlots(buf, rest, s :: acc)(k).asExpr
                }
              }.asTerm
            case JKind.JDouble =>
              '{
                var v: Double = 0.0; var pNext: Int = 0; var seen: Boolean = false
                ${
                  val s = JSlot(idx, name, JKind.JDouble, kk => kk('{ v }.asTerm), '{ seen },
                    (p, end) => '{ v = JsonScanner.readDoubleAt($buf, $p, $end); pNext = JsonScanner.numEnd; seen = true; pNext }, '{ v }.asTerm)
                  withSlots(buf, rest, s :: acc)(k).asExpr
                }
              }.asTerm
            case JKind.JString =>
              // lazy slice: slots are (start, len); the column decodes to String ON READ (the engine memoizes it),
              // so the sink defers the `new String` past the filter — compute-for-survivors, for free.
              '{
                var st: Int = 0; var ln: Int = 0; var seen: Boolean = false
                ${
                  val s = JSlot(idx, name, JKind.JString, kk => kk('{ JsonScanner.decodeLatin1($buf, st, ln) }.asTerm), '{ seen },
                    (p, end) => '{ val vs = $p + 1; val ve = JsonScanner.scanStringEnd($buf, vs, $end); st = vs; ln = ve - vs; seen = true; ve + 1 },
                    '{ JsonScanner.decodeLatin1($buf, st, ln) }.asTerm)
                  withSlots(buf, rest, s :: acc)(k).asExpr
                }
              }.asTerm

    // the one-pass key-dispatch scan over a record's fields (compact NDJSON fast path). `earlyPred` = the leading-
    // filter check folded in: the moment its fields are all captured, evaluate it; on failure `reject` + stop.
    def scanRecord(buf: Expr[Array[Byte]], lineStart: Expr[Int], lineEnd: Expr[Int], slots: List[JSlot], keyRefs: Map[String, Expr[Array[Byte]]],
        earlyPred: Option[(Expr[Boolean], Expr[Boolean])], reject: Expr[Unit]): Expr[Unit] =
      def dispatch(ks: Expr[Int], ke: Expr[Int], p: Expr[Int]): Expr[Int] =
        slots.foldRight[Expr[Int]]('{ JsonScanner.skipValue($buf, $p, $lineEnd) }) { (s, elseB) =>
          '{ if ! ${ s.seen } && JsonScanner.keyEquals($buf, $ks, $ke, ${ keyRefs(s.name) }) then ${ s.fill(p, lineEnd) } else $elseB }
        }
      // projection EARLY-OUT: once every live field is captured, abandon the rest (nothing else is read).
      def allSeen: Expr[Boolean] = slots match
        case Nil           => '{ false } // no live fields → never early-out (scan to '}' normally)
        case first :: more => more.foldLeft(first.seen)((acc, s) => '{ $acc && ${ s.seen } })
      earlyPred match
        case None =>
          '{
            var p: Int = $lineStart + 1
            var open: Boolean = $buf($lineStart) == '{'
            while open do
              if $buf(p) == '}' then open = false
              else
                val ks = p + 1; val ke = JsonScanner.scanStringEnd($buf, ks, $lineEnd)
                p = ${ dispatch('ks, 'ke, '{ ke + 2 }) }
                if $allSeen then open = false
                else if $buf(p) == ',' then p += 1
                else open = false
          }
        case Some((allPredSeen, predHolds)) =>
          '{
            var p: Int = $lineStart + 1
            var open: Boolean = $buf($lineStart) == '{'
            var predChecked: Boolean = false
            while open do
              if $buf(p) == '}' then open = false
              else
                val ks = p + 1; val ke = JsonScanner.scanStringEnd($buf, ks, $lineEnd)
                p = ${ dispatch('ks, 'ke, '{ ke + 2 }) }
                if !predChecked && $allPredSeen then
                  predChecked = true
                  if ! $predHolds then { $reject; open = false } // PREDICATE FAILED → abandon the record now
                if open then
                  if $allSeen then open = false
                  else if $buf(p) == ',' then p += 1
                  else open = false
          }

    // one record: declare slots, compute the leading-filter early-out, scan, then hand the engine the decomposed
    // columns (live slots + dead-field defaults) to continue downstream.
    def decodeRecord(buf: Expr[Array[Byte]], lineStart: Expr[Int], lineEnd: Expr[Int], keyRefs: Map[String, Expr[Array[Byte]]]): Term =
      withSlots(buf, liveFs, Nil) { slots =>
        val byIdx = slots.map(s => s.idx -> s).toMap
        // the engine's continuation gets one column per field (live → slot read; dead → typed default).
        val columns: List[Column[q.type]] = fields.zipWithIndex.map { case ((_, ftpe), i) =>
          byIdx.get(i) match
            case Some(s) => Column[q.type](s.read, isString = s.jk == JKind.JString)
            case None    => val d = defaultOf(ftpe); Column[q.type](k => k(d), isString = false)
        }
        def continueDownstream: Expr[Unit] = in.continue(RecordColumns[q.type](columns))

        // ---- PREDICATE-FAIL EARLY-OUT ----
        // the LEADING run of positive filters can be evaluated DURING the scan, the moment their fields are captured;
        // a failure abandons the record before the projection-only fields are scanned. (Kept downstream too — a cheap
        // re-check on survivors — so correctness never depends on stripping them.)
        val leadingFilters: List[Term] = in.stageLambdas.takeWhile(s => s.isPositiveFilter).map(_.lambda)
        val predFieldIdxs: Set[Int] =
          leadingFilters.flatMap(p => Ast.decomposeLambda(p).toList.flatMap((param, b) => Ast.collectPaths(b, param).flatMap(_.headOption.map(_._1)))).toSet
        val predSlots = slots.filter(s => predFieldIdxs.contains(s.idx))
        val projOnly = slots.exists(s => !predFieldIdxs.contains(s.idx))
        val earlyPred: Option[(Expr[Boolean], Expr[Boolean])] =
          if leadingFilters.isEmpty || predSlots.isEmpty || !projOnly then None
          else
            val allPredSeen = predSlots.map(_.seen).reduce((a, b) => '{ $a && $b })
            // substitute each leading-filter field read with the slot's RAW value term (decoded var / String).
            def predBool(p: Term): Option[Expr[Boolean]] = Ast.decomposeLambda(p) match
              case Some((param, body)) =>
                val paths = Ast.collectPaths(body, param)
                val refs: Map[Ast.Path, Term] = paths.flatMap(path => path.headOption.flatMap { case (i, _) => byIdx.get(i).map(s => path -> s.rawRead) }).toMap
                if refs.size == paths.size then Some(Ast.substPaths(body, param, refs).asExprOf[Boolean]) else None
              case None => None
            val predBools = leadingFilters.map(predBool)
            if predBools.contains(None) then None else Some((allPredSeen, predBools.flatten.reduce((a, b) => '{ $a && $b })))

        earlyPred match
          case None =>
            '{ ${ scanRecord(buf, lineStart, lineEnd, slots, keyRefs, None, '{ () }) }; ${ continueDownstream } }.asTerm
          case some =>
            '{
              var rejected: Boolean = false
              ${ scanRecord(buf, lineStart, lineEnd, slots, keyRefs, some, '{ rejected = true }) }
              if !rejected then ${ continueDownstream }
            }.asTerm
      }

    // bind each live field's wanted-key bytes to a val ABOVE the loop (interned once) for zero-alloc per-record compares.
    def withKeyRefs(rem: List[((String, TypeRepr), Int)], acc: Map[String, Expr[Array[Byte]]])(k: Map[String, Expr[Array[Byte]]] => Expr[Unit]): Expr[Unit] =
      rem match
        case Nil => k(acc)
        case ((name, _), _) :: rest =>
          '{
            val keyBytes: Array[Byte] = JsonScanner.internKey(${ Expr(name) })
            ${ withKeyRefs(rest, acc + (name -> '{ keyBytes }))(k) }
          }

    '{
      val src = ${ in.src }
      ${
        withKeyRefs(liveFs, Map.empty) { keyRefs =>
          '{
            try
              while src.nextChunk() && $notDone do
                while src.nextRecord() && $notDone do
                  // read `buf` PER RECORD: a streaming framer may reallocate its working buffer to stitch a record
                  // across a block boundary, so a per-chunk cache could go stale. recordStart/End are relative to the
                  // buffer current AT THIS nextRecord(). (For the in-memory source `buf` is a stable field → free.)
                  val buf: Array[Byte] = src.buf
                  val recStart: Int = src.recordStart
                  val recEnd: Int = src.recordEnd
                  ${ decodeRecord('buf, 'recStart, 'recEnd, keyRefs).asExprOf[Unit] }
            finally src.close()
          }
        }
      }
    }

  // ── live-field analysis (projection pushdown) — shared by `lower` and `planString` ──────────────────────────
  /** the record fields + the live set (fields any stage/terminal lambda reads). `forcesWholeRecord` (topN/reduce/…)
    * or a whole-param use makes every field live.
    */
  private def liveFields(using q: Quotes)(in: DecomposedInput[q.type]): (List[(String, q.reflect.TypeRepr)], Set[String]) =
    import q.reflect.*
    val fields = Ast
      .productFields(in.srcElem)
      .getOrElse(report.errorAndAbort(s"fuse-json: ${in.srcElem.show} is not a case class (flat case-class records only in v1)"))
    val live = scala.collection.mutable.LinkedHashSet.empty[String]
    var wholeUse = in.forcesWholeRecord
    def addFromBody(param: Symbol, b: Term): Unit =
      val paths = Ast.collectPaths(b, param)
      if paths.contains(Nil) then wholeUse = true
      paths.foreach(path => path.headOption.foreach(h => live += h._2))
    in.stageLambdas.foreach(s =>
      Ast.decomposeLambda(s.lambda) match
        case Some((p, b)) => addFromBody(p, b)
        case None         => report.errorAndAbort(s"fuse-json: could not decompose a stage lambda: ${s.lambda.show}")
    )
    in.terminalRecordReaders.foreach((p, b) => addFromBody(p, b))
    val liveSet = if wholeUse then fields.map(_._1).toSet else live.toSet
    (fields, liveSet)

  // ── plan description (testing) ───────────────────────────────────────────────────────────────────────────────
  /** a machine-checkable DESCRIPTION of the plan the decoder built — tests assert on the STRUCTURE (which fields are
    * scanned, decoded vs sliced, the predicate/early-out set, rebuild, terminal) rather than on brittle code/values.
    */
  def planString(using q: Quotes)(in: DecomposedInput[q.type]): String =
    import q.reflect.*
    val (fields, liveSet) = liveFields(in)
    def jks(t: TypeRepr): JKind =
      if t =:= TypeRepr.of[Int] then JKind.JInt
      else if t =:= TypeRepr.of[Long] then JKind.JLong
      else if t =:= TypeRepr.of[Double] then JKind.JDouble
      else JKind.JString
    // the record is REBUILT iff a topN/reduce/extremumByElem forces it, or some lambda uses the param whole.
    def usesWhole(p: Symbol, b: Term): Boolean = Ast.collectPaths(b, p).contains(Nil)
    val whole = in.forcesWholeRecord ||
      in.stageLambdas.exists(s => Ast.decomposeLambda(s.lambda).exists((p, b) => usesWhole(p, b))) ||
      in.terminalRecordReaders.exists((p, b) => usesWhole(p, b))
    val live = fields.filter(f => liveSet.contains(f._1))
    val decoded = live.collect { case (n, t) if jks(t) != JKind.JString => n }
    val sliced = live.collect { case (n, t) if jks(t) == JKind.JString => n }
    val leadingFilters = in.stageLambdas.takeWhile(s => s.isPositiveFilter).map(_.lambda)
    val predNames =
      leadingFilters.flatMap(p => Ast.decomposeLambda(p).toList.flatMap((param, b) => Ast.collectPaths(b, param).flatMap(_.headOption.map(_._2)))).distinct
    val projOnly = live.map(_._1).exists(n => !predNames.contains(n))
    val earlyOut = leadingFilters.nonEmpty && predNames.nonEmpty && projOnly
    def s(xs: Iterable[String]) = xs.toList.sorted.mkString("[", ",", "]")
    s"JsonPlan(record=${in.srcElem.typeSymbol.name}, terminal=${in.terminalName}, " +
      s"live=${s(liveSet)}, decoded=${s(decoded)}, sliced=${s(sliced)}, " +
      s"predicate=${s(predNames)}, earlyOut=$earlyOut, rebuildsRecord=$whole)"
