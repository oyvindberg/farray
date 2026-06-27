package farray

import scala.quoted.*

/** which terminal a fused pipeline ends in (the macro shares one lowering core across all of them). */
enum TTag:
  case ToFArray, Foreach, Fold, Count, Find, Exists, Forall, HeadOption, Head

/**
 * Macro implementation for fused pipelines (see `Fuse` and `docs/fused-pipeline-design.md`).
 *
 * A terminal receives `'this` = the `Expr` of the whole receiver `xs.fuse.map(f).flatMap(g).take(k)…`. We peel
 * that AST back to the source `new Fuse(src)`, collect the stage list, and lower it to ONE fused loop nest:
 *
 *  - every source / flatMap-inner level is traversed once (leaf fast-path + `<kind>At` fallback), specialized
 *    over the four op-kinds (Int/Long/Double/Ref);
 *  - `map`/`filter`/`take`/`drop` are straight-line binds, guards and counters; `flatMap` opens a nested loop,
 *    so everything downstream of it runs inside that inner loop;
 *  - lambdas are beta-reduced into the body, so no `Function1` is allocated or called per element;
 *  - a single `done` flag (needed by `take` and by the short-circuit terminals `find`/`exists`/`forall`/`head`)
 *    is threaded onto every loop condition, so a match escapes all nesting levels and the source is read no
 *    further than necessary;
 *  - output goes straight into one array — preallocated to `min(srcLen, take-limits)` when the pipeline can
 *    only shrink, or grown via `ensureCap` when a `flatMap` can expand it — then trimmed. No intermediate
 *    `FArray` per stage.
 */
object FuseMacro:

  // ---------- entry points (one per terminal) ----------
  def toFArrayImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[FArray[A]] =
    '{ ${ core[A](self, TTag.ToFArray, Nil) }.asInstanceOf[FArray[A]] }

  def foreachImpl[A: Type](self: Expr[Fuse[A]], f: Expr[A => Unit])(using Quotes): Expr[Unit] =
    '{ ${ core[A](self, TTag.Foreach, List(f)) }.asInstanceOf[Unit] }

  def foldLeftImpl[A: Type, Z: Type](self: Expr[Fuse[A]], z: Expr[Z], op: Expr[(Z, A) => Z])(using Quotes): Expr[Z] =
    '{ ${ core[A](self, TTag.Fold, List(z, op)) }.asInstanceOf[Z] }

  def countImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[Int] =
    '{ ${ core[A](self, TTag.Count, Nil) }.asInstanceOf[Int] }

  def findImpl[A: Type](self: Expr[Fuse[A]], p: Expr[A => Boolean])(using Quotes): Expr[Option[A]] =
    '{ ${ core[A](self, TTag.Find, List(p)) }.asInstanceOf[Option[A]] }

  def existsImpl[A: Type](self: Expr[Fuse[A]], p: Expr[A => Boolean])(using Quotes): Expr[Boolean] =
    '{ ${ core[A](self, TTag.Exists, List(p)) }.asInstanceOf[Boolean] }

  def forallImpl[A: Type](self: Expr[Fuse[A]], p: Expr[A => Boolean])(using Quotes): Expr[Boolean] =
    '{ ${ core[A](self, TTag.Forall, List(p)) }.asInstanceOf[Boolean] }

  def headOptionImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[Option[A]] =
    '{ ${ core[A](self, TTag.HeadOption, Nil) }.asInstanceOf[Option[A]] }

  def headImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[A] =
    '{ ${ core[A](self, TTag.Head, Nil) }.asInstanceOf[A] }

  // ---------- shared lowering core (Exprs cross the boundary; Terms are derived inside) ----------
  private def core[A: Type](self: Expr[Fuse[A]], tag: TTag, extraExprs: List[Expr[Any]])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    // --- stage model ---
    sealed trait Stage
    final case class MapS(f: Term) extends Stage
    final case class FilterS(p: Term, negate: Boolean) extends Stage
    final case class TakeS(n: Term) extends Stage
    final case class DropS(n: Term) extends Stage
    final case class FlatMapS(f: Term, bTpe: TypeRepr) extends Stage
    case object ZipWithIndexS extends Stage

    enum Kind:
      case KInt, KLong, KDouble, KRef
    // Specialize-or-fail, mirroring the eager API's `Repr` evidence (RefRepr requires A <: AnyRef): a primitive
    // kind needs the EXACT type; a reference kind needs `<: AnyRef`. Anything else (Any/AnyVal/Matchable, an
    // unbounded type param) can't be read unboxed — a primitive-backed FArray covariantly widened to such a
    // type would `isInstanceOf[RefArr]`-miss and read nulls — so reject it at compile time rather than miscompile.
    def kindOf(t: TypeRepr): Kind =
      if t =:= TypeRepr.of[Int] then Kind.KInt
      else if t =:= TypeRepr.of[Long] then Kind.KLong
      else if t =:= TypeRepr.of[Double] then Kind.KDouble
      else if t <:< TypeRepr.of[AnyRef] then Kind.KRef
      else report.errorAndAbort(
        s"fuse: cannot specialize element type `${t.show}` — fused pipelines support Int, Long, Double, or a " +
        s"reference type (`<: AnyRef`). A primitive-backed FArray widened to Any/AnyVal can't be read unboxed; " +
        s"use a concrete element type.")

    // --- loop-state handles (Exprs that read/mutate vars declared above the loop) ---
    final case class Done(set: Expr[Unit], read: Expr[Boolean])
    // an above-loop counter; `lim` is present for take/drop (their clamped limit), absent for zipWithIndex.
    final case class Slot(read: Expr[Int], inc: Expr[Unit], lim: Option[Expr[Int]])
    final case class Ctx(consume: Term => Term, done: Option[Done], counters: Map[Int, Slot])

    // The value flowing through a pure map/filter segment, decomposed into independent columns. A `Sc`'s `read`
    // is CPS: it binds the column to a fresh val on FIRST read (memoized, so no recomputation) and yields the
    // ref — so a column read only inside a guard binds there, i.e. computes only for survivors (the sink).
    sealed trait Shape
    final case class Sc(read: (Term => Term) => Term) extends Shape
    final case class Tup(parts: List[Shape]) extends Shape

    val unit: Term = '{ () }.asTerm

    // --- AST helpers ---
    def unwrap(t: Term): Term = t match
      case Inlined(_, _, e) => unwrap(e)
      case Typed(e, _)      => unwrap(e)
      case Block(Nil, e)    => unwrap(e)
      case _                => t

    def fuseElem(t: TypeRepr): TypeRepr = t.widen.dealias match
      case AppliedType(_, List(a)) => a
      case other                   => report.errorAndAbort(s"fuse: expected Fuse[_], got ${other.show}")

    def parse(t0: Term, acc: List[Stage]): (Term, TypeRepr, List[Stage]) =
      unwrap(t0) match
        case Apply(TypeApply(Select(prev, "map"), _), List(f))            => parse(prev, MapS(unwrap(f)) :: acc)
        case Apply(TypeApply(Select(prev, "flatMap"), List(b)), List(f))  => parse(prev, FlatMapS(unwrap(f), b.tpe) :: acc)
        case Apply(Select(prev, "filter"), List(p))                       => parse(prev, FilterS(unwrap(p), false) :: acc)
        case Apply(Select(prev, "filterNot"), List(p))                    => parse(prev, FilterS(unwrap(p), true) :: acc)
        case Apply(Select(prev, "take"), List(n))                         => parse(prev, TakeS(unwrap(n)) :: acc)
        case Apply(Select(prev, "drop"), List(n))                         => parse(prev, DropS(unwrap(n)) :: acc)
        case Select(prev, "zipWithIndex")                                 => parse(prev, ZipWithIndexS :: acc)
        case base @ Apply(Select(New(_), "<init>"), List(src))               => (src, fuseElem(base.tpe), acc)
        case base @ Apply(TypeApply(Select(New(_), "<init>"), _), List(src)) => (src, fuseElem(base.tpe), acc)
        case other =>
          report.errorAndAbort(s"fuse: unsupported pipeline step near:\n${other.show(using Printer.TreeStructure)}")

    /** beta-reduce `fn(args…)` to inline the lambda body (no closure); falls back to a real `.apply` call. */
    def applyN(fn: Term, args: List[Term]): Term =
      val app = Apply(Select.unique(fn, "apply"), args)
      Term.betaReduce(app).getOrElse(app)
    def applyLambda(fn: Term, arg: Term): Term = applyN(fn, List(arg))

    /** `{ val v = value; cont(v) }` — binds `value` to a fresh val (no recompute; owners handled by the quote). */
    def letBind(value: Term)(cont: Term => Term): Term =
      value.tpe.widen.asType match
        case '[t] => '{ val v: t = ${ value.asExprOf[t] }; ${ cont('v.asTerm).asExprOf[Unit] } }.asTerm

    /** the `(value, index)` pair for zipWithIndex; use the stdlib @specialized Tuple2 for a primitive element
     *  (Int/Long/Double) so it isn't boxed — exactly like the eager zipWithIndex. Everything else boxes via a
     *  plain Tuple2 (correct for references and the rarer primitive kinds). */
    def tupleWithIndex(value: Term, idx: Expr[Int]): Term =
      val t = value.tpe.widen
      // The @specialized subclass stores its fields unboxed but isn't a static subtype of `(K, Int)` (it's a
      // bincompat synthetic); cast it (a runtime no-op — it really IS a Tuple2) so downstream `._1`/`._2`
      // project cleanly and stay unboxed.
      if t =:= TypeRepr.of[Int] then '{ new scala.Tuple2$mcII$sp(${ value.asExprOf[Int] }, $idx).asInstanceOf[(Int, Int)] }.asTerm
      else if t =:= TypeRepr.of[Long] then '{ new scala.Tuple2$mcJI$sp(${ value.asExprOf[Long] }, $idx).asInstanceOf[(Long, Int)] }.asTerm
      else if t =:= TypeRepr.of[Double] then '{ new scala.Tuple2$mcDI$sp(${ value.asExprOf[Double] }, $idx).asInstanceOf[(Double, Int)] }.asTerm
      else '{ scala.Tuple2(${ value.asExpr }, $idx) }.asTerm

    // --- parse ---
    val selfTerm = self.asTerm
    val aType = TypeRepr.of[A]
    val args = extraExprs.map(e => unwrap(e.asTerm))
    val (srcTerm, srcElem, stages) = parse(selfTerm.underlyingArgument, Nil)

    // identity → hand back the source (only meaningful for ToFArray).
    if stages.isEmpty && tag == TTag.ToFArray then
      return '{ ${ srcTerm.asExpr }.asInstanceOf[FBase] }

    val srcK = kindOf(srcElem)
    val outK = kindOf(aType)
    val srcBase: Expr[FBase] = '{ ${ srcTerm.asExpr }.asInstanceOf[FBase] }

    val indexed: List[(Stage, Int)] = stages.zipWithIndex
    // stages needing an above-loop counter; take/drop carry a clamped limit arg, zipWithIndex doesn't (None).
    val counterStages: List[(Int, Option[Term])] = indexed.collect {
      case (TakeS(n), i)     => (i, Some(n))
      case (DropS(n), i)     => (i, Some(n))
      case (ZipWithIndexS, i) => (i, None)
    }
    val takesPresent: Boolean = stages.exists(_.isInstanceOf[TakeS])
    val hasFlatMap: Boolean = stages.exists(_.isInstanceOf[FlatMapS])
    val shortCircuit: Boolean = tag match
      case TTag.Find | TTag.Exists | TTag.Forall | TTag.HeadOption | TTag.Head => true
      case _                                                                   => false
    val needsDone: Boolean = takesPresent || shortCircuit

    // ===== Layer B: pure map/filter segment optimizer (compute-for-survivors via memoized lazy columns) =====

    /** a column that binds to a val on first read (memoized), then yields the ref. */
    def memoScalar(compute: (Term => Term) => Term): Shape =
      var bound: Option[Term] = None
      Sc(k => bound match
        case Some(r) => k(r)
        case None    => compute(v => letBind(v)(r => { bound = Some(r); k(r) })))

    def srcShape(cur: Term): Shape = Sc(k => k(cur)) // already bound by the loop / an upstream stage

    /** materialize a shape to a single Term (binding columns as needed); tuples re-tuple (boxes, like eager). */
    def readShape(shape: Shape)(k: Term => Term): Term = shape match
      case Sc(read)   => read(k)
      case Tup(parts) => readAll(parts, Nil)(ts => k(mkTuple(ts)))
    def readAll(parts: List[Shape], acc: List[Term])(k: List[Term] => Term): Term = parts match
      case Nil       => k(acc.reverse)
      case p :: rest => readShape(p)(t => readAll(rest, t :: acc)(k))
    def mkTuple(ts: List[Term]): Term = ts match
      case List(a, b)    => '{ scala.Tuple2(${ a.asExpr }, ${ b.asExpr }) }.asTerm
      case List(a, b, c) => '{ scala.Tuple3(${ a.asExpr }, ${ b.asExpr }, ${ c.asExpr }) }.asTerm
      case _             => report.errorAndAbort("fuse: only tuple arities 2 and 3 are supported in fused projections")

    // --- AST shape detection / substitution ---
    def decomposeLambda(t: Term): Option[(Symbol, Term)] = unwrap(t) match
      case Lambda(List(vd), body) => Some((vd.symbol, body))
      case _                      => None
    def isProj(name: String): Boolean = name.length > 1 && name(0) == '_' && name.drop(1).forall(_.isDigit)
    def isProjection(t: Term): Option[(Term, Int)] = t match
      case Select(inner, name) if isProj(name) => Some((inner, name.drop(1).toInt - 1))
      case _                                    => None
    def isTupleLiteral(t: Term): Option[List[Term]] =
      def ok(obj: Term, args: List[Term]) =
        if (args.length == 2 || args.length == 3) && obj.symbol.fullName.startsWith("scala.Tuple") then Some(args) else None
      t match
        case Apply(TypeApply(Select(obj, "apply"), _), args) => ok(obj, args)
        case Apply(Select(obj, "apply"), args)               => ok(obj, args)
        case _                                               => None
    // a maximal projection PATH rooted at a symbol: `p._1._2` -> (p, List(0,1)); a bare `p` -> (p, Nil).
    def projPath(t: Term): Option[(Symbol, List[Int])] = t match
      case id: Ident                            => Some((id.symbol, Nil))
      case Select(inner, name) if isProj(name)  => projPath(inner).map((s, p) => (s, p :+ (name.drop(1).toInt - 1)))
      case _                                    => None
    /** every maximal projection path from `param` in `body` (Nil path = param used whole). */
    def collectPaths(body: Term, param: Symbol): List[List[Int]] =
      val buf = scala.collection.mutable.LinkedHashSet.empty[List[Int]]
      (new TreeTraverser:
        override def traverseTree(t: Tree)(owner: Symbol): Unit = t match
          case (_: Ident | _: Select) if projPath(t.asInstanceOf[Term]).exists((s, _) => s == param) =>
            buf += projPath(t.asInstanceOf[Term]).get._2 // maximal: don't descend into inner projections
          case _ => super.traverseTree(t)(owner)
      ).traverseTree(body)(Symbol.spliceOwner)
      buf.toList
    def substParam(body: Term, param: Symbol, repl: Term): Term =
      (new TreeMap:
        override def transformTerm(t: Term)(owner: Symbol): Term = t match
          case id: Ident if id.symbol == param => repl
          case _                                => super.transformTerm(t)(owner)
      ).transformTerm(body)(Symbol.spliceOwner)
    /** replace each maximal param-projection path with its resolved column ref. */
    def substPaths(body: Term, param: Symbol, refs: Map[List[Int], Term]): Term =
      (new TreeMap:
        override def transformTerm(t: Term)(owner: Symbol): Term = projPath(t) match
          case Some((s, p)) if s == param && refs.contains(p) => refs(p)
          case _                                              => super.transformTerm(t)(owner)
      ).transformTerm(body)(Symbol.spliceOwner)
    /** navigate a shape down a projection path to the leaf column (no intermediate tuple is materialized). */
    def navigate(cur: Shape, path: List[Int]): Shape = path match
      case Nil     => cur
      case j :: rest => cur match
        case Tup(ps) if j >= 0 && j < ps.length => navigate(ps(j), rest)
        case other                              => navigate(projScalar(other, j), rest)

    // --- interpret a map lambda body symbolically into a Shape (decomposing tuples, resolving projections) ---
    def interp(body: Term, param: Symbol, cur: Shape): Shape =
      isTupleLiteral(body) match
        case Some(parts) => Tup(parts.map(interp(_, param, cur)))
        case None => isProjection(body) match
          case Some((inner, j)) => interp(inner, param, cur) match
            case Tup(ps) if j >= 0 && j < ps.length => ps(j)
            case sc                                  => projScalar(sc, j)
          case None => body match
            case id: Ident if id.symbol == param => cur
            case _                               => memoScalar(k => substColumns(body, param, cur)(k))
    def projScalar(sc: Shape, j: Int): Shape =
      memoScalar(k => readShape(sc)(t => k(Select.unique(t, "_" + (j + 1)))))
    /** substitute param-uses in an atomic body with the columns it reads (binding only those columns). A bare
     *  param use (Nil path) forces materializing the whole shape; otherwise each path resolves to a leaf column. */
    def substColumns(body: Term, param: Symbol, cur: Shape)(k: Term => Term): Term =
      val paths = collectPaths(body, param)
      if paths.contains(Nil) then readShape(cur)(ct => k(substParam(body, param, ct)))
      else readPaths(cur, paths, Map.empty)(refs => k(substPaths(body, param, refs)))
    def readPaths(cur: Shape, paths: List[List[Int]], acc: Map[List[Int], Term])(k: Map[List[Int], Term] => Term): Term =
      paths match
        case Nil       => k(acc)
        case p :: rest => readShape(navigate(cur, p))(ref => readPaths(cur, rest, acc + (p -> ref))(k))

    def applyMap(f: Term, cur: Shape): Shape = decomposeLambda(f) match
      case Some((param, body)) => interp(body, param, cur)
      case None                => memoScalar(k => readShape(cur)(ct => k(applyLambda(f, ct))))

    /** read a filter predicate (binding only the columns it touches), then hand the Boolean term to `k`. */
    def predShape(p: Term, neg: Boolean, cur: Shape)(k: Term => Term): Term =
      def fin(b: Term): Term = k(if neg then '{ !${ b.asExprOf[Boolean] } }.asTerm else b)
      decomposeLambda(p) match
        case Some((param, body)) => readShape(interp(body, param, cur))(fin)
        case None                => readShape(cur)(ct => fin(applyLambda(p, ct)))

    /** lower a contiguous map/filter run; columns sink past filters automatically (lazy memoized reads). */
    def buildSegment(ss: List[Stage], cur: Shape)(k: Shape => Term): Term = ss match
      case Nil                  => k(cur)
      case MapS(f) :: rest      => buildSegment(rest, applyMap(f, cur))(k)
      case FilterS(p, neg) :: rest => predShape(p, neg, cur)(b => If(b, buildSegment(rest, cur)(k), unit))
      case _                    => k(cur) // segment is map/filter only

    // --- per-element body: map/filter/take/drop/flatMap chain, ending in `ctx.consume` ---
    def buildBody(ss: List[(Stage, Int)], cur: Term, ctx: Ctx): Term = ss match
      case Nil => ctx.consume(cur)
      case (MapS(_) | FilterS(_, _), _) :: _ =>
        val (seg, rest) = ss.span { case (MapS(_), _) => true; case (FilterS(_, _), _) => true; case _ => false }
        buildSegment(seg.map(_._1), srcShape(cur))(finalShape => readShape(finalShape)(t => buildBody(rest, t, ctx)))
      case (FlatMapS(f, bTpe), _) :: rest =>
        // open a nested loop over f(cur): everything downstream runs inside it.
        val inner: Expr[FBase] = '{ ${ applyLambda(f, cur).asExpr }.asInstanceOf[FBase] }
        loopOver(inner, kindOf(bTpe), bTpe, rest, ctx).asTerm
      case (TakeS(_), i) :: rest =>
        val sl = ctx.counters(i); val lim = sl.lim.get; val d = ctx.done.get
        '{ val cv = ${ sl.read }
           if cv >= ${ lim } then ${ d.set }
           else { ${ buildBody(rest, cur, ctx).asExprOf[Unit] }; ${ sl.inc }; if cv + 1 >= ${ lim } then ${ d.set } } }.asTerm
      case (DropS(_), i) :: rest =>
        val sl = ctx.counters(i)
        '{ if ${ sl.read } < ${ sl.lim.get } then ${ sl.inc }
           else ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }.asTerm
      case (ZipWithIndexS, i) :: rest =>
        // pair the element with the current index, then advance the index (once per element reaching here).
        val sl = ctx.counters(i)
        letBind(tupleWithIndex(cur, sl.read)) { t =>
          '{ ${ sl.inc }; ${ buildBody(rest, t, ctx).asExprOf[Unit] } }.asTerm
        }

    // --- traverse one level (source OR a flatMap inner): leaf fast-path + `<kind>At` fallback, `done` break ---
    def loopOver(src: Expr[FBase], k: Kind, elemTpe: TypeRepr, ss: List[(Stage, Int)], ctx: Ctx): Expr[Unit] =
      def cond(len: Expr[Int], i: Expr[Int]): Expr[Boolean] = ctx.done match
        case Some(d) => '{ $i < $len && !${ d.read } }
        case None    => '{ $i < $len }
      def perElem(read: Term): Expr[Unit] =
        letBind(read)(x => buildBody(ss, x, ctx)).asExprOf[Unit]
      elemTpe.asType match
        case '[se] => k match
          case Kind.KInt =>
            '{ val s = $src; val len = s.length
               if s.isInstanceOf[IntArr] then {
                 val a = s.asInstanceOf[IntArr].arr; var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.intAt(s, i) }.asTerm) }; i += 1 }
               } }
          case Kind.KLong =>
            '{ val s = $src; val len = s.length
               if s.isInstanceOf[LongArr] then {
                 val a = s.asInstanceOf[LongArr].arr; var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.longAt(s, i) }.asTerm) }; i += 1 }
               } }
          case Kind.KDouble =>
            '{ val s = $src; val len = s.length
               if s.isInstanceOf[DoubleArr] then {
                 val a = s.asInstanceOf[DoubleArr].arr; var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.doubleAt(s, i) }.asTerm) }; i += 1 }
               } }
          case Kind.KRef =>
            '{ val s = $src; val len = s.length
               if s.isInstanceOf[RefArr] then {
                 val a = s.asInstanceOf[RefArr].arr; var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ a(i).asInstanceOf[se] }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.refAt(s, i).asInstanceOf[se] }.asTerm) }; i += 1 }
               } }

    // --- declare loop-state vars (above the loop), then assemble the terminal body ---
    def withDone(k: Option[Done] => Term): Term =
      if needsDone then '{ var done: Boolean = false; ${ k(Some(Done('{ done = true }, '{ done }))).asExpr } }.asTerm
      else k(None)

    def declareSlots(rem: List[(Int, Option[Term])], acc: Map[Int, Slot])(k: Map[Int, Slot] => Term): Term =
      rem match
        case Nil => k(acc)
        case (idx, Some(argT)) :: rest => // take/drop: a clamped limit val + a counter
          '{ val lim: Int = { val t = ${ argT.asExprOf[Int] }; if t < 0 then 0 else t }
             var c: Int = 0
             ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, Some('{ lim }))))(k).asExpr } }.asTerm
        case (idx, None) :: rest =>      // zipWithIndex: a counter only
          '{ var c: Int = 0
             ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, None)))(k).asExpr } }.asTerm

    /** static upper bound on output length = min(source length, every `take` limit). */
    def capExpr(n0: Expr[Int], counters: Map[Int, Slot]): Expr[Int] =
      indexed.collect { case (TakeS(_), i) => counters(i).lim.get }.foldLeft(n0)((a, l) => '{ java.lang.Math.min($a, $l) })

    def assemble(src0: Expr[FBase], n0: Expr[Int], done: Option[Done], counters: Map[Int, Slot]): Term =
      def ctx(consume: Term => Term) = Ctx(consume, done, counters)
      def loop(consume: Term => Term): Expr[Unit] = loopOver(src0, srcK, srcElem, indexed, ctx(consume))
      tag match
        case TTag.Count =>
          '{ var cnt = 0; ${ loop(_ => '{ cnt += 1 }.asTerm) }; cnt }.asTerm
        case TTag.Foreach =>
          val f = args(0)
          '{ ${ loop(v => applyLambda(f, v)) }; () }.asTerm
        case TTag.Fold =>
          val z = args(0); val op = args(1)
          z.tpe.widen.asType match
            case '[zz] =>
              '{ var acc: zz = ${ z.asExprOf[zz] }
                 ${ loop(v => '{ acc = ${ applyN(op, List('{ acc }.asTerm, v)).asExprOf[zz] } }.asTerm) }
                 acc }.asTerm
        case TTag.Find =>
          // build with Option[Any] (no `A` inside the deep nested quote — keeps Type[A] evidence from leaking);
          // the entry method casts back to Option[A].
          val p = args(0); val d = done.get
          '{ var res: Option[Any] = None
             ${ loop(v => '{ if ${ applyLambda(p, v).asExprOf[Boolean] } then { res = Some(${ v.asExpr }); ${ d.set } } }.asTerm) }
             res }.asTerm
        case TTag.Exists =>
          val p = args(0); val d = done.get
          '{ var res = false
             ${ loop(v => '{ if ${ applyLambda(p, v).asExprOf[Boolean] } then { res = true; ${ d.set } } }.asTerm) }
             res }.asTerm
        case TTag.Forall =>
          val p = args(0); val d = done.get
          '{ var res = true
             ${ loop(v => '{ if !${ applyLambda(p, v).asExprOf[Boolean] } then { res = false; ${ d.set } } }.asTerm) }
             res }.asTerm
        case TTag.HeadOption =>
          val d = done.get
          '{ var res: Option[Any] = None
             ${ loop(v => '{ res = Some(${ v.asExpr }); ${ d.set } }.asTerm) }
             res }.asTerm
        case TTag.Head =>
          val d = done.get
          '{ var res: Option[Any] = None
             ${ loop(v => '{ res = Some(${ v.asExpr }); ${ d.set } }.asTerm) }
             res.getOrElse(throw new java.util.NoSuchElementException("head of empty fused pipeline")) }.asTerm
        case TTag.ToFArray => assembleOut(src0, n0, counters, ctx)

    // output array assembly: grow via ensureCap when a flatMap can expand it, else preallocate the upper bound.
    def assembleOut(src0: Expr[FBase], n0: Expr[Int], counters: Map[Int, Slot], ctx: (Term => Term) => Ctx): Term =
      def loop(consume: Term => Term): Expr[Unit] = loopOver(src0, srcK, srcElem, indexed, ctx(consume))
      outK match
        case Kind.KInt =>
          if hasFlatMap then
            '{ var out = new Array[Int](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapInt(out, o + 1); out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm) }
               if o == 0 then (IntArr.EMPTY: FBase) else if o == out.length then new IntArr(out, o) else new IntArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Int](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm) }
               if o == 0 then (IntArr.EMPTY: FBase) else if o == cap then new IntArr(out, o) else new IntArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
        case Kind.KLong =>
          if hasFlatMap then
            '{ var out = new Array[Long](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapLong(out, o + 1); out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm) }
               if o == 0 then (LongArr.EMPTY: FBase) else if o == out.length then new LongArr(out, o) else new LongArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Long](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm) }
               if o == 0 then (LongArr.EMPTY: FBase) else if o == cap then new LongArr(out, o) else new LongArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
        case Kind.KDouble =>
          if hasFlatMap then
            '{ var out = new Array[Double](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapDouble(out, o + 1); out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm) }
               if o == 0 then (DoubleArr.EMPTY: FBase) else if o == out.length then new DoubleArr(out, o) else new DoubleArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Double](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm) }
               if o == 0 then (DoubleArr.EMPTY: FBase) else if o == cap then new DoubleArr(out, o) else new DoubleArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
        case Kind.KRef =>
          if hasFlatMap then
            '{ var out = new Array[Object](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapRef(out, o + 1); out(o) = ${ v.asExpr }.asInstanceOf[Object]; o += 1 }.asTerm) }
               if o == 0 then (RefArr.EMPTY: FBase) else if o == out.length then new RefArr(out, o) else new RefArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Object](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExpr }.asInstanceOf[Object]; o += 1 }.asTerm) }
               if o == 0 then (RefArr.EMPTY: FBase) else if o == cap then new RefArr(out, o) else new RefArr(java.util.Arrays.copyOf(out, o), o) }.asTerm

    // --- final assembly: bind source + length once, then state vars, then the loop nest ---
    '{ val src0: FBase = $srcBase; val n0: Int = src0.length
       ${ withDone(done => declareSlots(counterStages, Map.empty)(counters => assemble('src0, 'n0, done, counters))).asExpr } }
