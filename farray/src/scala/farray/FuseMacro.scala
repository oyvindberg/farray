package farray

import scala.quoted.*

/** which terminal a fused pipeline ends in (the macro shares one lowering core across all of them). */
enum TTag:
  case ToFArray, Foreach, Fold, Count

/**
 * Macro implementation for fused pipelines (see `Fuse` and `docs/fused-pipeline-design.md`).
 *
 * A terminal receives `'this` = the `Expr` of the whole receiver `xs.fuse.map(f).filter(p).take(k)…`. We peel
 * that AST back to the source `new Fuse(src)`, collect the stage list, and lower it to ONE fused loop:
 *
 *  - the source is traversed once (leaf fast-path + `<kind>At` fallback), all four op-kinds (Int/Long/Double/Ref);
 *  - `map`/`filter`/`filterNot`/`take`/`drop` become straight-line binds, guards and counters in the loop body;
 *  - lambdas are beta-reduced into the body, so no `Function1` is allocated or called per element;
 *  - `take` threads a `done` flag onto the loop condition, so positional pipelines stop early (read no more than
 *    they must), and a single output array (sized to the static upper bound) is filled and trimmed — no
 *    intermediate `FArray` per stage.
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

  // ---------- shared lowering core (Exprs cross the boundary; Terms are derived inside) ----------
  private def core[A: Type](self: Expr[Fuse[A]], tag: TTag, extraExprs: List[Expr[Any]])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    // --- stage model ---
    sealed trait Stage
    final case class MapS(f: Term) extends Stage
    final case class FilterS(p: Term, negate: Boolean) extends Stage
    final case class TakeS(n: Term) extends Stage
    final case class DropS(n: Term) extends Stage

    enum Kind:
      case KInt, KLong, KDouble, KRef
    def kindOf(t: TypeRepr): Kind =
      if t =:= TypeRepr.of[Int] then Kind.KInt
      else if t =:= TypeRepr.of[Long] then Kind.KLong
      else if t =:= TypeRepr.of[Double] then Kind.KDouble
      else Kind.KRef

    // --- loop-state handles (Exprs that read/mutate vars declared above the loop) ---
    final case class Done(set: Expr[Unit], read: Expr[Boolean])
    final case class Slot(read: Expr[Int], inc: Expr[Unit], lim: Expr[Int])
    final case class Ctx(consume: Term => Term, done: Option[Done], counters: Map[Int, Slot])

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
        case Apply(TypeApply(Select(prev, "map"), _), List(f)) => parse(prev, MapS(unwrap(f)) :: acc)
        case Apply(Select(prev, "filter"), List(p))            => parse(prev, FilterS(unwrap(p), false) :: acc)
        case Apply(Select(prev, "filterNot"), List(p))         => parse(prev, FilterS(unwrap(p), true) :: acc)
        case Apply(Select(prev, "take"), List(n))              => parse(prev, TakeS(unwrap(n)) :: acc)
        case Apply(Select(prev, "drop"), List(n))              => parse(prev, DropS(unwrap(n)) :: acc)
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
    val slots: List[(Int, Term)] = indexed.collect { case (TakeS(n), i) => (i, n); case (DropS(n), i) => (i, n) }
    val takesPresent: Boolean = stages.exists(_.isInstanceOf[TakeS])

    // --- per-element body: map/filter/take/drop chain, ending in `ctx.consume` ---
    def buildBody(ss: List[(Stage, Int)], cur: Term, ctx: Ctx): Term = ss match
      case Nil => ctx.consume(cur)
      case (MapS(f), _) :: rest => letBind(applyLambda(f, cur))(v => buildBody(rest, v, ctx))
      case (FilterS(p, neg), _) :: rest =>
        val raw  = applyLambda(p, cur)
        val cond = if neg then '{ !${ raw.asExprOf[Boolean] } }.asTerm else raw
        If(cond, buildBody(rest, cur, ctx), unit)
      case (TakeS(_), i) :: rest =>
        val sl = ctx.counters(i); val d = ctx.done.get
        '{ val cv = ${ sl.read }
           if cv >= ${ sl.lim } then ${ d.set }
           else { ${ buildBody(rest, cur, ctx).asExprOf[Unit] }; ${ sl.inc }; if cv + 1 >= ${ sl.lim } then ${ d.set } } }.asTerm
      case (DropS(_), i) :: rest =>
        val sl = ctx.counters(i)
        '{ if ${ sl.read } < ${ sl.lim } then ${ sl.inc }
           else ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }.asTerm

    // --- source traversal: leaf fast-path (unboxed array) + `<kind>At` fallback, with the `done` break ---
    def fillLoop(src0: Expr[FBase], n0: Expr[Int], ctx: Ctx): Expr[Unit] =
      def cond(i: Expr[Int]): Expr[Boolean] = ctx.done match
        case Some(d) => '{ $i < $n0 && !${ d.read } }
        case None    => '{ $i < $n0 }
      def perElem(read: Term): Expr[Unit] =
        letBind(read)(x => buildBody(indexed, x, ctx)).asExprOf[Unit]
      srcElem.asType match
        case '[se] => srcK match
          case Kind.KInt =>
            '{ if $src0.isInstanceOf[IntArr] then {
                 val a = $src0.asInstanceOf[IntArr].arr; var i = 0
                 while ${ cond('i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('i) } do { ${ perElem('{ FArrayOps.intAt($src0, i) }.asTerm) }; i += 1 }
               } }
          case Kind.KLong =>
            '{ if $src0.isInstanceOf[LongArr] then {
                 val a = $src0.asInstanceOf[LongArr].arr; var i = 0
                 while ${ cond('i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('i) } do { ${ perElem('{ FArrayOps.longAt($src0, i) }.asTerm) }; i += 1 }
               } }
          case Kind.KDouble =>
            '{ if $src0.isInstanceOf[DoubleArr] then {
                 val a = $src0.asInstanceOf[DoubleArr].arr; var i = 0
                 while ${ cond('i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('i) } do { ${ perElem('{ FArrayOps.doubleAt($src0, i) }.asTerm) }; i += 1 }
               } }
          case Kind.KRef =>
            '{ if $src0.isInstanceOf[RefArr] then {
                 val a = $src0.asInstanceOf[RefArr].arr; var i = 0
                 while ${ cond('i) } do { ${ perElem('{ a(i).asInstanceOf[se] }.asTerm) }; i += 1 }
               } else {
                 var i = 0
                 while ${ cond('i) } do { ${ perElem('{ FArrayOps.refAt($src0, i).asInstanceOf[se] }.asTerm) }; i += 1 }
               } }

    // --- declare loop-state vars (above the loop), then assemble the terminal body ---
    def withDone(k: Option[Done] => Term): Term =
      if takesPresent then '{ var done: Boolean = false; ${ k(Some(Done('{ done = true }, '{ done }))).asExpr } }.asTerm
      else k(None)

    def declareSlots(rem: List[(Int, Term)], acc: Map[Int, Slot])(k: Map[Int, Slot] => Term): Term =
      rem match
        case Nil => k(acc)
        case (idx, argT) :: rest =>
          '{ val lim: Int = { val t = ${ argT.asExprOf[Int] }; if t < 0 then 0 else t }
             var c: Int = 0
             ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, '{ lim })))(k).asExpr } }.asTerm

    /** static upper bound on output length = min(source length, every `take` limit). */
    def capExpr(n0: Expr[Int], counters: Map[Int, Slot]): Expr[Int] =
      indexed.collect { case (TakeS(_), i) => counters(i).lim }.foldLeft(n0)((a, l) => '{ java.lang.Math.min($a, $l) })

    def assemble(src0: Expr[FBase], n0: Expr[Int], done: Option[Done], counters: Map[Int, Slot]): Term =
      def ctx(consume: Term => Term) = Ctx(consume, done, counters)
      tag match
        case TTag.Count =>
          '{ var cnt = 0; ${ fillLoop(src0, n0, ctx(_ => '{ cnt += 1 }.asTerm)) }; cnt }.asTerm
        case TTag.Foreach =>
          val f = args(0)
          '{ ${ fillLoop(src0, n0, ctx(v => applyLambda(f, v))) }; () }.asTerm
        case TTag.Fold =>
          val z = args(0); val op = args(1)
          z.tpe.widen.asType match
            case '[zz] =>
              '{ var acc: zz = ${ z.asExprOf[zz] }
                 ${ fillLoop(src0, n0, ctx(v => '{ acc = ${ applyN(op, List('{ acc }.asTerm, v)).asExprOf[zz] } }.asTerm)) }
                 acc }.asTerm
        case TTag.ToFArray => outK match
          case Kind.KInt =>
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Int](cap); var o = 0
               ${ fillLoop(src0, n0, ctx(v => '{ out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm)) }
               if o == 0 then (IntArr.EMPTY: FBase) else if o == cap then new IntArr(out, o) else new IntArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          case Kind.KLong =>
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Long](cap); var o = 0
               ${ fillLoop(src0, n0, ctx(v => '{ out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm)) }
               if o == 0 then (LongArr.EMPTY: FBase) else if o == cap then new LongArr(out, o) else new LongArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          case Kind.KDouble =>
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Double](cap); var o = 0
               ${ fillLoop(src0, n0, ctx(v => '{ out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm)) }
               if o == 0 then (DoubleArr.EMPTY: FBase) else if o == cap then new DoubleArr(out, o) else new DoubleArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          case Kind.KRef =>
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Object](cap); var o = 0
               ${ fillLoop(src0, n0, ctx(v => '{ out(o) = ${ v.asExpr }.asInstanceOf[Object]; o += 1 }.asTerm)) }
               if o == 0 then (RefArr.EMPTY: FBase) else if o == cap then new RefArr(out, o) else new RefArr(java.util.Arrays.copyOf(out, o), o) }.asTerm

    // --- final assembly: bind source + length once, then state vars, then the loop ---
    '{ val src0: FBase = $srcBase; val n0: Int = src0.length
       ${ withDone(done => declareSlots(slots, Map.empty)(counters => assemble('src0, 'n0, done, counters))).asExpr } }
