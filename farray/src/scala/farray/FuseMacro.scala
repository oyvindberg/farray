package farray

import scala.quoted.*

/** which terminal a fused pipeline ends in (the macro shares one lowering core across all of them). */
enum TTag:
  case Run, Foreach, Fold, Count, Find, Exists, Forall, HeadOption, Head, IndexWhere, ReduceOpt, ExtremumBy

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
  def runImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[FArray[A]] =
    '{ ${ core[A](self, TTag.Run, Nil) }.asInstanceOf[FArray[A]] }

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

  // index of the first survivor satisfying `p` (post-upstream-filtering position), or -1 — short-circuits.
  def indexWhereImpl[A: Type](self: Expr[Fuse[A]], p: Expr[A => Boolean])(using Quotes): Expr[Int] =
    '{ ${ core[A](self, TTag.IndexWhere, List(p)) }.asInstanceOf[Int] }
  // seeded single-accumulator reduce → Some(acc) over survivors, None if empty (one allocation, at the end).
  def reduceOptImpl[A: Type, B: Type](self: Expr[Fuse[A]], op: Expr[(B, A) => B])(using Quotes): Expr[Option[B]] =
    '{ ${ core[A](self, TTag.ReduceOpt, List(op)) }.asInstanceOf[Option[B]] }
  // the survivor minimizing/maximizing key `f` under `better` (true if the new key wins), or None — two vars,
  // no tuple, `f` computed once per element.
  def extremumByImpl[A: Type, B: Type](self: Expr[Fuse[A]], f: Expr[A => B], better: Expr[(B, B) => Boolean])(using Quotes): Expr[Option[A]] =
    '{ ${ core[A](self, TTag.ExtremumBy, List(f, better)) }.asInstanceOf[Option[A]] }

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
    final case class CollectS(pf: Term, bTpe: TypeRepr) extends Stage // collect: filter+map via a PartialFunction
    final case class TakeWhileS(p: Term) extends Stage                 // emit until p first fails, then stop (done)
    final case class DropWhileS(p: Term) extends Stage                 // skip the leading run matching p
    final case class DistinctS(key: Option[Term]) extends Stage        // distinct (None) / distinctBy (Some f)
    final case class ScanLeftS(z: Term, op: Term, zTpe: TypeRepr) extends Stage // running fold (emits z then each op)
    case object ZipWithIndexS extends Stage
    final case class ZipS(that: Term, bTpe: TypeRepr, combine: Option[Term]) extends Stage // zip (None) / map2 (Some f)

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
    // `flag` (dropWhile: read + clear a Boolean) and `seen` (distinct: the dedup set) carry non-counter state.
    final case class Slot(read: Expr[Int], inc: Expr[Unit], lim: Option[Expr[Int]], zipThat: Option[(Expr[FBase], TypeRepr)] = None,
                          flag: Option[(Expr[Boolean], Expr[Unit])] = None, seen: Option[Expr[scala.collection.mutable.HashSet[Any]]] = None,
                          acc: Option[(Term, Term => Term)] = None) // scanLeft: (read accumulator, assign accumulator)
    // how a counter-needing stage declares its above-loop state
    sealed trait CSpec
    final case class LimSpec(arg: Term) extends CSpec       // take/drop: a clamped limit
    case object IdxSpec extends CSpec                        // zipWithIndex: a bare counter
    final case class ZipSpec(that: Term, bTpe: TypeRepr) extends CSpec // zip/map2: bind `that` + its length + a counter
    case object DropWhileSpec extends CSpec                  // dropWhile: a `var dropping = true`
    case object DistinctSpec extends CSpec                   // distinct/distinctBy: a `seen` HashSet
    final case class ScanSpec(z: Term, zTpe: TypeRepr) extends CSpec // scanLeft: a `var acc: Z = z`
    final case class Ctx(consume: Term => Term, done: Option[Done], counters: Map[Int, Slot])

    // The value flowing through a pure map/filter segment, decomposed into independent columns. A `Sc`'s `read`
    // is CPS: it binds the column to a fresh val on FIRST read (memoized, so no recomputation) and yields the
    // ref — so a column read only inside a guard binds there, i.e. computes only for survivors (the sink).
    sealed trait Shape
    final case class Sc(read: (Term => Term) => Term) extends Shape
    // a decomposed PRODUCT (tuple / case class / named tuple): independent column shapes + how to rebuild the
    // whole value from them (only used if the product is ever materialized — used whole or emitted).
    final case class Tup(parts: List[Shape], rebuild: List[Term] => Term) extends Shape
    type Path = List[(Int, String)] // a field path into a product: (column index, accessor name) per hop

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
        case Apply(Select(prev, "withFilter"), List(p))                   => parse(prev, FilterS(unwrap(p), false) :: acc)
        case Apply(Select(prev, "filterNot"), List(p))                    => parse(prev, FilterS(unwrap(p), true) :: acc)
        case Apply(TypeApply(Select(prev, "collect"), List(b)), List(pf)) => parse(prev, CollectS(unwrap(pf), b.tpe) :: acc)
        case Apply(Select(prev, "take"), List(n))                         => parse(prev, TakeS(unwrap(n)) :: acc)
        case Apply(Select(prev, "drop"), List(n))                         => parse(prev, DropS(unwrap(n)) :: acc)
        case Apply(Select(prev, "takeWhile"), List(p))                    => parse(prev, TakeWhileS(unwrap(p)) :: acc)
        case Apply(Select(prev, "dropWhile"), List(p))                    => parse(prev, DropWhileS(unwrap(p)) :: acc)
        case Select(prev, "distinct")                                     => parse(prev, DistinctS(None) :: acc)
        case Apply(TypeApply(Select(prev, "distinctBy"), _), List(f))     => parse(prev, DistinctS(Some(unwrap(f))) :: acc)
        case Apply(Apply(TypeApply(Select(prev, "scanLeft"), List(b)), List(z)), List(op)) => parse(prev, ScanLeftS(unwrap(z), unwrap(op), b.tpe) :: acc)
        case Select(prev, "zipWithIndex")                                 => parse(prev, ZipWithIndexS :: acc)
        case Apply(TypeApply(Select(prev, "zip"), List(b)), List(that))    => parse(prev, ZipS(unwrap(that), b.tpe, None) :: acc)
        case Apply(Apply(TypeApply(Select(prev, "map2"), List(b, _)), List(that)), List(f)) => parse(prev, ZipS(unwrap(that), b.tpe, Some(unwrap(f))) :: acc)
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

    /** read `src(idx)` unboxed at the given element kind (leaf fast-path lives inside `<kind>At`). */
    def readAtKind(elemTpe: TypeRepr, src: Expr[FBase], idx: Expr[Int]): Term = kindOf(elemTpe) match
      case Kind.KInt    => '{ FArrayOps.intAt($src, $idx) }.asTerm
      case Kind.KLong   => '{ FArrayOps.longAt($src, $idx) }.asTerm
      case Kind.KDouble => '{ FArrayOps.doubleAt($src, $idx) }.asTerm
      case Kind.KRef    => elemTpe.asType match { case '[b] => '{ FArrayOps.refAt($src, $idx).asInstanceOf[b] }.asTerm }

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

    // identity → hand back the source (only meaningful for Run).
    if stages.isEmpty && tag == TTag.Run then
      return '{ ${ srcTerm.asExpr }.asInstanceOf[FBase] }

    val srcK = kindOf(srcElem)
    val outK = kindOf(aType)
    val srcBase: Expr[FBase] = '{ ${ srcTerm.asExpr }.asInstanceOf[FBase] }

    val indexed: List[(Stage, Int)] = stages.zipWithIndex
    // stages needing an above-loop counter (take/drop = a clamped limit, zipWithIndex = a bare counter,
    // zip/map2 = `that` + its length + a counter).
    val counterStages: List[(Int, CSpec)] = indexed.collect {
      case (TakeS(n), i)            => (i, LimSpec(n))
      case (DropS(n), i)            => (i, LimSpec(n))
      case (ZipWithIndexS, i)       => (i, IdxSpec)
      case (ZipS(that, b, _), i)    => (i, ZipSpec(that, b))
      case (DropWhileS(_), i)       => (i, DropWhileSpec)
      case (DistinctS(_), i)        => (i, DistinctSpec)
      case (ScanLeftS(z, _, zt), i) => (i, ScanSpec(z, zt))
    }
    val takesPresent: Boolean = stages.exists(_.isInstanceOf[TakeS])
    val hasZip: Boolean        = stages.exists(_.isInstanceOf[ZipS])
    val hasFlatMap: Boolean = stages.exists(_.isInstanceOf[FlatMapS])
    val hasTakeWhile: Boolean = stages.exists(_.isInstanceOf[TakeWhileS])
    val scanCount: Int = stages.count(_.isInstanceOf[ScanLeftS])
    if scanCount > 1 then report.errorAndAbort("fuse: at most one scanLeft per pipeline")
    val hasScan: Boolean = scanCount == 1
    // scanLeft emits one MORE element than its input (the initial z), so the output can exceed the source length
    val needsGrow: Boolean = hasFlatMap || hasScan
    // the scanLeft stage index + the stages downstream of it (run once with z as a prologue, then per element)
    val scanInfo: Option[(Int, List[(Stage, Int)])] =
      indexed.collectFirst { case (ScanLeftS(_, _, _), i) => i }.map(i => (i, indexed.drop(i + 1)))
    val shortCircuit: Boolean = tag match
      case TTag.Find | TTag.Exists | TTag.Forall | TTag.HeadOption | TTag.Head | TTag.IndexWhere => true
      case _                                                                                     => false
    val needsDone: Boolean = takesPresent || shortCircuit || hasZip || hasTakeWhile // any stage that can stop the stream

    // ===== Layer B: pure map/filter segment optimizer (compute-for-survivors via memoized lazy columns) =====

    /** a column that binds to a val on first read (memoized), then yields the ref. */
    def memoScalar(compute: (Term => Term) => Term): Shape =
      var bound: Option[Term] = None
      Sc(k => bound match
        case Some(r) => k(r)
        case None    => compute(v => letBind(v)(r => { bound = Some(r); k(r) })))

    def srcShape(cur: Term): Shape = Sc(k => k(cur)) // already bound by the loop / an upstream stage

    /** materialize a shape to a single Term (binding columns as needed); a decomposed product is rebuilt. */
    def readShape(shape: Shape)(k: Term => Term): Term = shape match
      case Sc(read)            => read(k)
      case Tup(parts, rebuild) => readAll(parts, Nil)(ts => k(rebuild(ts)))
    def readAll(parts: List[Shape], acc: List[Term])(k: List[Term] => Term): Term = parts match
      case Nil       => k(acc.reverse)
      case p :: rest => readShape(p)(t => readAll(rest, t :: acc)(k))
    /** materialize a 2-tuple. If BOTH fields are primitive (Int/Long/Double), build the stdlib @specialized
     *  `Tuple2$mcXY$sp` subclass so the fields stay UNBOXED, then cast to the generic `(A, B)` — a runtime no-op
     *  (it really IS a Tuple2). Mixed/reference fields fall back to the generic `Tuple2` (only a primitive in a
     *  generic slot would box, and that's Scala's Tuple2 erasure, not ours). Tuple3 isn't @specialized. */
    def mkPair(a: Term, b: Term): Term =
      val ti = TypeRepr.of[Int]; val tj = TypeRepr.of[Long]; val td = TypeRepr.of[Double]
      val ta = a.tpe.widen; val tb = b.tpe.widen
      def is(t: TypeRepr, u: TypeRepr) = t =:= u
      if      is(ta, ti) && is(tb, ti) then '{ new scala.Tuple2$mcII$sp(${ a.asExprOf[Int] }, ${ b.asExprOf[Int] }).asInstanceOf[(Int, Int)] }.asTerm
      else if is(ta, ti) && is(tb, tj) then '{ new scala.Tuple2$mcIJ$sp(${ a.asExprOf[Int] }, ${ b.asExprOf[Long] }).asInstanceOf[(Int, Long)] }.asTerm
      else if is(ta, ti) && is(tb, td) then '{ new scala.Tuple2$mcID$sp(${ a.asExprOf[Int] }, ${ b.asExprOf[Double] }).asInstanceOf[(Int, Double)] }.asTerm
      else if is(ta, tj) && is(tb, ti) then '{ new scala.Tuple2$mcJI$sp(${ a.asExprOf[Long] }, ${ b.asExprOf[Int] }).asInstanceOf[(Long, Int)] }.asTerm
      else if is(ta, tj) && is(tb, tj) then '{ new scala.Tuple2$mcJJ$sp(${ a.asExprOf[Long] }, ${ b.asExprOf[Long] }).asInstanceOf[(Long, Long)] }.asTerm
      else if is(ta, tj) && is(tb, td) then '{ new scala.Tuple2$mcJD$sp(${ a.asExprOf[Long] }, ${ b.asExprOf[Double] }).asInstanceOf[(Long, Double)] }.asTerm
      else if is(ta, td) && is(tb, ti) then '{ new scala.Tuple2$mcDI$sp(${ a.asExprOf[Double] }, ${ b.asExprOf[Int] }).asInstanceOf[(Double, Int)] }.asTerm
      else if is(ta, td) && is(tb, tj) then '{ new scala.Tuple2$mcDJ$sp(${ a.asExprOf[Double] }, ${ b.asExprOf[Long] }).asInstanceOf[(Double, Long)] }.asTerm
      else if is(ta, td) && is(tb, td) then '{ new scala.Tuple2$mcDD$sp(${ a.asExprOf[Double] }, ${ b.asExprOf[Double] }).asInstanceOf[(Double, Double)] }.asTerm
      else '{ scala.Tuple2(${ a.asExpr }, ${ b.asExpr }) }.asTerm
    def mkTuple(ts: List[Term]): Term = ts match
      case List(a, b)    => mkPair(a, b)
      case List(a, b, c) => '{ scala.Tuple3(${ a.asExpr }, ${ b.asExpr }, ${ c.asExpr }) }.asTerm
      case _             => report.errorAndAbort("fuse: only tuple arities 2 and 3 are supported here")

    /** the (label, type) of each field of a product type (case class / tuple / named tuple), else None. This is
     *  reflect's view of `Mirror.ProductOf` — `caseFields` are the product's accessors in declaration order. */
    def productFields(tpe: TypeRepr): Option[List[(String, TypeRepr)]] =
      val sym = tpe.typeSymbol
      if sym.flags.is(Flags.Case) && sym.caseFields.nonEmpty then Some(sym.caseFields.map(f => (f.name, tpe.memberType(f))))
      else None
    /** reconstruct a product value of `tpe` from its field values (companion `apply`, e.g. `C(...)` / tuple). */
    def mkProduct(tpe: TypeRepr, vs: List[Term]): Term =
      // a 2-tuple goes through mkPair so an all-primitive pair uses the unboxed @specialized Tuple2
      if vs.length == 2 && tpe.typeSymbol == TypeRepr.of[Tuple2[Any, Any]].typeSymbol then return mkPair(vs(0), vs(1))
      val comp = tpe.typeSymbol.companionModule
      comp.methodMember("apply").find(_.paramSymss.flatMap(_.filter(_.isTerm)).length == vs.length) match
        case Some(m) =>
          val base = Ref(comp).select(m)
          (if tpe.typeArgs.nonEmpty then base.appliedToTypes(tpe.typeArgs) else base).appliedToArgs(vs)
        case None => Apply(Select(New(Inferred(tpe)), tpe.typeSymbol.primaryConstructor), vs)

    // --- AST shape detection / substitution ---
    def decomposeLambda(t: Term): Option[(Symbol, Term)] = unwrap(t) match
      case Lambda(List(vd), body) => Some((vd.symbol, body))
      case _                      => None
    /** the index of field `name` in `inner`'s product type, if `inner.name` is a product-field access. */
    def fieldAccess(inner: Term, name: String): Option[Int] =
      productFields(inner.tpe.widen).flatMap { fs => val i = fs.indexWhere(_._1 == name); if i >= 0 then Some(i) else None }
    def isFieldSelect(t: Term): Option[(Term, Int, String)] = t match
      case Select(inner, name) => fieldAccess(inner, name).map(i => (inner, i, name))
      case _                    => None
    /** a CANONICAL product construction `C(a, b, …)` / `(a, b)` / `new C(…)` → (product type, field args). Only the
     *  product's OWN apply/constructor counts (not an arbitrary factory returning C), so args == fields. */
    def isProductCtor(t: Term): Option[(TypeRepr, List[Term])] =
      val rt = t.tpe.widen
      productFields(rt) match
        case Some(fields) =>
          def appliedTo(fn: Term, as: List[Term]): Option[(TypeRepr, List[Term])] =
            if as.length != fields.length then None
            else fn match
              case Select(New(_), "<init>")                                                            => Some((rt, as))
              case sel @ Select(_, "apply") if sel.symbol.owner == rt.typeSymbol.companionModule.moduleClass => Some((rt, as))
              case _                                                                                    => None
          t match
            case Apply(TypeApply(fn, _), as) => appliedTo(fn, as)
            case Apply(fn, as)               => appliedTo(fn, as)
            case _                           => None
        case None => None
    // a maximal field PATH rooted at a symbol: `p.a.x` -> (p, List((iₐ,"a"),(iₓ,"x"))); a bare `p` -> (p, Nil).
    def projPath(t: Term): Option[(Symbol, Path)] = t match
      case id: Ident => Some((id.symbol, Nil))
      case Select(inner, name) =>
        (projPath(inner), fieldAccess(inner, name)) match
          case (Some((s, p)), Some(idx)) => Some((s, p :+ (idx, name)))
          case _                         => None
      case _ => None
    /** every maximal field path from `param` in `body` (Nil path = param used whole). */
    def collectPaths(body: Term, param: Symbol): List[Path] =
      val buf = scala.collection.mutable.LinkedHashSet.empty[Path]
      (new TreeTraverser:
        override def traverseTree(t: Tree)(owner: Symbol): Unit = t match
          case (_: Ident | _: Select) if projPath(t.asInstanceOf[Term]).exists((s, _) => s == param) =>
            buf += projPath(t.asInstanceOf[Term]).get._2 // maximal: don't descend into inner field reads
          case _ => super.traverseTree(t)(owner)
      ).traverseTree(body)(Symbol.spliceOwner)
      buf.toList
    def substParam(body: Term, param: Symbol, repl: Term): Term =
      (new TreeMap:
        override def transformTerm(t: Term)(owner: Symbol): Term = t match
          case id: Ident if id.symbol == param => repl
          case _                                => super.transformTerm(t)(owner)
      ).transformTerm(body)(Symbol.spliceOwner)
    /** replace each maximal param-field path with its resolved column ref. */
    def substPaths(body: Term, param: Symbol, refs: Map[Path, Term]): Term =
      (new TreeMap:
        override def transformTerm(t: Term)(owner: Symbol): Term = projPath(t) match
          case Some((s, p)) if s == param && refs.contains(p) => refs(p)
          case _                                              => super.transformTerm(t)(owner)
      ).transformTerm(body)(Symbol.spliceOwner)
    /** navigate a shape down a field path to the leaf column (no intermediate product is materialized). */
    def navigate(cur: Shape, path: Path): Shape = path match
      case Nil              => cur
      case (j, name) :: rest => cur match
        case Tup(ps, _) if j >= 0 && j < ps.length => navigate(ps(j), rest)
        case other                                 => navigate(projField(other, name), rest)
    def projField(sc: Shape, name: String): Shape =
      memoScalar(k => readShape(sc)(t => k(Select.unique(t, name))))

    // --- deep CSE: hash-cons identical sub-expressions into memoized columns (shared across one map's tuple
    //     components / one predicate), so e.g. `(f(x)+1, f(x)+2)` computes f(x) ONCE. Keyed structurally; the
    //     memo binds each unique sub-expr on first read, so it still respects the sink. ---
    type CseT = scala.collection.mutable.Map[String, Shape]
    def trivial(t: Term): Boolean = t match
      case _: Ident | _: Literal => true
      case Typed(e, _)           => trivial(e)
      case _                     => false
    /** the immediate sub-terms of `t` worth CSE-ing — only plain value expressions (Apply/Select/TypeApply), so
     *  we never try to bind a varargs/repeated/by-name child. `&&`/`||` are left whole to keep short-circuiting. */
    def cseChildren(t: Term): List[Term] =
      // a value computation worth binding: an Apply/Select/TypeApply that is NOT a package/module/type path
      // (binding `_root_.scala` or a companion-object ref is nonsense and miscompiles).
      def pathLike(c: Term): Boolean =
        val s = c.symbol; !s.isNoSymbol && (s.flags.is(Flags.Package) || s.flags.is(Flags.Module) || s.isType)
      // a partially-applied method ref (e.g. the `Predef.ArrowAssoc` under `x -> y`) has a method/poly type — it
      // is not a value, so binding it to a val fails ("partially applied Term"). Only bind real value terms.
      def isValue(c: Term): Boolean = c.tpe.widen match { case _: MethodType | _: PolyType => false; case _ => true }
      def ok(c: Term): Boolean = (c match { case _: Apply | _: Select | _: TypeApply => true; case _ => false }) && !pathLike(c) && isValue(c)
      t match
        case Apply(Select(_, "&&" | "||"), _)           => Nil
        case Apply(TypeApply(Select(recv, _), _), args) => (recv :: args).filter(ok)
        case Apply(Select(recv, _), args)               => (recv :: args).filter(ok)
        case Apply(fn, args)                            => (fn :: args).filter(ok)
        case Select(recv, _)                            => List(recv).filter(ok)
        case TypeApply(inner, _)                        => List(inner).filter(ok)
        case Typed(e, _)                                => List(e).filter(ok)
        case _                                          => Nil
    /** rebuild `t` with each given child sub-term replaced (by identity) by its bound ref. */
    def replaceChildren(t: Term, repl: Map[Term, Term]): Term =
      (new TreeMap:
        override def transformTerm(x: Term)(owner: Symbol): Term = repl.getOrElse(x, super.transformTerm(x)(owner))
      ).transformTerm(t)(Symbol.spliceOwner)
    def cse(t: Term, cseT: CseT): Shape =
      if trivial(t) then Sc(k => k(t))
      else cseT.getOrElseUpdate(t.show(using Printer.TreeStructure), decomposeCse(t, cseT))
    def decomposeCse(t: Term, cseT: CseT): Shape =
      val children = cseChildren(t)
      if children.isEmpty then memoScalar(k => k(t)) // opaque non-trivial → memoize whole
      else memoScalar(k => readAll(children.map(c => cse(c, cseT)), Nil)(refs =>
        k(replaceChildren(t, children.zip(refs).toMap))))

    /** inline a `{ val a = …; val b = …; expr }` block (e.g. the desugaring of an untupled `(a, b) => …` lambda)
     *  into `expr`, so the result expression's tuple/projection structure becomes visible to `interp`. Inlining
     *  may duplicate a binding's rhs, but CSE re-shares it. Last-binding-first so earlier vals rewrite later ones. */
    def flattenBlock(t: Term): Term = unwrap(t) match
      case Block(stats, expr) if stats.nonEmpty && stats.forall { case vd: ValDef => vd.rhs.isDefined; case _ => false } =>
        var e = expr
        stats.reverse.foreach { case vd: ValDef =>
          e = (new TreeMap:
                override def transformTerm(x: Term)(owner: Symbol): Term =
                  x match { case id: Ident if id.symbol == vd.symbol => vd.rhs.get; case _ => super.transformTerm(x)(owner) }
              ).transformTerm(e)(Symbol.spliceOwner)
        }
        flattenBlock(e)
      case other => other

    // --- interpret a map lambda body symbolically into a Shape (decomposing tuples, resolving projections) ---
    def interp(body0: Term, param: Symbol, cur: Shape, cseT: CseT): Shape =
      val body = flattenBlock(body0)
      isProductCtor(body) match
        case Some((rt, parts)) => Tup(parts.map(interp(_, param, cur, cseT)), vs => mkProduct(rt, vs)) // decompose; CSE shared
        case None => isFieldSelect(body) match
          case Some((inner, idx, name)) => interp(inner, param, cur, cseT) match
            case Tup(ps, _) if idx >= 0 && idx < ps.length => ps(idx)
            case sc                                        => projField(sc, name)
          case None => body match
            case id: Ident if id.symbol == param => cur
            case _ => Sc(k => substColumns(body, param, cur)(subst => readShape(cse(subst, cseT))(k)))
    /** substitute param-uses in an atomic body with the columns it reads (binding only those columns). A bare
     *  param use (Nil path) forces materializing the whole product; otherwise each path resolves to a leaf column. */
    def substColumns(body: Term, param: Symbol, cur: Shape)(k: Term => Term): Term =
      val paths = collectPaths(body, param)
      // whole-param use → substitute the materialized product for every param mention. A Tup must be rebuilt —
      // bind it to ONE val first so it isn't rebuilt per occurrence; a Sc is already a bound ref.
      if paths.contains(Nil) then cur match
        case Tup(_, _) => readShape(cur)(ct => letBind(ct)(r => k(substParam(body, param, r))))
        case _         => readShape(cur)(ct => k(substParam(body, param, ct)))
      else readPaths(cur, paths, Map.empty)(refs => k(substPaths(body, param, refs)))
    def readPaths(cur: Shape, paths: List[Path], acc: Map[Path, Term])(k: Map[Path, Term] => Term): Term =
      paths match
        case Nil       => k(acc)
        case p :: rest => readShape(navigate(cur, p))(ref => readPaths(cur, rest, acc + (p -> ref))(k))

    /** a local `case class` defined inside a lambda body can't be inlined into the fused loop — its synthesized
     *  companion (`apply`/`unapply`/`copy`/product members) doesn't relocate (true of `betaReduce` too, not just
     *  our TreeMap), so any relocation yields "reference … outside scope". Detect it and fail with a clear message
     *  instead of the cryptic one. (Local `def`s, non-case `class`es, vals, and TOP-LEVEL case classes all work.) */
    def checkNoLocalCaseClass(t: Term): Unit =
      (new TreeTraverser:
        override def traverseTree(x: Tree)(o: Symbol): Unit = x match
          case cd: ClassDef if cd.symbol.flags.is(Flags.Case) => report.errorAndAbort(
            s"fuse: a `case class` (`${cd.name}`) defined INSIDE a lambda body isn't supported — the macro can't " +
            s"relocate its synthesized companion into the fused loop. Define the case class at the top level (a " +
            s"top-level case class, incl. generic, decomposes fully), or use a local non-case `class`.", cd.pos)
          case _ => super.traverseTree(x)(o)
      ).traverseTree(t)(Symbol.spliceOwner)

    def applyMap(f: Term, cur: Shape): Shape = decomposeLambda(f) match
      case Some((param, body)) => checkNoLocalCaseClass(body); interp(body, param, cur, scala.collection.mutable.Map.empty)
      case None                => memoScalar(k => readShape(cur)(ct => k(applyLambda(f, ct))))

    /** read a filter predicate (binding only the columns it touches), then hand the Boolean term to `k`. */
    def predShape(p: Term, neg: Boolean, cur: Shape)(k: Term => Term): Term =
      def fin(b: Term): Term = k(if neg then '{ !${ b.asExprOf[Boolean] } }.asTerm else b)
      decomposeLambda(p) match
        case Some((param, body)) => checkNoLocalCaseClass(body); readShape(interp(body, param, cur, scala.collection.mutable.Map.empty))(fin)
        case None                => readShape(cur)(ct => fin(applyLambda(p, ct)))

    /** lower a contiguous map/filter run; columns sink past filters automatically (lazy memoized reads). */
    def buildSegment(ss: List[Stage], cur: Shape)(k: Shape => Term): Term = ss match
      case Nil                  => k(cur)
      case MapS(f) :: rest      => buildSegment(rest, applyMap(f, cur))(k)
      case FilterS(p, neg) :: rest => predShape(p, neg, cur)(b => If(b, buildSegment(rest, cur)(k), unit))
      case _                    => k(cur) // segment is map/filter only

    /** continue downstream from a decomposed Shape: run the leading map/filter run column-aware (so independent
     *  columns sink/DCE), then materialize and hand off to buildBody for the rest. Used by collect / zipWithIndex
     *  / zip so a produced value stays decomposed through the following segment. */
    def continueShape(shape: Shape, rest: List[(Stage, Int)], ctx: Ctx): Term =
      val (seg, tail) = rest.span { case (MapS(_), _) => true; case (FilterS(_, _), _) => true; case _ => false }
      buildSegment(seg.map(_._1), shape)(fs => readShape(fs)(t => buildBody(tail, t, ctx)))

    /** lower `collect(pf)` by inlining the PartialFunction literal's match into the loop. Scala 3 encodes a
     *  `{ case … }` literal as `new PartialFunction { def applyOrElse(x, default) = x match { <cases>; case _ =>
     *  default(x) } … }`; we splice that match with scrutinee = `cur`, each real case continuing downstream and
     *  the `default(x)` fallthrough → skip. Falls back to isDefinedAt/apply if the literal's shape is unusual. */
    def collectBody(pf: Term, bTpe: TypeRepr, cur: Term, rest: List[(Stage, Int)], ctx: Ctx): Term =
      // a PF literal is `Block(DefDef($anonfun, (x$1), Match(x$1, userCases)), Closure(_, PartialFunction))` — the
      // user's cases only, partiality carried by the Closure type. We splice the match with scrutinee = `cur`,
      // each case continuing downstream, plus our own `case _ => skip` (the match is partial).
      def extract(t: Term): Option[(Symbol, List[CaseDef])] = unwrap(t) match
        case Block(stats, _: Closure) =>
          stats.collectFirst { case dd: DefDef => dd }.flatMap { dd =>
            val ps = dd.paramss.flatMap { case TermParamClause(tps) => tps; case _ => Nil }
            dd.rhs.map(unwrap).collect { case Match(_, cs) if ps.nonEmpty => (ps.head.symbol, cs) }
          }
        case _ => None
      def isCatchAll(cd: CaseDef): Boolean =
        cd.guard.isEmpty && (cd.pattern match { case Wildcard() => true; case Bind(_, Wildcard()) => true; case _ => false })
      def subX(xSym: Symbol)(t: Term): Term =
        (new TreeMap:
          override def transformTerm(x: Term)(o: Symbol): Term =
            x match { case id: Ident if id.symbol == xSym => cur; case _ => super.transformTerm(x)(o) }
        ).transformTerm(t)(Symbol.spliceOwner)
      extract(pf) match
        case Some((xSym, cases)) =>
          // each case body goes through interp (param = the scrutinee x$1, bound to the element) so a tuple/product
          // case result decomposes into columns — the same DCE/sink/CSE a `map` body gets. Guards stay literal.
          val mapped = cases.map(cd =>
            CaseDef(cd.pattern, cd.guard.map(subX(xSym)),
                    continueShape(interp(cd.rhs, xSym, srcShape(cur), scala.collection.mutable.Map.empty), rest, ctx)))
          // append a skip default unless the user's match is already total (avoids an unreachable-case warning)
          val newCases = if cases.exists(isCatchAll) then mapped else mapped :+ CaseDef(Wildcard(), None, unit)
          Match(cur, newCases).changeOwner(Symbol.spliceOwner)
        case None =>
          bTpe.asType match
            case '[bb] =>
              '{ val pf0 = ${ pf.asExpr }.asInstanceOf[PartialFunction[Any, Any]]
                 if pf0.isDefinedAt(${ cur.asExpr }) then
                   ${ buildBody(rest, '{ pf0(${ cur.asExpr }).asInstanceOf[bb] }.asTerm, ctx).asExprOf[Unit] } }.asTerm

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
      case (CollectS(pf, bTpe), _) :: rest =>
        // inline the PartialFunction's match into the loop: each real case continues downstream, the synthetic
        // `case _ => default(x)` fallthrough becomes a skip. So collect is filter+map+match fused, no PF object.
        collectBody(pf, bTpe, cur, rest, ctx)
      case (TakeWhileS(p), _) :: rest =>
        // emit while p holds; the first failure stops the whole (possibly nested) traversal via `done`.
        val d = ctx.done.get
        '{ if ${ applyLambda(p, cur).asExprOf[Boolean] } then ${ buildBody(rest, cur, ctx).asExprOf[Unit] } else ${ d.set } }.asTerm
      case (DropWhileS(p), i) :: rest =>
        // skip the leading run matching p; once we stop dropping, never drop again (&& short-circuits p after).
        val sl = ctx.counters(i); val (dropping, stopDropping) = sl.flag.get
        '{ if $dropping && ${ applyLambda(p, cur).asExprOf[Boolean] } then ()
           else { $stopDropping; ${ buildBody(rest, cur, ctx).asExprOf[Unit] } } }.asTerm
      case (DistinctS(keyFn), i) :: rest =>
        // emit only if the element/key is newly seen (HashSet.add returns true on first insertion).
        val seen = ctx.counters(i).seen.get
        val key: Term = keyFn match { case Some(f) => applyLambda(f, cur); case None => cur }
        '{ if ${ seen }.add(${ key.asExpr }) then ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }.asTerm
      case (ScanLeftS(_, op, _), i) :: rest =>
        // update the running accumulator, then emit it downstream. (The initial z is emitted by withScanPrologue.)
        val (accRead, setAcc) = ctx.counters(i).acc.get
        '{ ${ setAcc(applyN(op, List(accRead, cur))).asExprOf[Unit] }
           ${ letBind(accRead)(a => buildBody(rest, a, ctx)).asExprOf[Unit] } }.asTerm
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
        // capture the index, advance it, then hand a DECOMPOSED (value, index) pair downstream — so a filter on
        // the index or a map keeping only the value never builds the tuple. The pair is rebuilt only if read whole.
        val sl = ctx.counters(i)
        letBind(sl.read.asTerm) { pos =>
          '{ ${ sl.inc }
             ${ continueShape(Tup(List(srcShape(cur), srcShape(pos)), ts => tupleWithIndex(ts(0), ts(1).asExprOf[Int])), rest, ctx).asExprOf[Unit] } }.asTerm
        }
      case (ZipS(_, bTpe, combine), i) :: rest =>
        // lock-step: stop (done) when `that` is exhausted; otherwise capture the position, advance, and continue
        // with a LAZY `that(pos)` column — so if the zipped value is never read downstream it is never read at all.
        val sl = ctx.counters(i); val d = ctx.done.get; val zn = sl.lim.get; val (zthat, _) = sl.zipThat.get
        '{ if ${ sl.read } >= $zn then ${ d.set }
           else {
             val pos: Int = ${ sl.read }
             ${ sl.inc }
             ${ val lazyB = memoScalar(kk => kk(readAtKind(bTpe, zthat, '{ pos })))
                val continue: Term = combine match
                  // map2 forces the value (f uses it); zip hands a Tup([cur, lazyB]) to the downstream segment so
                  // projections resolve to typed columns, no pair is allocated, and a discarded side is never read.
                  case Some(f) => readShape(lazyB)(b => buildBody(rest, applyN(f, List(cur, b)), ctx))
                  case None    => continueShape(Tup(List(srcShape(cur), lazyB), mkTuple), rest, ctx)
                continue.asExprOf[Unit] }
           } }.asTerm

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

    def declareSlots(rem: List[(Int, CSpec)], acc: Map[Int, Slot])(k: Map[Int, Slot] => Term): Term =
      rem match
        case Nil => k(acc)
        case (idx, LimSpec(argT)) :: rest => // take/drop: a clamped limit val + a counter
          '{ val lim: Int = { val t = ${ argT.asExprOf[Int] }; if t < 0 then 0 else t }
             var c: Int = 0
             ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, Some('{ lim }))))(k).asExpr } }.asTerm
        case (idx, IdxSpec) :: rest =>       // zipWithIndex: a counter only
          '{ var c: Int = 0
             ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, None)))(k).asExpr } }.asTerm
        case (idx, ZipSpec(thatT, bTpe)) :: rest => // zip/map2: bind `that` + its length + a counter
          '{ val zthat: FBase = ${ thatT.asExpr }.asInstanceOf[FBase]; val zn: Int = zthat.length
             var c: Int = 0
             ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, Some('{ zn }), Some(('{ zthat }, bTpe)))))(k).asExpr } }.asTerm
        case (idx, DropWhileSpec) :: rest =>  // dropWhile: a Boolean flag, read + cleared in the loop
          '{ var dropping: Boolean = true
             ${ declareSlots(rest, acc + (idx -> Slot('{ 0 }, '{ () }, None, flag = Some(('{ dropping }, '{ dropping = false })))))(k).asExpr } }.asTerm
        case (idx, DistinctSpec) :: rest =>   // distinct/distinctBy: a dedup set keyed by element/key value
          '{ val seen = new scala.collection.mutable.HashSet[Any]()
             ${ declareSlots(rest, acc + (idx -> Slot('{ 0 }, '{ () }, None, seen = Some('{ seen }))))(k).asExpr } }.asTerm
        case (idx, ScanSpec(z, zt)) :: rest => // scanLeft: a mutable accumulator initialized to z
          zt.asType match
            case '[zz] =>
              '{ var acc0: zz = ${ z.asExprOf[zz] }
                 ${ val slot = Slot('{ 0 }, '{ () }, None,
                                    acc = Some(('{ acc0 }.asTerm, (v: Term) => '{ acc0 = ${ v.asExprOf[zz] } }.asTerm)))
                    declareSlots(rest, acc + (idx -> slot))(k).asExpr } }.asTerm

    /** static upper bound on output length = min(source length, every `take` limit and zip `that` length). */
    def capExpr(n0: Expr[Int], counters: Map[Int, Slot]): Expr[Int] =
      indexed.collect { case (TakeS(_), i) => counters(i).lim.get; case (ZipS(_, _, _), i) => counters(i).lim.get }
        .foldLeft(n0)((a, l) => '{ java.lang.Math.min($a, $l) })

    // scanLeft: emit the initial `z` through the downstream ONCE before the loop (so the result has its leading
    // element even for empty input). Both `loop` callers below go through this, so it works for every terminal.
    def withScanPrologue(c: Ctx, body: Expr[Unit]): Expr[Unit] = scanInfo match
      case Some((i, post)) => '{ ${ buildBody(post, c.counters(i).acc.get._1, c).asExprOf[Unit] }; $body }
      case None            => body

    def assemble(src0: Expr[FBase], n0: Expr[Int], done: Option[Done], counters: Map[Int, Slot]): Term =
      def ctx(consume: Term => Term) = Ctx(consume, done, counters)
      def loop(consume: Term => Term): Expr[Unit] = { val c = ctx(consume); withScanPrologue(c, loopOver(src0, srcK, srcElem, indexed, c)) }
      tag match
        case TTag.Count =>
          '{ var cnt = 0; ${ loop(_ => '{ cnt += 1 }.asTerm) }; cnt }.asTerm
        case TTag.Foreach =>
          val f = args(0)
          '{ ${ loop(v => applyLambda(f, v)) }; () }.asTerm
        case TTag.Fold =>
          val z = args(0); val op = args(1)
          // Z = op's result type (op: (Z, A) => Z). Using z's own type would over-narrow when the seed is more
          // specific than Z — e.g. `foldLeft[Option[B]](None)(…)`, where z: None.type but acc must be Option[B].
          val zTpe = op.tpe.widen.dealias match { case AppliedType(_, targs) => targs.last; case _ => z.tpe.widen }
          zTpe.asType match
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
        case TTag.IndexWhere =>
          // bare `var idx` + position counter — no (value, index) pair is built; stop at the first match.
          val p = args(0); val d = done.get
          '{ var idx: Int = -1; var c: Int = 0
             ${ loop(v => '{ if ${ applyLambda(p, v).asExprOf[Boolean] } then { idx = c; ${ d.set } }; c = c + 1 }.asTerm) }
             idx }.asTerm
        case TTag.ReduceOpt =>
          // one seeded accumulator (no per-element Option); a single Some(acc) at the end. acc's type = op's
          // result (op: (B, A) => B). The `seeded` flag means the null/0 initializer is never observed.
          val op = args(0)
          val bTpe = op.tpe.widen.dealias match { case AppliedType(_, targs) => targs.last; case _ => srcElem }
          bTpe.asType match
            case '[bb] =>
              '{ var seeded = false; var acc: bb = null.asInstanceOf[bb]
                 ${ loop(v => '{ if !seeded then { acc = ${ v.asExprOf[bb] }; seeded = true }
                                 else acc = ${ applyN(op, List('{ acc }.asTerm, v)).asExprOf[bb] } }.asTerm) }
                 if seeded then Some(acc) else None }.asTerm
        case TTag.ExtremumBy =>
          // best element + its key in two vars (no tuple); `f` once per element; `better(newKey, bestKey)` wins.
          val f = args(0); val better = args(1)
          val bTpe = f.tpe.widen.dealias match { case AppliedType(_, targs) => targs.last; case _ => srcElem }
          bTpe.asType match
            case '[bb] =>
              '{ var seeded = false; var best: Any = null; var bestK: bb = null.asInstanceOf[bb]
                 ${ loop(v => '{ val k: bb = ${ applyLambda(f, v).asExprOf[bb] }
                                 if !seeded || ${ applyN(better, List('{ k }.asTerm, '{ bestK }.asTerm)).asExprOf[Boolean] } then
                                   { best = ${ v.asExpr }; bestK = k; seeded = true } }.asTerm) }
                 if seeded then Some(best) else None }.asTerm
        case TTag.Run => assembleOut(src0, n0, counters, ctx)

    // output array assembly: grow via ensureCap when a flatMap can expand it, else preallocate the upper bound.
    def assembleOut(src0: Expr[FBase], n0: Expr[Int], counters: Map[Int, Slot], ctx: (Term => Term) => Ctx): Term =
      def loop(consume: Term => Term): Expr[Unit] = { val c = ctx(consume); withScanPrologue(c, loopOver(src0, srcK, srcElem, indexed, c)) }
      outK match
        case Kind.KInt =>
          if needsGrow then
            '{ var out = new Array[Int](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapInt(out, o + 1); out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm) }
               if o == 0 then (IntArr.EMPTY: FBase) else if o == out.length then new IntArr(out, o) else new IntArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Int](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm) }
               if o == 0 then (IntArr.EMPTY: FBase) else if o == cap then new IntArr(out, o) else new IntArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
        case Kind.KLong =>
          if needsGrow then
            '{ var out = new Array[Long](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapLong(out, o + 1); out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm) }
               if o == 0 then (LongArr.EMPTY: FBase) else if o == out.length then new LongArr(out, o) else new LongArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Long](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm) }
               if o == 0 then (LongArr.EMPTY: FBase) else if o == cap then new LongArr(out, o) else new LongArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
        case Kind.KDouble =>
          if needsGrow then
            '{ var out = new Array[Double](java.lang.Math.max(8, $n0)); var o = 0
               ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapDouble(out, o + 1); out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm) }
               if o == 0 then (DoubleArr.EMPTY: FBase) else if o == out.length then new DoubleArr(out, o) else new DoubleArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
          else
            '{ val cap = ${ capExpr(n0, counters) }; val out = new Array[Double](cap); var o = 0
               ${ loop(v => '{ out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm) }
               if o == 0 then (DoubleArr.EMPTY: FBase) else if o == cap then new DoubleArr(out, o) else new DoubleArr(java.util.Arrays.copyOf(out, o), o) }.asTerm
        case Kind.KRef =>
          if needsGrow then
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
