package farray

import scala.quoted.*
import farray.json.{NdjsonSource, JsonScanner}

/** which terminal a fused pipeline ends in (the macro shares one lowering core across all of them). */
enum TTag:
  case Run, Find, Exists, Forall, HeadOption, Head, IndexWhere, Agg
  case GroupReduce // group by key(a), combine value(a) per key with reduce → Map[K,B]; primitive-keyed, no boxing
  case Plan // not a real terminal: returns a String DESCRIPTION of the plan the macro built, for testing

/** Macro implementation for fused pipelines (see `Fuse` and `docs/fused-pipeline-design.md`).
  *
  * A terminal receives `'this` = the `Expr` of the whole receiver `xs.fuse.map(f).flatMap(g).take(k)…`. We peel that AST back to the source `new Fuse(src)`,
  * collect the stage list, and lower it to ONE fused loop nest:
  *
  *   - every source / flatMap-inner level is traversed once (leaf fast-path + `<kind>At` fallback), specialized over the four op-kinds (Int/Long/Double/Ref);
  *   - `map`/`filter`/`take`/`drop` are straight-line binds, guards and counters; `flatMap` opens a nested loop, so everything downstream of it runs inside
  *     that inner loop;
  *   - lambdas are beta-reduced into the body, so no `Function1` is allocated or called per element;
  *   - a single `done` flag (needed by `take` and by the short-circuit terminals `find`/`exists`/`forall`/`head`) is threaded onto every loop condition, so a
  *     match escapes all nesting levels and the source is read no further than necessary;
  *   - output goes straight into one array — preallocated to `min(srcLen, take-limits)` when the pipeline can only shrink, or grown via `ensureCap` when a
  *     `flatMap` can expand it — then trimmed. No intermediate `FArray` per stage.
  */
object FuseMacro:

  // ---------- entry points (one per terminal) ----------
  def runImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[FArray[A]] =
    '{ ${ core[A](self, TTag.Run, Nil) }.asInstanceOf[FArray[A]] }

  // (foreach/foldLeft/count are NOT separate macro entry points — they desugar to a single-aggregate `agg(...)`
  //  in Fuse.scala and flow through aggImpl, sharing the Agg/AState terminal machinery.)

  // multi-aggregate: `aggs` is `List(Agg.xxx(...), …)`; the macro reads each `Agg.xxx(...)` call off the AST,
  // carries one accumulator per aggregate in one loop, and returns a tuple `R` of the finished results.
  def aggImpl[A: Type, R: Type](self: Expr[Fuse[A]], aggs: Expr[List[Agg[A, Any]]])(using Quotes): Expr[R] =
    '{ ${ core[A](self, TTag.Agg, List(aggs)) }.asInstanceOf[R] }
  // like aggImpl but the finished results are passed to `make` (a case-class constructor / any productN builder)
  // instead of tupled — so `aggTo(Summary.apply)(…)` returns a `Summary`. `make` is args(1) for the Agg case.
  def aggToImpl[A: Type, R: Type](self: Expr[Fuse[A]], aggs: Expr[List[Agg[A, Any]]], make: Expr[Any])(using Quotes): Expr[R] =
    '{ ${ core[A](self, TTag.Agg, List(aggs, make)) }.asInstanceOf[R] }

  // primitive-keyed group-reduce: group by key(a), combine value(a) per key with reduce. One fused pass into an
  // open-addressing map (unboxed Int/Long key, primitive value array for prim B), materialized to Map[K,B].
  def groupReduceByImpl[A: Type, K: Type, B: Type](self: Expr[Fuse[A]], key: Expr[A => K], value: Expr[A => B], reduce: Expr[(B, B) => B])(using
      Quotes
  ): Expr[Map[K, B]] =
    '{ ${ core[A](self, TTag.GroupReduce, List(key, value, reduce)) }.asInstanceOf[Map[K, B]] }

  // plan description (testing): TTag.Plan returns a String describing the built plan, never runs the loop.
  def planImpl[A: Type](self: Expr[Fuse[A]])(using Quotes): Expr[String] =
    '{ ${ core[A](self, TTag.Plan, Nil) }.asInstanceOf[String] }
  // planFold: same, but the live-set also reflects a fold `op` that reads the element directly.
  def planFoldImpl[A: Type, Z: Type](self: Expr[Fuse[A]], op: Expr[(Z, A) => Z])(using Quotes): Expr[String] =
    '{ ${ core[A](self, TTag.Plan, List(op)) }.asInstanceOf[String] }
  // planAgg: the live-set reflects the union of the aggregates' read fields. Passes the varargs Seq as args(0)
  // (parseAggList's seqElems already peels a Repeated/Typed(Repeated) — the varargs literal form).
  def planAggImpl[A: Type](self: Expr[Fuse[A]], aggs: Expr[Seq[Agg[A, Any]]])(using Quotes): Expr[String] =
    '{ ${ core[A](self, TTag.Plan, List(aggs)) }.asInstanceOf[String] }

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
  // (seeded reduce and extremum-by-element now flow through the Agg machinery — `Agg.reduceL` / `Agg.minBy` /
  //  `Agg.maxBy` — so the standalone reduceOption/minByOption/maxByOption terminals desugar to a single-agg call.)

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
    final case class TakeWhileS(p: Term) extends Stage // emit until p first fails, then stop (done)
    final case class DropWhileS(p: Term) extends Stage // skip the leading run matching p
    final case class DistinctS(key: Option[Term]) extends Stage // distinct (None) / distinctBy (Some f)
    final case class ScanLeftS(z: Term, op: Term, zTpe: TypeRepr) extends Stage // running fold (emits z then each op)
    final case class TapEachS(f: Term) extends Stage // run f for side effect, pass element through
    case object ZipWithIndexS extends Stage
    final case class ZipS(that: Term, bTpe: TypeRepr, combine: Option[Term]) extends Stage // zip (None) / map2 (Some f)

    enum Kind:
      case KInt, KLong, KDouble, KFloat, KShort, KByte, KChar, KBoolean, KRef
    // Specialize-or-fail, mirroring the eager API's `Repr` evidence (RefRepr requires A <: AnyRef): a primitive
    // kind needs the EXACT type; a reference kind needs `<: AnyRef`. Anything else (Any/AnyVal/Matchable, an
    // unbounded type param) can't be read unboxed — a primitive-backed FArray covariantly widened to such a
    // type would `isInstanceOf[RefArr]`-miss and read nulls — so reject it at compile time rather than miscompile.
    def kindOf(t: TypeRepr): Kind =
      if t =:= TypeRepr.of[Int] then Kind.KInt
      else if t =:= TypeRepr.of[Long] then Kind.KLong
      else if t =:= TypeRepr.of[Double] then Kind.KDouble
      else if t =:= TypeRepr.of[Float] then Kind.KFloat
      else if t =:= TypeRepr.of[Short] then Kind.KShort
      else if t =:= TypeRepr.of[Byte] then Kind.KByte
      else if t =:= TypeRepr.of[Char] then Kind.KChar
      else if t =:= TypeRepr.of[Boolean] then Kind.KBoolean
      else if t <:< TypeRepr.of[AnyRef] then Kind.KRef
      else
        report.errorAndAbort(
          s"fuse: cannot specialize element type `${t.show}` — fused pipelines support the JVM primitives or a " +
            s"reference type (`<: AnyRef`). A primitive-backed FArray widened to Any/AnyVal can't be read unboxed; " +
            s"use a concrete element type."
        )

    // --- loop-state handles (Exprs that read/mutate vars declared above the loop) ---
    final case class Done(set: Expr[Unit], read: Expr[Boolean])
    // an above-loop counter; `lim` is present for take/drop (their clamped limit), absent for zipWithIndex.
    // `flag` (dropWhile: read + clear a Boolean) and `seen` (distinct: the dedup set) carry non-counter state.
    final case class Slot(
        read: Expr[Int],
        inc: Expr[Unit],
        lim: Option[Expr[Int]],
        zipThat: Option[(Expr[FBase], TypeRepr)] = None,
        flag: Option[(Expr[Boolean], Expr[Unit])] = None,
        seen: Option[Expr[scala.collection.mutable.HashSet[Any]]] = None,
        acc: Option[(Term, Term => Term)] = None
    ) // scanLeft: (read accumulator, assign accumulator)
    // how a counter-needing stage declares its above-loop state
    sealed trait CSpec
    final case class LimSpec(arg: Term) extends CSpec // take/drop: a clamped limit
    case object IdxSpec extends CSpec // zipWithIndex: a bare counter
    final case class ZipSpec(that: Term, bTpe: TypeRepr) extends CSpec // zip/map2: bind `that` + its length + a counter
    case object DropWhileSpec extends CSpec // dropWhile: a `var dropping = true`
    case object DistinctSpec extends CSpec // distinct/distinctBy: a `seen` HashSet
    final case class ScanSpec(z: Term, zTpe: TypeRepr) extends CSpec // scanLeft: a `var acc: Z = z`
    // `consume` takes a MATERIALIZED element. `consumeShape`, when present, takes the element's decomposed Shape
    // instead — so a terminal whose lambda projects fields (e.g. `foldLeft((acc, r) => acc + r.amount)`) reads
    // only the columns it touches and never rebuilds the whole product (DCE/sink reaching the terminal's lambda).
    final case class Ctx(consume: Term => Term, done: Option[Done], counters: Map[Int, Slot], consumeShape: Option[Shape => Term] = None)

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
        case Apply(TypeApply(Select(prev, "map"), _), List(f))                              => parse(prev, MapS(unwrap(f)) :: acc)
        case Apply(TypeApply(Select(prev, "flatMap"), List(b)), List(f))                    => parse(prev, FlatMapS(unwrap(f), b.tpe) :: acc)
        case Apply(Select(prev, "filter"), List(p))                                         => parse(prev, FilterS(unwrap(p), false) :: acc)
        case Apply(Select(prev, "withFilter"), List(p))                                     => parse(prev, FilterS(unwrap(p), false) :: acc)
        case Apply(Select(prev, "filterNot"), List(p))                                      => parse(prev, FilterS(unwrap(p), true) :: acc)
        case Apply(TypeApply(Select(prev, "collect"), List(b)), List(pf))                   => parse(prev, CollectS(unwrap(pf), b.tpe) :: acc)
        case Apply(Select(prev, "take"), List(n))                                           => parse(prev, TakeS(unwrap(n)) :: acc)
        case Apply(Select(prev, "drop"), List(n))                                           => parse(prev, DropS(unwrap(n)) :: acc)
        case Apply(Select(prev, "takeWhile"), List(p))                                      => parse(prev, TakeWhileS(unwrap(p)) :: acc)
        case Apply(Select(prev, "dropWhile"), List(p))                                      => parse(prev, DropWhileS(unwrap(p)) :: acc)
        case Select(prev, "distinct")                                                       => parse(prev, DistinctS(None) :: acc)
        case Apply(TypeApply(Select(prev, "distinctBy"), _), List(f))                       => parse(prev, DistinctS(Some(unwrap(f))) :: acc)
        case Apply(Apply(TypeApply(Select(prev, "scanLeft"), List(b)), List(z)), List(op))  => parse(prev, ScanLeftS(unwrap(z), unwrap(op), b.tpe) :: acc)
        case Apply(Select(prev, "tapEach"), List(f))                                        => parse(prev, TapEachS(unwrap(f)) :: acc)
        case Select(prev, "zipWithIndex")                                                   => parse(prev, ZipWithIndexS :: acc)
        case Apply(TypeApply(Select(prev, "zip"), List(b)), List(that))                     => parse(prev, ZipS(unwrap(that), b.tpe, None) :: acc)
        case Apply(Apply(TypeApply(Select(prev, "map2"), List(b, _)), List(that)), List(f)) => parse(prev, ZipS(unwrap(that), b.tpe, Some(unwrap(f))) :: acc)
        case base @ Apply(Select(New(_), "<init>"), List(src))                              => (src.underlyingArgument, fuseElem(base.tpe), acc)
        case base @ Apply(TypeApply(Select(New(_), "<init>"), _), List(src))                => (src.underlyingArgument, fuseElem(base.tpe), acc)
        case other                                                                          =>
          report.errorAndAbort(s"fuse: unsupported pipeline step near:\n${other.show(using Printer.TreeStructure)}")

    /** beta-reduce `fn(args…)` to inline the lambda body (no closure); falls back to a real `.apply` call. */
    def applyN(fn: Term, args: List[Term]): Term =
      val app = Apply(Select.unique(fn, "apply"), args)
      Term.betaReduce(app).getOrElse(app)
    def applyLambda(fn: Term, arg: Term): Term = applyN(fn, List(arg))

    /** If `body` is a LITERAL fixed-arity `FArray(e0, e1, …)` construction (which inline-expands to a Block that allocates a `${K}Arr` + backing array), return
      * the element expressions `[e0, e1, …]` so a flatMap can splat them downstream with NO array allocation. The expanded shape (any leaf kind) is: Block(
      * ValDef("out", new Array[K](n)) :: out.update(0,e0) :: out.update(1,e1) :: … , new ${K}Arr(out, n) ) wrapped in `Inlined`/`Typed`/`TypeApply(_,
      * "$asInstanceOf$")`. Match it structurally (peel the wrappers, confirm the result is `new …Arr(<out>, _)`, then read the `<out>.update(i, e_i)` stores in
      * index order). A non-literal body (variable `FArray`, a `range`/`tabulate`, an empty/1-elem special-case, …) returns None.
      */
    def literalArrayElems(body: Term): Option[List[Term]] =
      // peel asInstanceOf / Typed / single-expr Inlined wrappers WITHOUT collapsing a stmt-bearing Block.
      def peel(t: Term): Term = t match
        case TypeApply(Select(inner, "$asInstanceOf$"), _)                          => peel(inner)
        case TypeApply(Select(inner, "asInstanceOf"), _)                            => peel(inner)
        case Typed(inner, _)                                                        => peel(inner)
        case Inlined(_, Nil, inner)                                                 => peel(inner)
        case Inlined(_, bindings, inner) if bindings.forall(_.isInstanceOf[ValDef]) => peel(inner)
        case _                                                                      => t
      peel(body) match
        case Block(stmts, last) =>
          // the result must be `new ${K}Arr(outIdent, lenLit)`; capture the `out` symbol. Covers every leaf:
          //   - PRIMITIVE (IntArr/LongArr/DoubleArr): stores are `Typed(e, K)` — strip the ascription to `e`.
          //   - REF (RefArr): stores are `(e: A).asInstanceOf[Object]` — strip the `.asInstanceOf[Object]` AND
          //     the `Typed(_, A)` to recover the clean A-typed `e`. The downstream segment expects an A-typed
          //     element, so the bare `e` (typed A by construction) re-splices correctly.
          val leafArr = Set("IntArr", "LongArr", "DoubleArr", "RefArr")
          val outName: Option[String] = peel(last) match
            case Apply(Select(New(tp), "<init>"), List(outRef, _)) if leafArr.contains(tp.tpe.typeSymbol.name) =>
              outRef match { case Ident(nm) => Some(nm); case _ => None }
            case _ => None
          outName.flatMap { nm =>
            // collect `nm.update(idx, elem)` stores; require they are exactly indices 0..k-1 and that `nm` is a
            // fresh `new Array[…](k)` local (so we are sure it is a self-contained literal, not an aliased array).
            val sizeOpt: Option[Int] = stmts.collectFirst {
              case ValDef(n, _, Some(rhs)) if n == nm =>
                peel(rhs) match
                  case Apply(TypeApply(Select(New(_), "<init>"), _), List(Literal(IntConstant(k)))) => k
                  case Apply(Select(New(_), "<init>"), List(Literal(IntConstant(k))))               => k
                  case _                                                                            => -1
            }
            // strip the store wrapper to the clean element expr: a prim `Typed(e, K)` ascription, or a Ref
            // `(e: A).asInstanceOf[Object]` cast (peel the cast, then the `Typed(_, A)`).
            def stripStore(t: Term): Term = t match
              case TypeApply(Select(inner, "asInstanceOf"), _) => stripStore(inner)
              case Typed(e, _)                                 => stripStore(e)
              case _                                           => t
            val updates: List[(Int, Term)] = stmts.collect {
              case Apply(Select(Ident(n), "update"), List(Literal(IntConstant(idx)), elem)) if n == nm => (idx, stripStore(elem))
            }
            sizeOpt match
              case Some(k) if k >= 0 && updates.length == k && updates.map(_._1).sorted == (0 until k).toList =>
                Some(updates.sortBy(_._1).map(_._2))
              case _ => None
          }
        case _ => None

    /** build the identity lambda `(a: A) => a` for element type `tpe` (used as the key of largest/smallest). */
    def identityLambda(tpe: TypeRepr): Term = tpe.asType match
      case '[a] => '{ (x: a) => x }.asTerm

    /** the `better(newKey, bestKey): Boolean` predicate for minBy/maxBy from an `Ordering[B]` term: `ord.lt` (min — a smaller new key wins) or `ord.gt` (max).
      * Mirrors the standalone `extremumByImpl` wiring.
      */
    def betterFromOrd(ordTerm: Term, bTpe: TypeRepr, max: Boolean): Term = bTpe.asType match
      case '[b] =>
        val ord = ordTerm.asExprOf[Ordering[b]]
        if max then '{ (k1: b, k2: b) => $ord.gt(k1, k2) }.asTerm
        else '{ (k1: b, k2: b) => $ord.lt(k1, k2) }.asTerm

    /** `{ val v = value; cont(v) }` — binds `value` to a fresh val (no recompute; owners handled by the quote). */
    def letBind(value: Term)(cont: Term => Term): Term =
      value.tpe.widen.asType match
        case '[t] => '{ val v: t = ${ value.asExprOf[t] }; ${ cont('v.asTerm).asExprOf[Unit] } }.asTerm

    /** like `letBind` but the body produces a VALUE (not a statement): the result type is whatever the continuation builds (e.g. a fold accumulator value), not
      * Unit. Used by the shape-aware fold/reduce path for a whole-product use.
      */
    def letBindV(value: Term)(cont: Term => Term): Term =
      value.tpe.widen.asType match
        case '[t] =>
          '{ val v: t = ${ value.asExprOf[t] }; ${ resultExpr(cont('v.asTerm)) } }.asTerm

    /** wrap a Term as an Expr of its own widened type (so a quote can splice it without forcing Unit). */
    def resultExpr(t: Term): Expr[Any] = t.tpe.widen.asType match { case '[r] => t.asExprOf[r] }

    /** read `src(idx)` unboxed at the given element kind (leaf fast-path lives inside `<kind>At`). */
    def readAtKind(elemTpe: TypeRepr, src: Expr[FBase], idx: Expr[Int]): Term = kindOf(elemTpe) match
      case Kind.KInt     => '{ FArrayOps.intAt($src, $idx) }.asTerm
      case Kind.KLong    => '{ FArrayOps.longAt($src, $idx) }.asTerm
      case Kind.KDouble  => '{ FArrayOps.doubleAt($src, $idx) }.asTerm
      case Kind.KFloat   => '{ FArrayOps.floatAt($src, $idx) }.asTerm
      case Kind.KShort   => '{ FArrayOps.shortAt($src, $idx) }.asTerm
      case Kind.KByte    => '{ FArrayOps.byteAt($src, $idx) }.asTerm
      case Kind.KChar    => '{ FArrayOps.charAt($src, $idx) }.asTerm
      case Kind.KBoolean => '{ FArrayOps.booleanAt($src, $idx) }.asTerm
      case Kind.KRef     => elemTpe.asType match { case '[b] => '{ FArrayOps.refAt($src, $idx).asInstanceOf[b] }.asTerm }

    /** the `(value, index)` pair for zipWithIndex; use the stdlib @specialized Tuple2 for a primitive element (Int/Long/Double) so it isn't boxed — exactly
      * like the eager zipWithIndex. Everything else boxes via a plain Tuple2 (correct for references and the rarer primitive kinds).
      */
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
    val (srcTerm, srcElem, stages0) = parse(selfTerm.underlyingArgument, Nil)

    // --- algebraic rewrites on adjacent stages (sound for a pure pipeline; removes redundant per-element
    //     machinery the JIT won't always eliminate). Collapses take/take and drop/drop that are adjacent OR
    //     separated only by LENGTH-AND-POSITION-PRESERVING stages (map / tapEach), by sliding the limit stage
    //     LEFT past such a stage first (`map(f).take(n)` ≡ `take(n).map(f)`, likewise drop). Then:
    //       take(n).take(m) → take(min(n,m))   (positions are post-prior-take, so the tighter bound wins)
    //       drop(n).drop(m) → drop(n+m)        (drop m more after dropping the first n)
    //     so e.g. `take(5).map(f).take(3)` → `take(3).map(f)` and `drop(a).map(f).drop(b)` → `drop(a+b).map(f)`.
    //     The int args may be runtime Terms, so min/+ are emitted as terms. We DON'T slide past filter (changes
    //     positions/length), flatMap/scan (changes length), takeWhile/dropWhile/distinct/zip (likewise). map.map
    //     already fuses into one loop with column-level DCE, so no rewrite is needed there. ---
    // May the limit `lim` commute LEFT past stage `s`? Only `take` past `map`: in the current lowering take's
    // done-flag already halts the loop at the bound, so `map(f).take(n)` evaluates `f` on exactly the same first-n
    // elements as `take(n).map(f)` — identical even if `f` throws/side-effects. `drop` must NOT slide past map:
    // `map(f).drop(k)` evaluates `f` on ALL elements (then drops k results), whereas `drop(k).map(f)` skips `f` on
    // the first k — a behavior change for an effectful/throwing `f`. Nothing slides past tapEach (observable
    // effects) or filter/flatMap/scan/while/distinct/zip (these change positions or length).
    def slidesLeftPast(lim: Stage, s: Stage): Boolean = (lim, s) match
      case (TakeS(_), MapS(_)) => true
      case _                   => false
    // collapse two adjacent same-kind limit stages: take/take → min, drop/drop → +. (`l` precedes `r`.)
    def collapse(l: Stage, r: Stage): Option[Stage] = (l, r) match
      case (TakeS(n), TakeS(m)) => Some(TakeS('{ java.lang.Math.min(${ n.asExprOf[Int] }, ${ m.asExprOf[Int] }) }.asTerm))
      case (DropS(n), DropS(m)) => Some(DropS('{ ${ n.asExprOf[Int] } + ${ m.asExprOf[Int] } }.asTerm))
      case _                    => None
    // ONE left-to-right pass. At each stage we first rewrite the tail, then either fuse with the head limit it
    // produced (adjacent take/take, drop/drop) or — for a preserving stage followed by a limit — pull that limit
    // IN FRONT (`map(f).take(n)` → `take(n).map(f)`) and re-run from there so it can fuse with a prior limit.
    def rewriteStages(ss: List[Stage]): List[Stage] = ss match
      case Nil       => Nil
      case s :: rest =>
        val tail = rewriteStages(rest)
        (s, tail) match
          case (TakeS(_) | DropS(_), (l @ (TakeS(_) | DropS(_))) :: more) if collapse(s, l).isDefined =>
            rewriteStages(collapse(s, l).get :: more) // fuse with the immediately-following limit
          case (_, (l @ (TakeS(_) | DropS(_))) :: more) if slidesLeftPast(l, s) =>
            rewriteStages(l :: s :: more) // commute the limit left of this preserving stage
          case _ => s :: tail
    val stages = rewriteStages(stages0)

    // --- multi-aggregate spec (only for TTag.Agg): one per `Agg.xxx(...)` call in the agg(...) arg list ---
    enum AggSpec:
      case Sum(f: Term, bTpe: TypeRepr) // Σ f(a)
      case Count // Σ 1
      case Foreach(f: Term) // run f(a) for side effect → Unit
      case Extremum(f: Term, bTpe: TypeRepr, max: Boolean, bare: Boolean) // min/max f(a) → Option[B] (or B if bare)
      case Fold(z: Term, op: Term, sTpe: TypeRepr) // seeded left fold
      case Avg(f: Term) // mean of f(a): Double
      case TopN(key: Term, aTpe: TypeRepr, bTpe: TypeRepr, n: Term, ord: Term, largest: Boolean) // top-n by key(a), best-first → FArray[A]
      case Reduce(op: Term, aTpe: TypeRepr, bare: Boolean) // seeded reduce op(acc, a) → Option[B] (or B if bare)
      case ExtremumByElem(f: Term, bTpe: TypeRepr, better: Term, bare: Boolean) // best ELEMENT by key f(a) → Option[A] (or A if bare)
    def parseAggList(t: Term): List[AggSpec] =
      // the arg is either `List(a1, a2, …)` (from agg/aggTo) or a bare varargs Seq (from planAgg); pull elements.
      def elems(x: Term): List[Term] = unwrap(x) match
        case Apply(TypeApply(sel, _), List(seq)) if sel.symbol.name == "apply" => seqElems(seq)
        case Apply(sel, List(seq)) if sel.symbol.name == "apply"               => seqElems(seq)
        case Typed(Repeated(_, _), _) | Repeated(_, _)                         => seqElems(x) // planAgg varargs Seq
        case _                                                                 => report.errorAndAbort(s"fuse: agg expects Agg.* arguments; got ${x.show}")
      def seqElems(seq: Term): List[Term] = unwrap(seq) match
        case Typed(Repeated(es, _), _) => es
        case Repeated(es, _)           => es
        case Apply(_, List(inner))     => seqElems(inner) // wrapRefArray(…)
        case _                         => report.errorAndAbort(s"fuse: agg arg not a literal list: ${seq.show}")
      def typeArgOf(t: Term, i: Int): TypeRepr = t match
        case TypeApply(_, ts) if ts.length > i => ts(i).tpe
        case Apply(fn, _)                      => typeArgOf(fn, i)
        case _                                 => TypeRepr.of[Any]
      def one(a: Term): AggSpec = unwrap(a) match
        case Apply(Apply(fn @ TypeApply(Select(_, "sum"), _), List(f)), _)     => AggSpec.Sum(unwrap(f), typeArgOf(fn, 1))
        case Apply(fn @ TypeApply(Select(_, "sum"), _), List(f))               => AggSpec.Sum(unwrap(f), typeArgOf(fn, 1))
        case TypeApply(Select(_, "count"), _)                                  => AggSpec.Count
        case Apply(Apply(fn @ TypeApply(Select(_, "min"), _), List(f)), _)     => AggSpec.Extremum(unwrap(f), typeArgOf(fn, 1), false, false)
        case Apply(Apply(fn @ TypeApply(Select(_, "max"), _), List(f)), _)     => AggSpec.Extremum(unwrap(f), typeArgOf(fn, 1), true, false)
        case Apply(Apply(fn @ TypeApply(Select(_, "min1"), _), List(f)), _)    => AggSpec.Extremum(unwrap(f), typeArgOf(fn, 1), false, true)
        case Apply(Apply(fn @ TypeApply(Select(_, "max1"), _), List(f)), _)    => AggSpec.Extremum(unwrap(f), typeArgOf(fn, 1), true, true)
        case Apply(Apply(TypeApply(Select(_, "fold"), st), List(z)), List(op)) => AggSpec.Fold(unwrap(z), unwrap(op), st.last.tpe)
        case Apply(TypeApply(Select(_, "foreach"), _), List(f))                => AggSpec.Foreach(unwrap(f))
        case Apply(fn @ TypeApply(Select(_, "avg"), _), List(f))               => AggSpec.Avg(unwrap(f))
        // topNBy/bottomNBy(n)(key)(using ord): Apply(Apply(Apply(TypeApply(Select,_,name), [A,B]),[n]),[key]),[ord]
        case Apply(Apply(Apply(fn @ TypeApply(Select(_, nm @ ("topNBy" | "bottomNBy")), _), List(n)), List(k)), List(ord)) =>
          AggSpec.TopN(unwrap(k), typeArgOf(fn, 0), typeArgOf(fn, 1), unwrap(n), unwrap(ord), nm == "topNBy")
        // largest/smallest(n)(using ord): key is identity. Apply(Apply(TypeApply(Select,_,name),[A]),[n]),[ord]
        case Apply(Apply(fn @ TypeApply(Select(_, nm @ ("largest" | "smallest")), _), List(n)), List(ord)) =>
          AggSpec.TopN(identityLambda(typeArgOf(fn, 0)), typeArgOf(fn, 0), typeArgOf(fn, 0), unwrap(n), unwrap(ord), nm == "largest")
        // reduce/reduce1/reduceL(op): seeded reduce → Option[B] (or B). Apply(TypeApply(Select,_,name),[A,B]),[op]
        case Apply(fn @ TypeApply(Select(_, nm @ ("reduce" | "reduce1" | "reduceL")), _), List(op)) =>
          AggSpec.Reduce(unwrap(op), typeArgOf(fn, 1), nm == "reduce1")
        // minBy/maxBy/minBy1/maxBy1(f)(using ord): best ELEMENT by key. Apply(Apply(TypeApply(Select,_,name),[A,B]),[f]),[ord]
        case Apply(Apply(fn @ TypeApply(Select(_, nm @ ("minBy" | "maxBy" | "minBy1" | "maxBy1")), _), List(f)), List(ord)) =>
          val bTpe = typeArgOf(fn, 1); val isMax = nm.startsWith("max")
          AggSpec.ExtremumByElem(unwrap(f), bTpe, betterFromOrd(unwrap(ord), bTpe, isMax), nm.endsWith("1"))
        case other =>
          report.errorAndAbort(s"fuse: unsupported agg — use Agg.sum/count/min/max/avg/fold/reduce/minBy/maxBy/topNBy/bottomNBy/largest/smallest. Got:\n${other
              .show(using Printer.TreeStructure)}")
      elems(t).map(one)

    // --- unboxed primitive ops for Agg.sum / Agg.min/max (else fall back to Numeric/Ordering, which box). ---
    def isPrim(t: TypeRepr, p: TypeRepr): Boolean = t =:= p
    def numZero(bTpe: TypeRepr): Term =
      if isPrim(bTpe, TypeRepr.of[Int]) then '{ 0 }.asTerm
      else if isPrim(bTpe, TypeRepr.of[Long]) then '{ 0L }.asTerm
      else if isPrim(bTpe, TypeRepr.of[Double]) then '{ 0.0 }.asTerm
      else if isPrim(bTpe, TypeRepr.of[Float]) then '{ 0.0f }.asTerm
      else
        bTpe.asType match
          case '[bb] => '{ ${ Expr.summon[Numeric[bb]].getOrElse(report.errorAndAbort(s"fuse: no Numeric for ${bTpe.show} in Agg.sum")) }.zero }.asTerm
    def numAdd(bTpe: TypeRepr, a: Term, b: Term): Term =
      if isPrim(bTpe, TypeRepr.of[Int]) then '{ ${ a.asExprOf[Int] } + ${ b.asExprOf[Int] } }.asTerm
      else if isPrim(bTpe, TypeRepr.of[Long]) then '{ ${ a.asExprOf[Long] } + ${ b.asExprOf[Long] } }.asTerm
      else if isPrim(bTpe, TypeRepr.of[Double]) then '{ ${ a.asExprOf[Double] } + ${ b.asExprOf[Double] } }.asTerm
      else if isPrim(bTpe, TypeRepr.of[Float]) then '{ ${ a.asExprOf[Float] } + ${ b.asExprOf[Float] } }.asTerm
      else
        bTpe.asType match
          case '[bb] =>
            '{
              ${ Expr.summon[Numeric[bb]].getOrElse(report.errorAndAbort(s"fuse: no Numeric for ${bTpe.show} in Agg.sum")) }
                .plus(${ a.asExprOf[bb] }, ${ b.asExprOf[bb] })
            }.asTerm

    /** `kk wins over best` — `kk > best` for max / `kk < best` for min, unboxed for primitives. */
    def ordWins(bTpe: TypeRepr, kk: Term, best: Term, isMax: Boolean): Expr[Boolean] =
      def prim[T: Type](k: Expr[T], b: Expr[T])(gt: (Expr[T], Expr[T]) => Expr[Boolean], lt: (Expr[T], Expr[T]) => Expr[Boolean]): Expr[Boolean] =
        if isMax then gt(k, b) else lt(k, b)
      if isPrim(bTpe, TypeRepr.of[Int]) then prim(kk.asExprOf[Int], best.asExprOf[Int])((x, y) => '{ $x > $y }, (x, y) => '{ $x < $y })
      else if isPrim(bTpe, TypeRepr.of[Long]) then prim(kk.asExprOf[Long], best.asExprOf[Long])((x, y) => '{ $x > $y }, (x, y) => '{ $x < $y })
      else if isPrim(bTpe, TypeRepr.of[Double]) then prim(kk.asExprOf[Double], best.asExprOf[Double])((x, y) => '{ $x > $y }, (x, y) => '{ $x < $y })
      else if isPrim(bTpe, TypeRepr.of[Float]) then prim(kk.asExprOf[Float], best.asExprOf[Float])((x, y) => '{ $x > $y }, (x, y) => '{ $x < $y })
      else
        bTpe.asType match
          case '[bb] =>
            val ord = Expr.summon[Ordering[bb]].getOrElse(report.errorAndAbort(s"fuse: no Ordering for ${bTpe.show} in Agg.min/max"))
            if isMax then '{ $ord.gt(${ kk.asExprOf[bb] }, ${ best.asExprOf[bb] }) } else '{ $ord.lt(${ kk.asExprOf[bb] }, ${ best.asExprOf[bb] }) }

    // Is this a byte-backed JSON NDJSON source (`Json.ndjson[T](bytes).fuse…`) rather than an in-memory FArray?
    // Detected by the source term's static type; if so we lower to a per-record scanner (see loopOverJson).
    // The RUNTIME source: read the value the `Fuse` wrapper already holds (`self.base`), NOT a re-splice of the
    // raw source expression. The source has already been evaluated once when the `Fuse` was constructed; splicing
    // the original (often INLINED, e.g. `FArray.fromIterable(...)`) tree a SECOND time emits a duplicate inlined
    // block whose internal `$proxy` bindings collide with the first copy — yielding silently WRONG results (a
    // hard-won finding: `filter.map.take` returned `[12,0]` instead of `[-12,12]` whenever the source wasn't a
    // bare local val). `srcTerm` is still used below for COMPILE-TIME type analysis only. `Fuse.base: AnyRef`.
    val selfBase: Expr[AnyRef] = '{ ${ self }.base }

    val jsonSrc: Option[Expr[NdjsonSource[?]]] =
      // match on the type SYMBOL (NdjsonSource is invariant in T, so NdjsonSource[Rec] is not <:< NdjsonSource[Any]).
      if srcTerm.tpe.widen.dealias.typeSymbol == TypeRepr.of[NdjsonSource[Any]].typeSymbol
      then Some('{ $selfBase.asInstanceOf[NdjsonSource[?]] })
      else None
    val isJson = jsonSrc.isDefined

    // a streaming `Source[A]` source (covariant, so check by subtyping against `Source[Any]`). The chunk driver
    // wraps the in-memory element loop in an outer pull loop → constant memory over an incremental source.
    val streamSrc: Option[Expr[Source[?]]] =
      if !isJson && srcTerm.tpe.widen.dealias <:< TypeRepr.of[Source[Any]]
      then Some('{ $selfBase.asInstanceOf[Source[?]] })
      else None
    val isStream = streamSrc.isDefined

    // identity → hand back the source (only meaningful for Run, and only for an already-realized in-memory FArray).
    if stages.isEmpty && tag == TTag.Run && !isJson && !isStream then return '{ $selfBase.asInstanceOf[FBase] }

    val srcK = if isJson then Kind.KRef else kindOf(srcElem)
    val outK = kindOf(aType)
    val srcBase: Expr[FBase] = if isJson || isStream then '{ null.asInstanceOf[FBase] } else '{ $selfBase.asInstanceOf[FBase] }

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
    val hasZip: Boolean = stages.exists(_.isInstanceOf[ZipS])
    val hasFlatMap: Boolean = stages.exists(_.isInstanceOf[FlatMapS])
    val hasTakeWhile: Boolean = stages.exists(_.isInstanceOf[TakeWhileS])
    val scanCount: Int = stages.count(_.isInstanceOf[ScanLeftS])
    if scanCount > 1 then report.errorAndAbort("fuse: at most one scanLeft per pipeline")
    val hasScan: Boolean = scanCount == 1
    // a JSON source has no static length (records are scanned, not indexed), so a Run/toList must grow the output
    // like flatMap/scan. scanLeft emits one MORE element than its input, so the output can exceed source length.
    // A JSON or streaming source has no static length, so a Run/toList must GROW the output (no `capExpr` bound).
    val needsGrow: Boolean = hasFlatMap || hasScan || isJson || isStream
    // v1 JSON scope guard: only map/filter stages over a JSON source (no zip/flatMap/take/drop/scan/distinct yet).
    if isJson then
      val unsupported = stages.find {
        case MapS(_) | FilterS(_, _) => false
        case _                       => true
      }
      unsupported.foreach(s =>
        report.errorAndAbort(
          s"fuse-json (v1): only `map` and `filter` stages are supported over a JSON source so far (got ${s.getClass.getSimpleName.stripSuffix("S")}). " +
            "Project/filter then use a terminal (sum/fold/toList/count/foreach)."
        )
      )
    // the scanLeft stage index + the stages downstream of it (run once with z as a prologue, then per element)
    val scanInfo: Option[(Int, List[(Stage, Int)])] =
      indexed.collectFirst { case (ScanLeftS(_, _, _), i) => i }.map(i => (i, indexed.drop(i + 1)))
    val shortCircuit: Boolean = tag match
      case TTag.Find | TTag.Exists | TTag.Forall | TTag.HeadOption | TTag.Head | TTag.IndexWhere => true
      case _                                                                                     => false
    val needsDone: Boolean = takesPresent || shortCircuit || hasZip || hasTakeWhile // any stage that can stop the stream
    // Does this terminal IGNORE the element value? If so, a trailing pure `map` produces a DEAD value — nothing
    // reads it — so its final shape must NOT be materialized (`map(_.category).count` must not decode the
    // strings; `map(expensive).count` must not run `expensive`). The filters still run.
    //   - count                : always discards the value.
    //   - exists/forall(p)      : discard the value IFF the predicate `p` ignores its argument — i.e. `_ => true`
    //                             / `_ => false`, which is exactly how `isEmpty`/`nonEmpty` desugar.
    // (lazy: `decomposeLambda`/`collectPaths` are defined further down; this is forced only later, in
    //  continueShape/buildBody.)
    lazy val discardsElem: Boolean = tag match
      // `agg(Agg.count)` (what the standalone `count` desugars to) reads nothing → the trailing map's value is dead.
      // Likewise `agg(Agg.foreach(p))` where the side-effect ignores its argument (the `isEmpty`/`nonEmpty`-style
      // discard). Only when EVERY spec discards (a single Count, or a value-ignoring Foreach).
      case TTag.Agg =>
        parseAggList(args(0)) match
          case List(AggSpec.Count)      => true
          case List(AggSpec.Foreach(f)) =>
            decomposeLambda(f) match
              case Some((param, body)) => collectPaths(body, param).isEmpty
              case None                => false
          case _ => false
      case TTag.Exists | TTag.Forall =>
        decomposeLambda(args(0)) match
          case Some((param, body)) => collectPaths(body, param).isEmpty // predicate never reads its argument
          case None                => false
      case _ => false

    // ===== Layer B: pure map/filter segment optimizer (compute-for-survivors via memoized lazy columns) =====

    /** a column that binds to a val on first read (memoized), then yields the ref. */
    def memoScalar(compute: (Term => Term) => Term): Shape =
      var bound: Option[Term] = None
      Sc(k =>
        bound match
          case Some(r) => k(r)
          case None    => compute(v => letBind(v)(r => { bound = Some(r); k(r) }))
      )

    def srcShape(cur: Term): Shape = Sc(k => k(cur)) // already bound by the loop / an upstream stage

    /** materialize a shape to a single Term (binding columns as needed); a decomposed product is rebuilt. */
    def readShape(shape: Shape)(k: Term => Term): Term = shape match
      case Sc(read)            => read(k)
      case Tup(parts, rebuild) => readAll(parts, Nil)(ts => k(rebuild(ts)))
    def readAll(parts: List[Shape], acc: List[Term])(k: List[Term] => Term): Term = parts match
      case Nil       => k(acc.reverse)
      case p :: rest => readShape(p)(t => readAll(rest, t :: acc)(k))

    /** materialize a 2-tuple. If BOTH fields are primitive (Int/Long/Double), build the stdlib @specialized `Tuple2$mcXY$sp` subclass so the fields stay
      * UNBOXED, then cast to the generic `(A, B)` — a runtime no-op (it really IS a Tuple2). Mixed/reference fields fall back to the generic `Tuple2` (only a
      * primitive in a generic slot would box, and that's Scala's Tuple2 erasure, not ours). Tuple3 isn't @specialized.
      */
    def mkPair(a: Term, b: Term): Term =
      val ti = TypeRepr.of[Int]; val tj = TypeRepr.of[Long]; val td = TypeRepr.of[Double]
      val ta = a.tpe.widen; val tb = b.tpe.widen
      def is(t: TypeRepr, u: TypeRepr) = t =:= u
      if is(ta, ti) && is(tb, ti) then '{ new scala.Tuple2$mcII$sp(${ a.asExprOf[Int] }, ${ b.asExprOf[Int] }).asInstanceOf[(Int, Int)] }.asTerm
      else if is(ta, ti) && is(tb, tj) then '{ new scala.Tuple2$mcIJ$sp(${ a.asExprOf[Int] }, ${ b.asExprOf[Long] }).asInstanceOf[(Int, Long)] }.asTerm
      else if is(ta, ti) && is(tb, td) then '{ new scala.Tuple2$mcID$sp(${ a.asExprOf[Int] }, ${ b.asExprOf[Double] }).asInstanceOf[(Int, Double)] }.asTerm
      else if is(ta, tj) && is(tb, ti) then '{ new scala.Tuple2$mcJI$sp(${ a.asExprOf[Long] }, ${ b.asExprOf[Int] }).asInstanceOf[(Long, Int)] }.asTerm
      else if is(ta, tj) && is(tb, tj) then '{ new scala.Tuple2$mcJJ$sp(${ a.asExprOf[Long] }, ${ b.asExprOf[Long] }).asInstanceOf[(Long, Long)] }.asTerm
      else if is(ta, tj) && is(tb, td) then '{ new scala.Tuple2$mcJD$sp(${ a.asExprOf[Long] }, ${ b.asExprOf[Double] }).asInstanceOf[(Long, Double)] }.asTerm
      else if is(ta, td) && is(tb, ti) then '{ new scala.Tuple2$mcDI$sp(${ a.asExprOf[Double] }, ${ b.asExprOf[Int] }).asInstanceOf[(Double, Int)] }.asTerm
      else if is(ta, td) && is(tb, tj) then '{ new scala.Tuple2$mcDJ$sp(${ a.asExprOf[Double] }, ${ b.asExprOf[Long] }).asInstanceOf[(Double, Long)] }.asTerm
      else if is(ta, td) && is(tb, td) then
        '{ new scala.Tuple2$mcDD$sp(${ a.asExprOf[Double] }, ${ b.asExprOf[Double] }).asInstanceOf[(Double, Double)] }.asTerm
      else '{ scala.Tuple2(${ a.asExpr }, ${ b.asExpr }) }.asTerm
    def mkTuple(ts: List[Term]): Term = ts match
      case List(a, b)    => mkPair(a, b)
      case List(a, b, c) => '{ scala.Tuple3(${ a.asExpr }, ${ b.asExpr }, ${ c.asExpr }) }.asTerm
      case _             => report.errorAndAbort("fuse: only tuple arities 2 and 3 are supported here")

    /** build a Tuple of arity 2–4 from the given field terms (used by the `agg` result). A single aggregate returns its result BARE (no Tuple1) — used by the
      * standalone `topN`/`reduce`-style sugar.
      */
    def mkTupleN(ts: List[Term]): Term = ts match
      case List(a)          => a
      case List(a, b)       => mkPair(a, b)
      case List(a, b, c)    => '{ scala.Tuple3(${ a.asExpr }, ${ b.asExpr }, ${ c.asExpr }) }.asTerm
      case List(a, b, c, d) => '{ scala.Tuple4(${ a.asExpr }, ${ b.asExpr }, ${ c.asExpr }, ${ d.asExpr }) }.asTerm
      case _                => report.errorAndAbort(s"fuse: agg supports 1–4 aggregates (got ${ts.length})")

    /** the (label, type) of each field of a product type (case class / tuple / named tuple), else None. This is reflect's view of `Mirror.ProductOf` —
      * `caseFields` are the product's accessors in declaration order.
      */
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
    /** a 2-param lambda `(p0, p1) => body` → (p0 sym, p1 sym, body). Used to decompose a fold/reduce `op`. */
    def decomposeLambda2(t: Term): Option[(Symbol, Symbol, Term)] = unwrap(t) match
      case Lambda(List(vd0, vd1), body) => Some((vd0.symbol, vd1.symbol, body))
      case _                            => None

    def decomposeLambda(t: Term): Option[(Symbol, Term)] = unwrap(t) match
      case Lambda(List(vd), body) => Some((vd.symbol, body))
      case _                      => None

    /** the index of field `name` in `inner`'s product type, if `inner.name` is a product-field access. */
    def fieldAccess(inner: Term, name: String): Option[Int] =
      productFields(inner.tpe.widen).flatMap { fs =>
        val i = fs.indexWhere(_._1 == name); if i >= 0 then Some(i) else None
      }
    def isFieldSelect(t: Term): Option[(Term, Int, String)] = t match
      case Select(inner, name) => fieldAccess(inner, name).map(i => (inner, i, name))
      case _                   => None

    /** a CANONICAL product construction `C(a, b, …)` / `(a, b)` / `new C(…)` → (product type, field args). Only the product's OWN apply/constructor counts (not
      * an arbitrary factory returning C), so args == fields.
      */
    def isProductCtor(t: Term): Option[(TypeRepr, List[Term])] =
      val rt = t.tpe.widen
      productFields(rt) match
        case Some(fields) =>
          def appliedTo(fn: Term, as: List[Term]): Option[(TypeRepr, List[Term])] =
            if as.length != fields.length then None
            else
              fn match
                case Select(New(_), "<init>")                                                                  => Some((rt, as))
                case sel @ Select(_, "apply") if sel.symbol.owner == rt.typeSymbol.companionModule.moduleClass => Some((rt, as))
                case _                                                                                         => None
          t match
            case Apply(TypeApply(fn, _), as) => appliedTo(fn, as)
            case Apply(fn, as)               => appliedTo(fn, as)
            case _                           => None
        case None => None
    // a maximal field PATH rooted at a symbol: `p.a.x` -> (p, List((iₐ,"a"),(iₓ,"x"))); a bare `p` -> (p, Nil).
    def projPath(t: Term): Option[(Symbol, Path)] = t match
      case id: Ident           => Some((id.symbol, Nil))
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
          case _                               => super.transformTerm(t)(owner)
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
      case Nil               => cur
      case (j, name) :: rest =>
        cur match
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

    /** the immediate sub-terms of `t` worth CSE-ing — only plain value expressions (Apply/Select/TypeApply), so we never try to bind a varargs/repeated/by-name
      * child. `&&`/`||` are left whole to keep short-circuiting.
      */
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
      else memoScalar(k => readAll(children.map(c => cse(c, cseT)), Nil)(refs => k(replaceChildren(t, children.zip(refs).toMap))))

    /** inline a `{ val a = …; val b = …; expr }` block (e.g. the desugaring of an untupled `(a, b) => …` lambda) into `expr`, so the result expression's
      * tuple/projection structure becomes visible to `interp`. Inlining may duplicate a binding's rhs, but CSE re-shares it. Last-binding-first so earlier vals
      * rewrite later ones.
      */
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
        case None              =>
          isFieldSelect(body) match
            case Some((inner, idx, name)) =>
              interp(inner, param, cur, cseT) match
                case Tup(ps, _) if idx >= 0 && idx < ps.length => ps(idx)
                case sc                                        => projField(sc, name)
            case None =>
              body match
                case id: Ident if id.symbol == param => cur
                case _                               => Sc(k => substColumns(body, param, cur)(subst => readShape(cse(subst, cseT))(k)))

    /** substitute param-uses in an atomic body with the columns it reads (binding only those columns). A bare param use (Nil path) forces materializing the
      * whole product; otherwise each path resolves to a leaf column.
      */
    def substColumns(body: Term, param: Symbol, cur: Shape)(k: Term => Term): Term =
      val paths = collectPaths(body, param)
      // whole-param use → substitute the materialized product for every param mention. A Tup must be rebuilt —
      // bind it to ONE val first so it isn't rebuilt per occurrence; a Sc is already a bound ref.
      if paths.contains(Nil) then
        cur match
          case Tup(_, _) => readShape(cur)(ct => letBind(ct)(r => k(substParam(body, param, r))))
          case _         => readShape(cur)(ct => k(substParam(body, param, ct)))
      else readPaths(cur, paths, Map.empty)(refs => k(substPaths(body, param, refs)))
    def readPaths(cur: Shape, paths: List[Path], acc: Map[Path, Term])(k: Map[Path, Term] => Term): Term =
      paths match
        case Nil       => k(acc)
        case p :: rest => readShape(navigate(cur, p))(ref => readPaths(cur, rest, acc + (p -> ref))(k))

    /** a local `case class` defined inside a lambda body can't be inlined into the fused loop — its synthesized companion (`apply`/`unapply`/`copy`/product
      * members) doesn't relocate (true of `betaReduce` too, not just our TreeMap), so any relocation yields "reference … outside scope". Detect it and fail
      * with a clear message instead of the cryptic one. (Local `def`s, non-case `class`es, vals, and TOP-LEVEL case classes all work.)
      */
    def checkNoLocalCaseClass(t: Term): Unit =
      (new TreeTraverser:
        override def traverseTree(x: Tree)(o: Symbol): Unit = x match
          case cd: ClassDef if cd.symbol.flags.is(Flags.Case) =>
            report.errorAndAbort(
              s"fuse: a `case class` (`${cd.name}`) defined INSIDE a lambda body isn't supported — the macro can't " +
                s"relocate its synthesized companion into the fused loop. Define the case class at the top level (a " +
                s"top-level case class, incl. generic, decomposes fully), or use a local non-case `class`.",
              cd.pos
            )
          case _ => super.traverseTree(x)(o)
      ).traverseTree(t)(Symbol.spliceOwner)

    def applyMap(f: Term, cur: Shape): Shape = decomposeLambda(f) match
      case Some((param, body)) => checkNoLocalCaseClass(body); interp(body, param, cur, scala.collection.mutable.Map.empty)
      case None                => memoScalar(k => readShape(cur)(ct => k(applyLambda(f, ct))))

    /** read a filter predicate (binding only the columns it touches), then hand the Boolean term to `k`. */
    def predShape(p: Term, neg: Boolean, cur: Shape)(k: Term => Term): Term =
      def fin(b: Term): Term = k(if neg then '{ ! ${ b.asExprOf[Boolean] } }.asTerm else b)
      decomposeLambda(p) match
        case Some((param, body)) => checkNoLocalCaseClass(body); readShape(interp(body, param, cur, scala.collection.mutable.Map.empty))(fin)
        case None                => readShape(cur)(ct => fin(applyLambda(p, ct)))

    /** apply a fold/reduce `op: (Z, A) => Z` to the running accumulator `accT` and the element SHAPE, decomposing the element param so `op`'s body reads only
      * the fields it projects (no whole-product rebuild for a body like `acc + r.amount`). `k(result)` consumes the new accumulator value (typically
      * `acc = result`, Unit). Falls back to materialize-then-apply for a non-2-param op.
      */
    def opOnShape(op: Term, accT: Term, cur: Shape)(k: Term => Term): Term = decomposeLambda2(op) match
      case Some((accSym, elemSym, body0)) =>
        checkNoLocalCaseClass(body0)
        // substitute the acc param with accT (acc is a scalar var, not decomposable), then decompose the element
        // param's field accesses against `cur`. substColumns materializes only on whole-param use (r.method()).
        val body = substParam(body0, accSym, accT)
        fnOnShape2(body, elemSym, cur)(k)
      case None => readShape(cur)(ct => k(applyN(op, List(accT, ct))))

    /** decompose `param`'s field accesses in `body` against `cur` (no rebuild unless `param` is used whole), then hand the resulting expression to `k`. Like
      * `substColumns` but the continuation result is unconstrained (not forced to Unit), so it can build a value (a new accumulator) rather than a statement.
      */
    def fnOnShape2(body: Term, param: Symbol, cur: Shape)(k: Term => Term): Term =
      val paths = collectPaths(body, param)
      if paths.contains(Nil) then
        // whole-param use → materialize once. A trivial result (an already-bound Ident/Literal — e.g. a scalar
        // element used whole, like `acc + a`) needs no fresh val; bind only a non-trivial product rebuild.
        readShape(cur)(ct =>
          if trivial(ct) then k(substParam(body, param, ct))
          else letBindV(ct)(r => k(substParam(body, param, r)))
        )
      else readPaths(cur, paths, Map.empty)(refs => k(substPaths(body, param, refs)))

    /** apply a 1-param `f: A => B` to the element SHAPE, decomposing the param (no rebuild for `r => r.field`). */
    def fnOnShape(f: Term, cur: Shape)(k: Term => Term): Term = decomposeLambda(f) match
      case Some((param, body)) => checkNoLocalCaseClass(body); fnOnShape2(body, param, cur)(k)
      case None                => readShape(cur)(ct => k(applyLambda(f, ct)))

    /** lower a contiguous map/filter run; columns sink past filters automatically (lazy memoized reads). */
    def buildSegment(ss: List[Stage], cur: Shape)(k: Shape => Term): Term = ss match
      case Nil                     => k(cur)
      case MapS(f) :: rest         => buildSegment(rest, applyMap(f, cur))(k)
      case FilterS(p, neg) :: rest => predShape(p, neg, cur)(b => If(b, buildSegment(rest, cur)(k), unit))
      case _                       => k(cur) // segment is map/filter only

    /** continue downstream from a decomposed Shape: run the leading map/filter run column-aware (so independent columns sink/DCE), then materialize and hand
      * off to buildBody for the rest. Used by collect / zipWithIndex / zip so a produced value stays decomposed through the following segment.
      */
    def continueShape(shape: Shape, rest: List[(Stage, Int)], ctx: Ctx): Term =
      val (seg, tail) = rest.span { case (MapS(_), _) => true; case (FilterS(_, _), _) => true; case _ => false }
      // DCE to the terminal: if this is the last segment and the terminal discards the element (count), don't
      // materialize the final shape — its value is dead (e.g. `map(_.category).count` must not decode the string).
      if tail.isEmpty && discardsElem then buildSegment(seg.map(_._1), shape)(_ => ctx.consume(unit))
      else if tail.isEmpty && ctx.consumeShape.isDefined then
        // shape-aware terminal (fold/reduce/agg): hand the decomposed shape so the terminal's lambdas project
        // only the fields they read — NEVER materialize the whole product (which would read dead placeholder
        // columns: the `{ null; …; 0.0 }` rebuild bug on a JSON record fed to agg with no leading filter).
        buildSegment(seg.map(_._1), shape)(fs => ctx.consumeShape.get(fs))
      else buildSegment(seg.map(_._1), shape)(fs => readShape(fs)(t => buildBody(tail, t, ctx)))

    /** lower `collect(pf)` by inlining the PartialFunction literal's match into the loop. Scala 3 encodes a `{ case … }` literal as `new PartialFunction { def
      * applyOrElse(x, default) = x match { <cases>; case _ => default(x) } … }`; we splice that match with scrutinee = `cur`, each real case continuing
      * downstream and the `default(x)` fallthrough → skip. Falls back to isDefinedAt/apply if the literal's shape is unusual.
      */
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
            CaseDef(cd.pattern, cd.guard.map(subX(xSym)), continueShape(interp(cd.rhs, xSym, srcShape(cur), scala.collection.mutable.Map.empty), rest, ctx))
          )
          // append a skip default unless the user's match is already total (avoids an unreachable-case warning)
          val newCases = if cases.exists(isCatchAll) then mapped else mapped :+ CaseDef(Wildcard(), None, unit)
          Match(cur, newCases).changeOwner(Symbol.spliceOwner)
        case None =>
          bTpe.asType match
            case '[bb] =>
              '{
                val pf0 = ${ pf.asExpr }.asInstanceOf[PartialFunction[Any, Any]]
                if pf0.isDefinedAt(${ cur.asExpr }) then ${ buildBody(rest, '{ pf0(${ cur.asExpr }).asInstanceOf[bb] }.asTerm, ctx).asExprOf[Unit] }
              }.asTerm

    // --- per-element body: map/filter/take/drop/flatMap chain, ending in `ctx.consume` ---
    def buildBody(ss: List[(Stage, Int)], cur: Term, ctx: Ctx): Term = ss match
      case Nil =>
        ctx.consumeShape match
          case Some(cs) => cs(srcShape(cur)) // shape-aware terminal directly on the source element
          case None     => ctx.consume(cur)
      case (MapS(_) | FilterS(_, _), _) :: _ =>
        val (seg, rest) = ss.span { case (MapS(_), _) => true; case (FilterS(_, _), _) => true; case _ => false }
        // when this is the last segment AND the terminal discards the element (count), DON'T materialize the
        // final shape — the value is dead, so its columns (e.g. a lazy string decode, an expensive map) never
        // fire. The filters in `seg` still run (predShape emits the guarding `If`s); only the trailing map's
        // value isn't forced. DCE/compute-for-survivors extended to the terminal.
        if rest.isEmpty && discardsElem then buildSegment(seg.map(_._1), srcShape(cur))(_ => ctx.consume(unit))
        else if rest.isEmpty && ctx.consumeShape.isDefined then
          // shape-aware terminal (fold/reduce/extremumBy): hand the decomposed shape to the terminal's lambda
          // so it reads only the fields it projects — no whole-product rebuild for `(acc, r) => acc + r.field`.
          buildSegment(seg.map(_._1), srcShape(cur))(finalShape => ctx.consumeShape.get(finalShape))
        else buildSegment(seg.map(_._1), srcShape(cur))(finalShape => readShape(finalShape)(t => buildBody(rest, t, ctx)))
      case (FlatMapS(f, bTpe), _) :: rest =>
        val innerBody = applyLambda(f, cur)
        // FAST PATH: a literal `FArray(e0, e1, …)` flatMap body inline-expands to a Block that allocates a
        // fresh `${K}Arr` (a `new Array` + element stores) PER ELEMENT. The JIT eliminates the `${K}Arr`
        // wrapper but NOT the backing array (a small array read with a variable loop index is not scalar-
        // replaceable on HotSpot) — so each such flatMap leaks one array/elem (measured: 1.68 MB/op, the C2
        // flatMap floor). If the body is exactly that literal form, SPLAT the element exprs straight into the
        // downstream segment — `buildBody(rest, e_k)` per element — with NO array, NO `${K}Arr`, NO inner loop.
        // 0 alloc; measured ~3x faster than the array form on C2 (49k vs 16k ops/s) and beats eager.
        literalArrayElems(innerBody) match
          case Some(elems) =>
            Block(elems.map(e => buildBody(rest, e, ctx)), '{ () }.asTerm)
          case None =>
            // general flatMap: open a nested loop over f(cur); everything downstream runs inside it.
            val inner: Expr[FBase] = '{ ${ innerBody.asExpr }.asInstanceOf[FBase] }
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
        '{
          if $dropping && ${ applyLambda(p, cur).asExprOf[Boolean] } then ()
          else { $stopDropping; ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }
        }.asTerm
      case (DistinctS(keyFn), i) :: rest =>
        // emit only if the element/key is newly seen (HashSet.add returns true on first insertion).
        val seen = ctx.counters(i).seen.get
        val key: Term = keyFn match { case Some(f) => applyLambda(f, cur); case None => cur }
        '{ if ${ seen }.add(${ key.asExpr }) then ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }.asTerm
      case (ScanLeftS(_, op, _), i) :: rest =>
        // update the running accumulator, then emit it downstream. (The initial z is emitted by withScanPrologue.)
        val (accRead, setAcc) = ctx.counters(i).acc.get
        '{
          ${ setAcc(applyN(op, List(accRead, cur))).asExprOf[Unit] }
          ${ letBind(accRead)(a => buildBody(rest, a, ctx)).asExprOf[Unit] }
        }.asTerm
      case (TapEachS(f), _) :: rest =>
        // run the side effect eagerly for every element reaching here, then pass the element through unchanged.
        '{ ${ applyLambda(f, cur).asExprOf[Unit] }; ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }.asTerm
      case (TakeS(_), i) :: rest =>
        val sl = ctx.counters(i); val lim = sl.lim.get; val d = ctx.done.get
        '{
          val cv = ${ sl.read }
          if cv >= ${ lim } then ${ d.set } else { ${ buildBody(rest, cur, ctx).asExprOf[Unit] }; ${ sl.inc }; if cv + 1 >= ${ lim } then ${ d.set } }
        }.asTerm
      case (DropS(_), i) :: rest =>
        val sl = ctx.counters(i)
        '{ if ${ sl.read } < ${ sl.lim.get } then ${ sl.inc } else ${ buildBody(rest, cur, ctx).asExprOf[Unit] } }.asTerm
      case (ZipWithIndexS, i) :: rest =>
        // capture the index, advance it, then hand a DECOMPOSED (value, index) pair downstream — so a filter on
        // the index or a map keeping only the value never builds the tuple. The pair is rebuilt only if read whole.
        val sl = ctx.counters(i)
        letBind(sl.read.asTerm) { pos =>
          '{
            ${ sl.inc }
            ${ continueShape(Tup(List(srcShape(cur), srcShape(pos)), ts => tupleWithIndex(ts(0), ts(1).asExprOf[Int])), rest, ctx).asExprOf[Unit] }
          }.asTerm
        }
      case (ZipS(_, bTpe, combine), i) :: rest =>
        // lock-step: stop (done) when `that` is exhausted; otherwise capture the position, advance, and continue
        // with a LAZY `that(pos)` column — so if the zipped value is never read downstream it is never read at all.
        val sl = ctx.counters(i); val d = ctx.done.get; val zn = sl.lim.get; val (zthat, _) = sl.zipThat.get
        '{
          if ${ sl.read } >= $zn then ${ d.set }
          else {
            val pos: Int = ${ sl.read }
            ${ sl.inc }
            ${
              val lazyB = memoScalar(kk => kk(readAtKind(bTpe, zthat, '{ pos })))
              val continue: Term = combine match
                // map2 forces the value (f uses it); zip hands a Tup([cur, lazyB]) to the downstream segment so
                // projections resolve to typed columns, no pair is allocated, and a discarded side is never read.
                case Some(f) => readShape(lazyB)(b => buildBody(rest, applyN(f, List(cur, b)), ctx))
                case None    => continueShape(Tup(List(srcShape(cur), lazyB), mkTuple), rest, ctx)
              continue.asExprOf[Unit]
            }
          }
        }.asTerm

    // --- traverse one level (source OR a flatMap inner): leaf fast-path + `<kind>At` fallback, `done` break ---
    // The shared INNER element loop over ONE realized chunk `s` (an `FBase` leaf/tree). Both the in-memory path
    // (one chunk = the whole FArray) and the streaming chunk-driver (many chunks) emit this verbatim — only the
    // SOURCE of `s` differs. `cond` already folds in `done`, so a satisfied short-circuit stops mid-chunk.
    def elementLoop(src: Expr[FBase], k: Kind, elemTpe: TypeRepr, ss: List[(Stage, Int)], ctx: Ctx): Expr[Unit] =
      def cond(len: Expr[Int], i: Expr[Int]): Expr[Boolean] = ctx.done match
        case Some(d) => '{ $i < $len && ! ${ d.read } }
        case None    => '{ $i < $len }
      def perElem(read: Term): Expr[Unit] =
        letBind(read)(x => buildBody(ss, x, ctx)).asExprOf[Unit]
      // A `${K}Arr` LEAF fast-path peeled INLINE ahead of the `${kind}At` fallback. Why it matters: a `flatMap`
      // inner builds a fresh small `${K}Arr` PER ELEMENT; reading it back through `${kind}At` (an FBase-typed
      // megamorphic call) makes the freshly-allocated array ESCAPE, so the JIT can't scalar-replace it — on
      // HotSpot C2 (weaker escape analysis than GraalVM) this turns a no-alloc fused pass into a per-element
      // allocation and craters it (measured ~13x slower than eager). Reading `leaf.arr(i)` from a CONCRETE
      // `${K}Arr` is monomorphic and INLINE (no call boundary) so EA proves the array stays local → scalar-
      // replaced, no alloc, on both JVMs. The `${kind}At` arm still serves genuine tree sources (Concat/Slice/…).
      // NOTE (measured, do NOT "simplify" back): routing this through the SHARED `foreachLeaf${K}`/`scFwdLeaf${K}`
      // driver (push each element through a `${K}Consumer`/`${K}Pred` SAM) is 1.7-2.8x SLOWER here — passing the
      // fresh inner array INTO a shared method is itself an escape boundary that defeats scalar replacement on
      // BOTH C2 and GraalVM. The inline leaf-match is the point. Loop bound is `a.length` (the backing array's),
      // NOT leaf.length, so the JIT drops the per-element bounds check.
      elemTpe.asType match
        case '[se] =>
          k match
            case Kind.KInt =>
              '{
                $src match
                  case leaf: IntArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.intAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KLong =>
              '{
                $src match
                  case leaf: LongArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.longAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KDouble =>
              '{
                $src match
                  case leaf: DoubleArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.doubleAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KFloat =>
              '{
                $src match
                  case leaf: FloatArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.floatAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KShort =>
              '{
                $src match
                  case leaf: ShortArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.shortAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KByte =>
              '{
                $src match
                  case leaf: ByteArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.byteAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KChar =>
              '{
                $src match
                  case leaf: CharArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.charAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KBoolean =>
              '{
                $src match
                  case leaf: BooleanArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i) }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.booleanAt(s, i) }.asTerm) }; i += 1 }
              }
            case Kind.KRef =>
              '{
                $src match
                  case leaf: RefArr =>
                    val a = leaf.arr; val len = a.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ a(i).asInstanceOf[se] }.asTerm) }; i += 1 }
                  case s =>
                    val len = s.length; var i = 0
                    while ${ cond('len, 'i) } do { ${ perElem('{ FArrayOps.refAt(s, i).asInstanceOf[se] }.asTerm) }; i += 1 }
              }

    // in-memory: ONE chunk = the whole FArray. Just the shared element loop.
    def loopOver(src: Expr[FBase], k: Kind, elemTpe: TypeRepr, ss: List[(Stage, Int)], ctx: Ctx): Expr[Unit] =
      elementLoop(src, k, elemTpe, ss, ctx)

    // streaming: drive a `Source` — pull a chunk, run the shared element loop over it, repeat. CONSTANT MEMORY:
    // live data = stage state (hoisted above by withDone/declareSlots) + ONE chunk's backing array. `done` is
    // checked at BOTH levels — the inner loop stops mid-chunk on short-circuit, and the outer `&& !done` gate
    // prevents the NEXT pull (so `take(1)` over a file reads one chunk and no more I/O happens).
    def loopOverSource(srcExpr: Expr[Source[?]], k: Kind, elemTpe: TypeRepr, ss: List[(Stage, Int)], ctx: Ctx): Expr[Unit] =
      val notDone: Expr[Boolean] = ctx.done match { case Some(d) => '{ ! ${ d.read } }; case None => '{ true } }
      // CRUCIAL: the next pull is GATED on `!done`. After the element loop sets `done` mid-chunk, we must NOT pull
      // the next chunk (that would be one chunk of wasted I/O, and breaks `take(1)`-reads-one-chunk). So the
      // trailing re-pull only happens while not done; otherwise we fall to End and exit.
      // ALWAYS try/finally close() — resource safety on all three exit paths (exhaustion / short-circuit / throw).
      // `close()` defaults to a no-op, so pure sources pay only the (JIT-elided) try/finally frame.
      '{
        val src = $srcExpr
        try
          var chunk: FArray[Any] | Source.End = src.pullChunk().asInstanceOf[FArray[Any] | Source.End]
          while (chunk ne Source.End) && $notDone do
            ${ elementLoop('{ chunk.asInstanceOf[FArray[Any]].asInstanceOf[FBase] }, k, elemTpe, ss, ctx) }
            chunk = if $notDone then src.pullChunk().asInstanceOf[FArray[Any] | Source.End] else Source.End
        finally src.close()
      }

    // ===== JSON source lowering: a per-record byte scanner whose record is a Tup of byte-sourced columns =====
    // The downstream (filter/map/sink/DCE/terminal) is the UNCHANGED optimizer — it consumes the Tup via
    // continueShape, exactly like zipWithIndex/collect. Only the SOURCE read changes from `a(i)` to a scan-pass.

    // the live top-level fields of T (those any map/filter lambda reads) — the projection∪predicate set, i.e.
    // dead-column elimination computed up front (the "backwards live-set flow"): a field nothing reads gets no
    // slot and is skipValue'd; the scanner only materializes what the pipeline touches.
    def jsonLiveFields(elemTpe: TypeRepr): (List[(String, TypeRepr)], Set[String]) =
      val fields =
        productFields(elemTpe).getOrElse(report.errorAndAbort(s"fuse-json: ${elemTpe.show} is not a case class (flat case-class records only in v1)"))
      val live = scala.collection.mutable.LinkedHashSet.empty[String]
      var wholeUse = false
      def addFromBody(param: Symbol, b: Term): Unit =
        val paths = collectPaths(b, param)
        if paths.contains(Nil) then wholeUse = true
        paths.foreach(path => path.headOption.foreach(h => live += h._2))
      def addFrom(lam: Term): Unit = decomposeLambda(lam) match
        case Some((p, b)) => addFromBody(p, b)
        case None         => report.errorAndAbort(s"fuse-json: could not decompose a stage lambda: ${lam.show}")
      // a terminal that applies a lambda to the ELEMENT must also contribute its read fields — otherwise the
      // scanner treats those fields as dead and feeds the terminal a default (the `acc + 0.0` agg bug).
      // CRUCIAL: only when the lambda's param is still the RECORD type. If a `map` changed the element type
      // (e.g. `…map(_.amount).sum`), the terminal's param is the MAPPED value, not the record — its field reads
      // are over that value, and the record's live fields were already declared by the map stage. Treating the
      // mapped param as a record reader (whole-use → all fields live) is the "interns all 20 keys" bug.
      def isRecordParam(param: Symbol): Boolean = param.termRef.widen =:= elemTpe
      def addTerminalLambdas(): Unit =
        def add1(lam: Term): Unit = decomposeLambda(lam).foreach((p, b) => if isRecordParam(p) then addFromBody(p, b))
        def add2(lam: Term): Unit = decomposeLambda2(lam).foreach((_, e, b) => if isRecordParam(e) then addFromBody(e, b)) // (acc, elem) => …
        tag match
          case TTag.IndexWhere                       => add1(args(0)) // p over the element
          case TTag.Find | TTag.Exists | TTag.Forall => add1(args(0))
          case TTag.GroupReduce                      => add1(args(0)); add1(args(1)) // key + value read record fields (reduce doesn't)
          case TTag.Agg                              => addAggSpecs(parseAggList(args(0)))
          case TTag.Plan if args.nonEmpty            =>
            // planFold passes a 2-param fold op; planAgg passes an agg list. Distinguish by shape.
            decomposeLambda2(args(0)) match
              case Some(_) => add2(args(0)) // planFold(op)
              case None    => addAggSpecs(parseAggList(args(0))) // planAgg(aggs)
          case _ => ()
        def addAggSpecs(specs: List[AggSpec]): Unit = specs.foreach {
          case AggSpec.Sum(f, _)                  => add1(f)
          case AggSpec.Avg(f)                     => add1(f)
          case AggSpec.Foreach(f)                 => add1(f)
          case AggSpec.Extremum(f, _, _, _)       => add1(f)
          case AggSpec.Fold(_, op, _)             => add2(op)
          case AggSpec.TopN(k, _, _, _, _, _)     => add1(k); wholeUse = true // topN RETAINS elements → whole record live
          case AggSpec.Reduce(op, _, _)           => add2(op); wholeUse = true // reduce combines whole elements
          case AggSpec.ExtremumByElem(f, _, _, _) => add1(f); wholeUse = true // returns the whole best element
          case AggSpec.Count                      => ()
        }
      stages.foreach {
        case MapS(f)       => addFrom(f)
        case FilterS(p, _) => addFrom(p)
        case _             => ()
      }
      addTerminalLambdas()
      val liveSet = if wholeUse then fields.map(_._1).toSet else live.toSet
      (fields, liveSet)

    // expose `wholeUse` (whether the record is materialized) separately for the plan description.
    def jsonWholeUse(elemTpe: TypeRepr): Boolean =
      productFields(elemTpe).getOrElse(Nil)
      var whole = false
      def chk(param: Symbol, b: Term): Unit = if collectPaths(b, param).contains(Nil) then whole = true
      def add1(lam: Term): Unit = decomposeLambda(lam).foreach((p, b) => if p.termRef.widen =:= elemTpe then chk(p, b))
      def add2(lam: Term): Unit = decomposeLambda2(lam).foreach((_, e, b) => if e.termRef.widen =:= elemTpe then chk(e, b))
      stages.foreach {
        case MapS(f) => decomposeLambda(f).foreach((p, b) => chk(p, b)); case FilterS(p, _) => decomposeLambda(p).foreach((pr, b) => chk(pr, b)); case _ => ()
      }
      tag match
        case TTag.IndexWhere | TTag.Find | TTag.Exists | TTag.Forall => add1(args(0))
        case TTag.GroupReduce                                        => add1(args(0)); add1(args(1))
        case TTag.Agg                                                => aggSpecs(parseAggList(args(0)))
        case TTag.Plan if args.nonEmpty => decomposeLambda2(args(0)) match { case Some(_) => add2(args(0)); case None => aggSpecs(parseAggList(args(0))) }
        case _                          => ()
      def aggSpecs(specs: List[AggSpec]): Unit = specs.foreach {
        case AggSpec.Sum(f, _) => add1(f); case AggSpec.Avg(f) => add1(f); case AggSpec.Foreach(f) => add1(f); case AggSpec.Extremum(f, _, _, _) => add1(f);
        case AggSpec.Fold(_, op, _)   => add2(op); case AggSpec.TopN(k, _, _, _, _, _)                   => add1(k); whole = true;
        case AggSpec.Reduce(op, _, _) => add2(op); whole = true; case AggSpec.ExtremumByElem(f, _, _, _) => add1(f); whole = true; case AggSpec.Count => ()
      }
      whole

    // the scanner kind for a field type — what the slot holds and how the column reads it.
    enum JKind { case JInt, JLong, JDouble, JString }
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

    // A machine-checkable DESCRIPTION of the plan the macro built for a JSON pipeline — so tests assert on the
    // STRUCTURE (which fields are scanned, decoded vs sliced, the predicate/early-out set, rebuild, terminal)
    // rather than on brittle pretty-printed code or on output values that can be coincidentally right.
    def jsonPlanString(elemTpe: TypeRepr): String =
      val (fields, liveSet) = jsonLiveFields(elemTpe)
      val whole = jsonWholeUse(elemTpe)
      // a String live field is kept as a lazy (start,len) slice; a numeric live field is decoded to a value.
      val live = fields.filter(f => liveSet.contains(f._1))
      val decoded = live.collect { case (n, t) if jkindOf(t) != JKind.JString => n }
      val sliced = live.collect { case (n, t) if jkindOf(t) == JKind.JString => n }
      // predicate fields = fields read by the LEADING positive filters (the early-out set).
      val leadingFilters = stages.takeWhile { case FilterS(_, false) => true; case _ => false }.collect { case FilterS(p, false) => p }
      val predNames =
        leadingFilters.flatMap(p => decomposeLambda(p).toList.flatMap((param, b) => collectPaths(b, param).flatMap(_.headOption.map(_._2)))).distinct
      val projOnly = live.map(_._1).exists(n => !predNames.contains(n))
      val earlyOut = leadingFilters.nonEmpty && predNames.nonEmpty && projOnly
      val termName =
        if tag == TTag.Plan && args.nonEmpty then (if decomposeLambda2(args(0)).isDefined then "Fold" else "Agg")
        else tag.toString // planFold reports Fold, planAgg reports Agg
      // deterministic, sorted, compact:
      def s(xs: Iterable[String]) = xs.toList.sorted.mkString("[", ",", "]")
      s"JsonPlan(record=${elemTpe.typeSymbol.name}, terminal=$termName, " +
        s"live=${s(liveSet)}, decoded=${s(decoded)}, sliced=${s(sliced)}, " +
        s"predicate=${s(predNames)}, earlyOut=$earlyOut, rebuildsRecord=$whole)"

    if tag == TTag.Plan then
      return (if isJson then Expr(jsonPlanString(srcElem)) else Expr(s"InMemoryPlan(elem=${srcElem.typeSymbol.name}, terminal=$tag, stages=${stages.length})"))

    def loopOverJson(jsrc: Expr[NdjsonSource[?]], elemTpe: TypeRepr, ss: List[(Stage, Int)], ctx: Ctx): Expr[Unit] =
      val (fields, liveSet) = jsonLiveFields(elemTpe)
      val liveFields = fields.zipWithIndex.filter { case ((n, _), _) => liveSet.contains(n) }
      // per live field: the slot vars (mutable) + how a column reads it. We declare slots inside the per-record
      // block and reference them from the Tup columns, so they dominate the reads (hygiene-safe).
      // We build the whole per-record body via one continuation so symbols stay in scope.
      val doneCond: Expr[Boolean] = ctx.done match { case Some(d) => d.read; case None => '{ false } }
      // bind each live field's wanted-key bytes to a val ABOVE the loop (interned once), so the per-record scan
      // references them with zero allocation. Thread the refs to jsonRecord.
      def withKeyRefs(rem: List[((String, TypeRepr), Int)], acc: Map[String, Expr[Array[Byte]]])(k: Map[String, Expr[Array[Byte]]] => Expr[Unit]): Expr[Unit] =
        rem match
          case Nil                    => k(acc)
          case ((name, _), _) :: rest =>
            '{
              val keyBytes: Array[Byte] = JsonScanner.internKey(${ Expr(name) })
              ${ withKeyRefs(rest, acc + (name -> '{ keyBytes }))(k) }
            }
      '{
        val src = $jsrc
        val buf: Array[Byte] = src.buf
        val until: Int = src.until
        ${
          withKeyRefs(liveFields, Map.empty) { keyRefs =>
            '{
              var lineStart: Int = src.from
              while lineStart < until && ! $doneCond do
                var lineEnd = lineStart
                while lineEnd < until && buf(lineEnd) != '\n' do lineEnd += 1
                ${ jsonRecord('buf, 'lineStart, 'lineEnd, fields, liveFields, keyRefs, ss, ctx).asExprOf[Unit] }
                lineStart = lineEnd + 1
            }
          }
        }
      }

    // a live field's mutable scan slots: the value column (reads the slot lazily), a `seen` flag ref, and a
    // `fill(buf, valuePos, end): newPos` quote that the scan-pass runs when this field's key matches.
    // `rawRead` is the direct value term (the `v` var, or `decodeLatin1(...)` for a String) used to evaluate a
    // predicate inline — it does NOT go through `letBind` (which is Unit-only), unlike the lazy `col`.
    final case class JSlot(idx: Int, name: String, jk: JKind, col: Shape, seen: Expr[Boolean], fill: (Expr[Int], Expr[Int]) => Expr[Int], rawRead: Term)

    // one record: declare slots (nested quotes, like declareSlots), run the scan-pass, build the Tup, continue.
    def jsonRecord(
        buf: Expr[Array[Byte]],
        lineStart: Expr[Int],
        lineEnd: Expr[Int],
        fields: List[(String, TypeRepr)],
        liveFields: List[((String, TypeRepr), Int)],
        keyRefs: Map[String, Expr[Array[Byte]]],
        ss: List[(Stage, Int)],
        ctx: Ctx
    ): Term =
      def withSlots(rem: List[((String, TypeRepr), Int)], acc: List[JSlot])(k: List[JSlot] => Term): Term =
        rem match
          case Nil                        => k(acc.reverse)
          case ((name, tpe), idx) :: rest =>
            jkindOf(tpe) match
              // numeric fills decode the value into the slot var + the new position into `pNext` (a second slot
              // var) — NO tuple returned (a `(Double,Int)` return boxed, costing the macro path its alloc edge).
              case JKind.JInt =>
                '{
                  var v: Int = 0; var pNext: Int = 0; var seen: Boolean = false
                  ${
                    val s = JSlot(
                      idx,
                      name,
                      JKind.JInt,
                      Sc(kk => kk('{ v }.asTerm)),
                      '{ seen },
                      (p, end) => '{ v = JsonScanner.readIntAt($buf, $p, $end); pNext = JsonScanner.numEnd; seen = true; pNext },
                      '{ v }.asTerm
                    )
                    withSlots(rest, s :: acc)(k).asExpr
                  }
                }.asTerm
              case JKind.JLong =>
                '{
                  var v: Long = 0L; var pNext: Int = 0; var seen: Boolean = false
                  ${
                    val s = JSlot(
                      idx,
                      name,
                      JKind.JLong,
                      Sc(kk => kk('{ v }.asTerm)),
                      '{ seen },
                      (p, end) => '{ v = JsonScanner.readLongAt($buf, $p, $end); pNext = JsonScanner.numEnd; seen = true; pNext },
                      '{ v }.asTerm
                    )
                    withSlots(rest, s :: acc)(k).asExpr
                  }
                }.asTerm
              case JKind.JDouble =>
                '{
                  var v: Double = 0.0; var pNext: Int = 0; var seen: Boolean = false
                  ${
                    val s = JSlot(
                      idx,
                      name,
                      JKind.JDouble,
                      Sc(kk => kk('{ v }.asTerm)),
                      '{ seen },
                      (p, end) => '{ v = JsonScanner.readDoubleAt($buf, $p, $end); pNext = JsonScanner.numEnd; seen = true; pNext },
                      '{ v }.asTerm
                    )
                    withSlots(rest, s :: acc)(k).asExpr
                  }
                }.asTerm
              case JKind.JString =>
                // lazy slice: slots are (start, len); the column decodes to String ON READ (memoized), so the
                // existing sink defers the `new String` past the filter — compute-for-survivors, for free.
                '{
                  var st: Int = 0; var ln: Int = 0; var seen: Boolean = false
                  ${
                    val col = memoScalar(kk => kk('{ JsonScanner.decodeLatin1($buf, st, ln) }.asTerm))
                    val s = JSlot(
                      idx,
                      name,
                      JKind.JString,
                      col,
                      '{ seen },
                      (p, end) => '{ val vs = $p + 1; val ve = JsonScanner.scanStringEnd($buf, vs, $end); st = vs; ln = ve - vs; seen = true; ve + 1 },
                      '{ JsonScanner.decodeLatin1($buf, st, ln) }.asTerm
                    )
                    withSlots(rest, s :: acc)(k).asExpr
                  }
                }.asTerm

      withSlots(liveFields, Nil) { slots =>
        val byIdx = slots.map(s => s.idx -> s).toMap
        // build the Tup: parts indexed like ALL fields (dead fields get a placeholder column that's never read).
        def colFor(i: Int, ftpe: TypeRepr): Shape =
          // a dead field's column yields a typed default — only ever forced if the WHOLE record is materialized
          // (rebuild), which the live-set marks all-fields-live for, so in practice this is never read; it exists
          // so a value-ignoring terminal (e.g. count) that still rebuilds the Tup typechecks.
          byIdx.get(i).map(_.col).getOrElse(Sc(k => k(defaultOf(ftpe))))
        val parts = fields.zipWithIndex.map { case ((_, ftpe), i) => colFor(i, ftpe) }
        val recordTup: Shape = Tup(parts, vs => mkProduct(srcElem, vs))

        // ---- PREDICATE-FAIL EARLY-OUT ----
        // The LEADING run of filters (filters before any map) can be evaluated DURING the scan, the moment all
        // their fields are captured. If a filter fails, abandon the record immediately — never scanning the
        // projection-only fields that come later. This is the strong early-out (vs `allSeen`, which waits for
        // ALL live fields). We keep the filters downstream too (a cheap re-check on survivors over already-
        // decoded values), so correctness doesn't depend on stripping them.
        // only POSITIVE filters (not `filterNot`) are taken for the inline early-out — negation would need a
        // wrapped lambda; `filterNot` falls back to downstream-only filtering (still correct, just no early-out).
        val leadingFilters: List[Term] = ss
          .takeWhile {
            case (FilterS(_, false), _) => true
            case _                      => false
          }
          .collect { case (FilterS(p, false), _) => p }
        // the fields those leading filters read (their slots must be seen before we can evaluate them).
        val predFieldIdxs: Set[Int] =
          leadingFilters.flatMap(p => decomposeLambda(p).toList.flatMap((param, b) => collectPaths(b, param).flatMap(_.headOption.map(_._1)))).toSet
        val predSlots = slots.filter(s => predFieldIdxs.contains(s.idx))
        // a predicate-fail early-out is only worthwhile when there ARE projection-only fields to skip AND the
        // predicate's fields are a proper subset of live (otherwise `allSeen` already covers it).
        val projOnly = slots.exists(s => !predFieldIdxs.contains(s.idx))
        val earlyPred: Option[(Expr[Boolean], Expr[Boolean])] =
          if leadingFilters.isEmpty || predSlots.isEmpty || !projOnly then None
          else
            // allPredSeen = conjunction of the predicate slots' seen flags
            val allPredSeen = predSlots.map(_.seen).reduce((a, b) => '{ $a && $b })
            // predHolds = conjunction of each leading filter evaluated inline. Each predicate's field accesses
            // (`_.amount`) are substituted DIRECTLY with the slot's read term (the decoded `v` var / a decoded
            // String) — no `letBind` (which is Unit-only, for loop bodies); the slots are already bound, so a
            // field read is just the var. We only handle predicates whose every path resolves to a known slot.
            val byField: Map[Int, JSlot] = byIdx
            def predBool(p: Term): Option[Expr[Boolean]] = decomposeLambda(p) match
              case Some((param, body)) =>
                val paths = collectPaths(body, param)
                // each path is a single-hop field read into a slot; map it to the slot's raw value term (the
                // decoded `v` var / `decodeLatin1(...)` — NOT through `letBind`).
                val refs: Map[Path, Term] = paths.flatMap { path =>
                  path.headOption.flatMap { case (i, _) => byField.get(i).map(s => path -> s.rawRead) }
                }.toMap
                if refs.size == paths.size then Some(substPaths(body, param, refs).asExprOf[Boolean])
                else None // a path didn't resolve to a slot → skip inline check (downstream handles it)
              case None => None
            val predBools = leadingFilters.map(predBool)
            val predHolds: Option[Expr[Boolean]] =
              if predBools.contains(None) then None
              else Some(predBools.flatten.reduce((a, b) => '{ $a && $b }))
            predHolds.map(ph => (allPredSeen, ph)) // None if any predicate couldn't be lowered inline

        if earlyPred.isEmpty then
          // no inline predicate early-out — just scan (with allSeen) and run downstream.
          '{
            ${ jsonScanPass(buf, lineStart, lineEnd, slots, keyRefs, None, '{ () }) }
            ${ continueShape(recordTup, ss, ctx).asExprOf[Unit] }
          }.asTerm
        else
          '{
            var rejected: Boolean = false
            ${ jsonScanPass(buf, lineStart, lineEnd, slots, keyRefs, earlyPred, '{ rejected = true }) }
            if !rejected then ${ continueShape(recordTup, ss, ctx).asExprOf[Unit] }
          }.asTerm
      }

    // emit the one-pass key-dispatch scan over a record's fields (compact NDJSON fast path).
    // earlyPred = Some((allPredSeen, predHolds)): evaluate the leading filters the moment their fields are all
    //   captured; if they fail, run `reject` and stop the scan (predicate-fail early-out — skips projection fields).
    def jsonScanPass(
        buf: Expr[Array[Byte]],
        lineStart: Expr[Int],
        lineEnd: Expr[Int],
        slots: List[JSlot],
        keyRefs: Map[String, Expr[Array[Byte]]],
        earlyPred: Option[(Expr[Boolean], Expr[Boolean])],
        reject: Expr[Unit]
    ): Expr[Unit] =
      // generate the per-key dispatch chain: if keyEquals(name0) fill0 else if … else skipValue.
      def dispatch(ks: Expr[Int], ke: Expr[Int], p: Expr[Int]): Expr[Int] =
        slots.foldRight[Expr[Int]]('{ JsonScanner.skipValue($buf, $p, $lineEnd) }) { (s, elseB) =>
          '{ if ! ${ s.seen } && JsonScanner.keyEquals($buf, $ks, $ke, ${ keyRefs(s.name) }) then ${ s.fill(p, lineEnd) } else $elseB }
        }
      // projection EARLY-OUT: once every live field is captured, abandon the rest (nothing else is read).
      def allSeen: Expr[Boolean] =
        slots match
          case Nil           => '{ false } // no live fields → never early-out (scan to '}' normally)
          case first :: more => more.foldLeft(first.seen)((acc, s) => '{ $acc && ${ s.seen } })
      // the per-key step, with the predicate-fail check (if any) folded in: after capturing a field, if all
      // predicate fields are now seen, evaluate the leading filters; on failure reject + stop.
      earlyPred match
        case None =>
          '{
            var p: Int = $lineStart + 1
            var open: Boolean = $buf($lineStart) == '{'
            while open do
              if $buf(p) == '}' then open = false
              else
                val ks = p + 1
                val ke = JsonScanner.scanStringEnd($buf, ks, $lineEnd)
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
                val ks = p + 1
                val ke = JsonScanner.scanStringEnd($buf, ks, $lineEnd)
                p = ${ dispatch('ks, 'ke, '{ ke + 2 }) }
                if !predChecked && $allPredSeen then
                  predChecked = true
                  if ! $predHolds then { $reject; open = false } // PREDICATE FAILED → abandon the record now
                if open then
                  if $allSeen then open = false
                  else if $buf(p) == ',' then p += 1
                  else open = false
          }

    // --- declare loop-state vars (above the loop), then assemble the terminal body ---
    def withDone(k: Option[Done] => Term): Term =
      if needsDone then '{ var done: Boolean = false; ${ k(Some(Done('{ done = true }, '{ done }))).asExpr } }.asTerm
      else k(None)

    def declareSlots(rem: List[(Int, CSpec)], acc: Map[Int, Slot])(k: Map[Int, Slot] => Term): Term =
      rem match
        case Nil                          => k(acc)
        case (idx, LimSpec(argT)) :: rest => // take/drop: a clamped limit val + a counter
          '{
            val lim: Int = { val t = ${ argT.asExprOf[Int] }; if t < 0 then 0 else t }
            var c: Int = 0
            ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, Some('{ lim }))))(k).asExpr }
          }.asTerm
        case (idx, IdxSpec) :: rest => // zipWithIndex: a counter only
          '{
            var c: Int = 0
            ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, None)))(k).asExpr }
          }.asTerm
        case (idx, ZipSpec(thatT, bTpe)) :: rest => // zip/map2: bind `that` + its length + a counter
          '{
            val zthat: FBase = ${ thatT.asExpr }.asInstanceOf[FBase]; val zn: Int = zthat.length
            var c: Int = 0
            ${ declareSlots(rest, acc + (idx -> Slot('{ c }, '{ c += 1 }, Some('{ zn }), Some(('{ zthat }, bTpe)))))(k).asExpr }
          }.asTerm
        case (idx, DropWhileSpec) :: rest => // dropWhile: a Boolean flag, read + cleared in the loop
          '{
            var dropping: Boolean = true
            ${ declareSlots(rest, acc + (idx -> Slot('{ 0 }, '{ () }, None, flag = Some(('{ dropping }, '{ dropping = false })))))(k).asExpr }
          }.asTerm
        case (idx, DistinctSpec) :: rest => // distinct/distinctBy: a dedup set keyed by element/key value
          '{
            val seen = new scala.collection.mutable.HashSet[Any]()
            ${ declareSlots(rest, acc + (idx -> Slot('{ 0 }, '{ () }, None, seen = Some('{ seen }))))(k).asExpr }
          }.asTerm
        case (idx, ScanSpec(z, zt)) :: rest => // scanLeft: a mutable accumulator initialized to z
          zt.asType match
            case '[zz] =>
              '{
                var acc0: zz = ${ z.asExprOf[zz] }
                ${
                  val slot = Slot('{ 0 }, '{ () }, None, acc = Some(('{ acc0 }.asTerm, (v: Term) => '{ acc0 = ${ v.asExprOf[zz] } }.asTerm)))
                  declareSlots(rest, acc + (idx -> slot))(k).asExpr
                }
              }.asTerm

    /** static upper bound on output length = min(source length, every `take` limit and zip `that` length). */
    def capExpr(n0: Expr[Int], counters: Map[Int, Slot]): Expr[Int] =
      indexed
        .collect { case (TakeS(_), i) => counters(i).lim.get; case (ZipS(_, _, _), i) => counters(i).lim.get }
        .foldLeft(n0)((a, l) => '{ java.lang.Math.min($a, $l) })

    // scanLeft: emit the initial `z` through the downstream ONCE before the loop (so the result has its leading
    // element even for empty input). Both `loop` callers below go through this, so it works for every terminal.
    def withScanPrologue(c: Ctx, body: Expr[Unit]): Expr[Unit] = scanInfo match
      case Some((i, post)) => '{ ${ buildBody(post, c.counters(i).acc.get._1, c).asExprOf[Unit] }; $body }
      case None            => body

    def assemble(src0: Expr[FBase], n0: Expr[Int], done: Option[Done], counters: Map[Int, Slot]): Term =
      def ctx(consume: Term => Term) = Ctx(consume, done, counters)
      def drive(c: Ctx): Expr[Unit] =
        (jsonSrc, streamSrc) match
          case (Some(js), _) => loopOverJson(js, srcElem, indexed, c)
          case (_, Some(ss)) => withScanPrologue(c, loopOverSource(ss, srcK, srcElem, indexed, c))
          case _             => withScanPrologue(c, loopOver(src0, srcK, srcElem, indexed, c))
      def loop(consume: Term => Term): Expr[Unit] = drive(ctx(consume))
      // shape-aware driver: the terminal consumes the element's decomposed Shape (so `op`/`f` projects columns
      // without rebuilding the product). `consume` is a materialize-then-apply fallback for non-segment paths.
      def loopS(consume: Term => Term)(consumeShape: Shape => Term): Expr[Unit] =
        drive(Ctx(consume, done, counters, Some(consumeShape)))
      tag match
        case TTag.Find =>
          // build with Option[Any] (no `A` inside the deep nested quote — keeps Type[A] evidence from leaking);
          // the entry method casts back to Option[A].
          val p = args(0); val d = done.get
          '{
            var res: Option[Any] = None
            ${ loop(v => '{ if ${ applyLambda(p, v).asExprOf[Boolean] } then { res = Some(${ v.asExpr }); ${ d.set } } }.asTerm) }
            res
          }.asTerm
        case TTag.Exists =>
          // shape-aware: decompose `p`'s element param → reads only the predicate's fields, no rebuild.
          val p = args(0); val d = done.get
          '{
            var res = false
            ${
              loopS(v => '{ if ${ applyLambda(p, v).asExprOf[Boolean] } then { res = true; ${ d.set } } }.asTerm)(sh =>
                fnOnShape(p, sh)(b => '{ if ${ b.asExprOf[Boolean] } then { res = true; ${ d.set } } }.asTerm)
              )
            }
            res
          }.asTerm
        case TTag.Forall =>
          val p = args(0); val d = done.get
          '{
            var res = true
            ${
              loopS(v => '{ if ! ${ applyLambda(p, v).asExprOf[Boolean] } then { res = false; ${ d.set } } }.asTerm)(sh =>
                fnOnShape(p, sh)(b => '{ if ! ${ b.asExprOf[Boolean] } then { res = false; ${ d.set } } }.asTerm)
              )
            }
            res
          }.asTerm
        case TTag.HeadOption =>
          val d = done.get
          '{
            var res: Option[Any] = None
            ${ loop(v => '{ res = Some(${ v.asExpr }); ${ d.set } }.asTerm) }
            res
          }.asTerm
        case TTag.Head =>
          val d = done.get
          '{
            var res: Option[Any] = None
            ${ loop(v => '{ res = Some(${ v.asExpr }); ${ d.set } }.asTerm) }
            res.getOrElse(throw new java.util.NoSuchElementException("head of empty fused pipeline"))
          }.asTerm
        case TTag.IndexWhere =>
          // bare `var idx` + position counter — no (value, index) pair is built; stop at the first match.
          // shape-aware: decompose `p`'s element param → reads only the predicate's fields, no product rebuild.
          val p = args(0); val d = done.get
          '{
            var idx: Int = -1; var c: Int = 0
            ${
              loopS(v => '{ if ${ applyLambda(p, v).asExprOf[Boolean] } then { idx = c; ${ d.set } }; c = c + 1 }.asTerm)(sh =>
                fnOnShape(p, sh)(b => '{ if ${ b.asExprOf[Boolean] } then { idx = c; ${ d.set } }; c = c + 1 }.asTerm)
              )
            }
            idx
          }.asTerm
        case TTag.Agg =>
          // one accumulator (or two) per aggregate, all carried in ONE loop. Each aggregate's element lambda is
          // decomposed against the SHARED element shape (via opOnShape/fnOnShape), so the aggregates' read
          // columns merge automatically: the union is computed, a shared field once (CSE), survivor-gated.
          val specs = parseAggList(args(0))
          // per spec: a state carrying the per-element step (CPS: `(shape, next) => term`, so each step's column
          // bindings stay in scope for the following steps → a column read by several aggregates binds ONCE) and
          // the finish term. `next` is the continuation = the remaining steps + the loop tail.
          // `step` is CPS with the continuation passed as a THUNK that is forced INSIDE this step's column-read
          // scope — so the NEXT step's column reads see this step's memoized bindings (a field read by several
          // aggregates binds ONCE, in the outermost reader's scope, and stays in scope for the rest).
          final case class AState(step: (Shape, () => Term) => Term, finish: Term)
          def stateFor(spec: AggSpec, k: AState => Term): Term = spec match
            case AggSpec.Count =>
              '{
                var c: Int = 0
                ${ k(AState((_, next) => '{ c += 1; ${ next().asExprOf[Unit] } }.asTerm, '{ c }.asTerm)).asExpr }
              }.asTerm
            case AggSpec.Foreach(f) =>
              // run f for its side effect; shape-aware so f reads only the fields it touches (no product rebuild).
              k(AState((sh, next) => fnOnShape(f, sh)(fv => '{ ${ fv.asExprOf[Unit] }; ${ next().asExprOf[Unit] } }.asTerm), '{ () }.asTerm))
            case AggSpec.Sum(f, bTpe) =>
              // UNBOXED for primitive field kinds: emit `acc + v` (iadd/ladd/dadd/fadd) directly, never the
              // generic Numeric.plus (which boxes both operands per element). Fall back to Numeric for other types.
              bTpe.asType match
                case '[bb] =>
                  '{
                    var acc: bb = ${ numZero(bTpe).asExprOf[bb] }
                    ${
                      k(
                        AState(
                          (sh, next) =>
                            fnOnShape(f, sh)(fv => '{ acc = ${ numAdd(bTpe, '{ acc }.asTerm, fv).asExprOf[bb] }; ${ next().asExprOf[Unit] } }.asTerm),
                          '{ acc }.asTerm
                        )
                      ).asExpr
                    }
                  }.asTerm
            case AggSpec.Avg(f) =>
              '{
                var s: Double = 0.0; var c: Int = 0
                ${
                  k(
                    AState(
                      (sh, next) => fnOnShape(f, sh)(fv => '{ s = s + ${ fv.asExprOf[Double] }; c += 1; ${ next().asExprOf[Unit] } }.asTerm),
                      '{ if c == 0 then 0.0 else s / c }.asTerm
                    )
                  ).asExpr
                }
              }.asTerm
            case AggSpec.Extremum(f, bTpe, isMax, bare) =>
              // UNBOXED for primitive field kinds: emit `kk > best` / `kk < best` directly (if_icmp/dcmp), never
              // the generic Ordering.gt/lt (which boxes). Fall back to Ordering for other types. `bare` (min1/max1)
              // returns B directly (throws on empty); otherwise Option[B].
              bTpe.asType match
                case '[bb] =>
                  '{
                    var best: bb = null.asInstanceOf[bb]; var seen: Boolean = false
                    ${
                      k(
                        AState(
                          (sh, next) =>
                            fnOnShape(f, sh)(fv =>
                              '{
                                val kk: bb = ${ fv.asExprOf[bb] }
                                if !seen || ${ ordWins(bTpe, '{ kk }.asTerm, '{ best }.asTerm, isMax) } then { best = kk; seen = true }
                                ${ next().asExprOf[Unit] }
                              }.asTerm
                            ),
                          if bare then '{ if seen then best else throw new java.util.NoSuchElementException("min1/max1 of empty fused pipeline") }.asTerm
                          else '{ if seen then Some(best) else None }.asTerm
                        )
                      ).asExpr
                    }
                  }.asTerm
            case AggSpec.Fold(z, op, sTpe) =>
              sTpe.asType match
                case '[ss] =>
                  '{
                    var acc: ss = ${ z.asExprOf[ss] }
                    ${
                      k(
                        AState(
                          (sh, next) => opOnShape(op, '{ acc }.asTerm, sh)(r => '{ acc = ${ r.asExprOf[ss] }; ${ next().asExprOf[Unit] } }.asTerm),
                          '{ acc }.asTerm
                        )
                      ).asExpr
                    }
                  }.asTerm
            case AggSpec.TopN(keyf, aTpe, bTpe, nT, ordT, largest) =>
              // bounded size-n heap, best-first. The heap evicts the WORST first. For a PRIMITIVE key (the common
              // `topNBy(_.numericField)`) we use an UNBOXED sibling heap (Int/Long/Double key array + direct </>
              // comparison via a `largest` flag) — NO per-element key boxing. A reference key falls back to the
              // generic Comparator heap (worst==minimum: the given ordering for `largest`, reversed for smallest).
              // `mkState` is reused per heap kind: heap built above the loop, `offer(elem, key)` per element, drain
              // to a best-first FArray at the end. The element is boxed only on ADMISSION (it's retained); a
              // primitive key stays unboxed through the comparison.
              val n = nT.asExprOf[Int]; val lg = Expr(largest)
              aTpe.asType match
                case '[aa] =>
                  def mkState[H](mkHeap: Expr[H], offer: (Expr[H], Term, Term) => Expr[Unit], drain: Expr[H] => Expr[Array[Object]])(using Type[H]): Term =
                    '{
                      val heap: H = $mkHeap
                      ${
                        k(
                          AState(
                            (sh, next) =>
                              readShape(sh)(elem => fnOnShape(keyf, sh)(key => '{ ${ offer('{ heap }, elem, key) }; ${ next().asExprOf[Unit] } }.asTerm)),
                            '{ FArray.fromBoxedArray[aa](${ drain('{ heap }) }) }.asTerm
                          )
                        ).asExpr
                      }
                    }.asTerm
                  if bTpe =:= TypeRepr.of[Int] then
                    mkState[IntTopNHeap](
                      '{ new IntTopNHeap($n, $lg) },
                      (h, e, key) => '{ $h.offer(${ e.asExpr }.asInstanceOf[Object], ${ key.asExprOf[Int] }) },
                      h => '{ $h.toSortedArray }
                    )
                  else if bTpe =:= TypeRepr.of[Long] then
                    mkState[LongTopNHeap](
                      '{ new LongTopNHeap($n, $lg) },
                      (h, e, key) => '{ $h.offer(${ e.asExpr }.asInstanceOf[Object], ${ key.asExprOf[Long] }) },
                      h => '{ $h.toSortedArray }
                    )
                  else if bTpe =:= TypeRepr.of[Double] then
                    mkState[DoubleTopNHeap](
                      '{ new DoubleTopNHeap($n, $lg) },
                      (h, e, key) => '{ $h.offer(${ e.asExpr }.asInstanceOf[Object], ${ key.asExprOf[Double] }) },
                      h => '{ $h.toSortedArray }
                    )
                  else
                    bTpe.asType match
                      case '[bb] =>
                        val ord = ordT.asExprOf[Ordering[bb]]
                        val cmp = if largest then '{ $ord } else '{ $ord.reverse }
                        mkState[TopNHeap](
                          '{ new TopNHeap($n, $cmp.asInstanceOf[java.util.Comparator[Any]]) },
                          (h, e, key) => '{ $h.offer(${ e.asExpr }.asInstanceOf[Object], ${ key.asExpr }.asInstanceOf[Object]) },
                          h => '{ $h.toSortedArray }
                        )
            case AggSpec.Reduce(op, bTpe, bare) =>
              // seeded reduce op(acc, a): the first element seeds acc (no zero); thereafter acc = op(acc, a). acc is
              // typed B (the op's result type; B >: A so the element seeds it). reduce combines whole elements, so we
              // read the element shape whole (wholeUse already set for JSON). `bare` (reduce1) returns B, else Option[B].
              bTpe.asType match
                case '[bb] =>
                  '{
                    var seeded = false; var acc: bb = null.asInstanceOf[bb]
                    ${
                      k(
                        AState(
                          (sh, next) =>
                            readShape(sh)(elem =>
                              '{
                                if !seeded then { acc = ${ elem.asExprOf[bb] }; seeded = true }
                                else acc = ${ applyN(op, List('{ acc }.asTerm, elem)).asExprOf[bb] }
                                ${ next().asExprOf[Unit] }
                              }.asTerm
                            ),
                          (if bare then '{ if seeded then acc else throw new java.util.NoSuchElementException("reduce1 of empty fused pipeline") }
                           else '{ if seeded then Some(acc) else None }).asTerm
                        )
                      ).asExpr
                    }
                  }.asTerm
            case AggSpec.ExtremumByElem(f, bTpe, better, bare) =>
              // best ELEMENT by key f(a): `f` once per element (shape-aware → reads only its fields), best element +
              // its key in two vars; `better(newKey, bestKey)` decides. Returns the element (Option[A] or bare A).
              bTpe.asType match
                case '[bb] =>
                  '{
                    var seeded = false; var best: Any = null; var bestK: bb = null.asInstanceOf[bb]
                    ${
                      k(
                        AState(
                          (sh, next) =>
                            readShape(sh)(elem =>
                              fnOnShape(f, sh)(fv =>
                                '{
                                  val kk: bb = ${ fv.asExprOf[bb] }
                                  if !seeded || ${ applyN(better, List('{ kk }.asTerm, '{ bestK }.asTerm)).asExprOf[Boolean] } then {
                                    best = ${ elem.asExpr }; bestK = kk; seeded = true
                                  }
                                  ${ next().asExprOf[Unit] }
                                }.asTerm
                              )
                            ),
                          (if bare then '{ if seeded then best else throw new java.util.NoSuchElementException("minBy1/maxBy1 of empty fused pipeline") }
                           else '{ if seeded then Some(best) else None }).asTerm
                        )
                      ).asExpr
                    }
                  }.asTerm
          // declare all states (nested vars), then run all steps per element in ONE shared scope, then tuple up.
          def withStates(rem: List[AggSpec], acc: List[AState])(k: List[AState] => Term): Term = rem match
            case Nil          => k(acc.reverse)
            case spec :: rest => stateFor(spec, st => withStates(rest, st :: acc)(k))
          withStates(specs, Nil) { states =>
            // chain head-first: step1's body forces `() => step2(...)` from WITHIN step1's column-read scope, so
            // memoized columns bind in the right (outermost) scope and the rest see them.
            def chain(ss: List[AState], sh: Shape): Term = ss match
              case Nil       => unit
              case s :: rest => s.step(sh, () => chain(rest, sh))
            val perElemShape: Shape => Term = sh => chain(states, sh)
            val perElemTerm: Term => Term = v => perElemShape(Sc(kk => kk(v))) // non-segment fallback
            // result: apply the user's `make` (aggTo) to the finished accumulators, else tuple them (agg).
            val result: Term = args.lift(1) match
              case Some(make) => applyN(make, states.map(_.finish))
              case None       => mkTupleN(states.map(_.finish))
            '{ ${ loopS(perElemTerm)(perElemShape) }; ${ resultExpr(result) } }.asTerm
          }
        case TTag.GroupReduce =>
          // group by key(a), combine value(a) per key with reduce → Map[K,B]. ONE fused pass into an
          // open-addressing map (UNBOXED Int key via IntKeyMap; other key kinds via a boxed HashMap fallback).
          // key/value lambdas are decomposed against the element Shape (unboxed, projection-aware over JSON too);
          // `reduce` is inlined (applyN) at the merge — no SAM stored, no per-element closure.
          val key = args(0); val value = args(1); val reduce = args(2)
          def resultTpe(lam: Term, n: Int): TypeRepr = lam.tpe.widen.dealias match
            case AppliedType(_, ts) if ts.length > n => ts(n)
            case _                                   => TypeRepr.of[Any]
          val kTpe = resultTpe(key, 0) // A => K   → last targ is K
          val bTpe = resultTpe(value, 0) // A => B   → last targ is B (both 1-arg: targ(1) is the result)
          val kTpeR = key.tpe.widen.dealias match { case AppliedType(_, ts) => ts.last; case _ => TypeRepr.of[Any] }
          val bTpeR = value.tpe.widen.dealias match { case AppliedType(_, ts) => ts.last; case _ => TypeRepr.of[Any] }
          (kTpeR.asType, bTpeR.asType) match
            case ('[kk], '[bb]) =>
              // value packing: only Int/Long/Double values use the unboxed long[] slot; everything else (incl.
              // Char/Byte/Short/Boolean/refs) uses the Object[] slot (boxed). Key: only Int uses IntKeyMap.
              val isI = bTpeR =:= TypeRepr.of[Int]
              val isL = bTpeR =:= TypeRepr.of[Long]
              val isD = bTpeR =:= TypeRepr.of[Double]
              val primVal = isI || isL || isD
              def packB(v: Term): Expr[Long] =
                if isI then '{ ${ v.asExprOf[Int] }.toLong }
                else if isL then v.asExprOf[Long]
                else '{ java.lang.Double.doubleToRawLongBits(${ v.asExprOf[Double] }) }
              def unpackB(l: Expr[Long]): Term =
                if isI then '{ $l.toInt }.asTerm
                else if isL then l.asTerm
                else '{ java.lang.Double.longBitsToDouble($l) }.asTerm
              val intKey = kTpeR =:= TypeRepr.of[Int]
              if intKey then
                // the per-element merge: probe(key); seed or reduce(get, value) — reduce inlined.
                def merge(m: Expr[IntKeyMap], kT: Term, vT: Term): Term =
                  if primVal then
                    '{
                      val slot = $m.probe(${ kT.asExprOf[Int] })
                      if $m.wasNew() then $m.setValuePrim(slot, ${ packB(vT) })
                      else $m.setValuePrim(slot, ${ packB(applyN(reduce, List(unpackB('{ $m.getValuePrim(slot) }), vT))) })
                    }.asTerm
                  else
                    '{
                      val slot = $m.probe(${ kT.asExprOf[Int] })
                      if $m.wasNew() then $m.setValueRef(slot, ${ vT.asExpr }.asInstanceOf[Object])
                      else $m.setValueRef(slot, ${ applyN(reduce, List('{ $m.getValueRef(slot).asInstanceOf[bb] }.asTerm, vT)).asExpr }.asInstanceOf[Object])
                    }.asTerm
                '{
                  val m = new IntKeyMap(16, ${ Expr(primVal) })
                  ${
                    loopS(v => fnOnShape(key, Sc(kk => kk(v)))(kT => fnOnShape(value, Sc(kk => kk(v)))(vT => merge('m, kT, vT))))(sh =>
                      fnOnShape(key, sh)(kT => fnOnShape(value, sh)(vT => merge('m, kT, vT)))
                    )
                  }
                  val b = Map.newBuilder[kk, bb]
                  ${
                    if primVal
                    then '{ m.foreachEntryPrim((k, l) => b += ((k.asInstanceOf[kk], ${ unpackB('l).asExprOf[bb] }))) }
                    else '{ m.foreachEntryRef((k, o) => b += ((k.asInstanceOf[kk], o.asInstanceOf[bb]))) }
                  }
                  b.result()
                }.asTerm
              else
                // FALLBACK for any non-Int key (Long/Double/Char/Ref/…): a boxed mutable HashMap (correct, not
                // yet specialized — Long/Ref-keyed unboxed maps are a follow-up).
                def mergeBoxed(m: Expr[scala.collection.mutable.HashMap[kk, bb]], kT: Term, vT: Term): Term =
                  '{
                    val k = ${ kT.asExprOf[kk] }; val v = ${ vT.asExprOf[bb] }
                    $m.get(k) match
                      case Some(o) => $m.update(k, ${ applyN(reduce, List('{ o }.asTerm, '{ v }.asTerm)).asExprOf[bb] })
                      case None    => $m.update(k, v)
                  }.asTerm
                '{
                  val m = scala.collection.mutable.HashMap.empty[kk, bb]
                  ${
                    loopS(v => fnOnShape(key, Sc(kk => kk(v)))(kT => fnOnShape(value, Sc(kk => kk(v)))(vT => mergeBoxed('m, kT, vT))))(sh =>
                      fnOnShape(key, sh)(kT => fnOnShape(value, sh)(vT => mergeBoxed('m, kT, vT)))
                    )
                  }
                  m.toMap
                }.asTerm
        case TTag.Run => assembleOut(src0, n0, counters, ctx)

    // output array assembly: grow via ensureCap when a flatMap can expand it, else preallocate the upper bound.
    def assembleOut(src0: Expr[FBase], n0: Expr[Int], counters: Map[Int, Slot], ctx: (Term => Term) => Ctx): Term =
      def loop(consume: Term => Term): Expr[Unit] =
        val c = ctx(consume)
        (jsonSrc, streamSrc) match
          case (Some(js), _) => loopOverJson(js, srcElem, indexed, c)
          case (_, Some(ss)) => withScanPrologue(c, loopOverSource(ss, srcK, srcElem, indexed, c))
          case _             => withScanPrologue(c, loopOver(src0, srcK, srcElem, indexed, c))
      outK match
        case Kind.KInt =>
          if needsGrow then
            '{
              var out = new Array[Int](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapInt(out, o + 1); out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new IntOne(out(0))
              else if o == out.length then new IntArr(out, o)
              else new IntArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Int](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Int] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new IntOne(out(0))
              else if o == cap then new IntArr(out, o)
              else new IntArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KLong =>
          if needsGrow then
            '{
              var out = new Array[Long](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapLong(out, o + 1); out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new LongOne(out(0))
              else if o == out.length then new LongArr(out, o)
              else new LongArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Long](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Long] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new LongOne(out(0))
              else if o == cap then new LongArr(out, o)
              else new LongArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KDouble =>
          if needsGrow then
            '{
              var out = new Array[Double](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapDouble(out, o + 1); out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new DoubleOne(out(0))
              else if o == out.length then new DoubleArr(out, o)
              else new DoubleArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Double](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Double] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new DoubleOne(out(0))
              else if o == cap then new DoubleArr(out, o)
              else new DoubleArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KFloat =>
          if needsGrow then
            '{
              var out = new Array[Float](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapFloat(out, o + 1); out(o) = ${ v.asExprOf[Float] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new FloatOne(out(0))
              else if o == out.length then new FloatArr(out, o)
              else new FloatArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Float](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Float] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new FloatOne(out(0))
              else if o == cap then new FloatArr(out, o)
              else new FloatArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KShort =>
          if needsGrow then
            '{
              var out = new Array[Short](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapShort(out, o + 1); out(o) = ${ v.asExprOf[Short] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new ShortOne(out(0))
              else if o == out.length then new ShortArr(out, o)
              else new ShortArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Short](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Short] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new ShortOne(out(0))
              else if o == cap then new ShortArr(out, o)
              else new ShortArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KByte =>
          if needsGrow then
            '{
              var out = new Array[Byte](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapByte(out, o + 1); out(o) = ${ v.asExprOf[Byte] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new ByteOne(out(0))
              else if o == out.length then new ByteArr(out, o)
              else new ByteArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Byte](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Byte] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new ByteOne(out(0))
              else if o == cap then new ByteArr(out, o)
              else new ByteArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KChar =>
          if needsGrow then
            '{
              var out = new Array[Char](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapChar(out, o + 1); out(o) = ${ v.asExprOf[Char] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new CharOne(out(0))
              else if o == out.length then new CharArr(out, o)
              else new CharArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Char](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Char] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new CharOne(out(0))
              else if o == cap then new CharArr(out, o)
              else new CharArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KBoolean =>
          if needsGrow then
            '{
              var out = new Array[Boolean](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapBoolean(out, o + 1); out(o) = ${ v.asExprOf[Boolean] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new BooleanOne(out(0))
              else if o == out.length then new BooleanArr(out, o)
              else new BooleanArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Boolean](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExprOf[Boolean] }; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new BooleanOne(out(0))
              else if o == cap then new BooleanArr(out, o)
              else new BooleanArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
        case Kind.KRef =>
          if needsGrow then
            '{
              var out = new Array[Object](java.lang.Math.max(8, $n0)); var o = 0
              ${ loop(v => '{ if o >= out.length then out = FArrayOps.ensureCapRef(out, o + 1); out(o) = ${ v.asExpr }.asInstanceOf[Object]; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new RefOne(out(0))
              else if o == out.length then new RefArr(out, o)
              else new RefArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm
          else
            '{
              val cap = ${ capExpr(n0, counters) }; val out = new Array[Object](cap); var o = 0
              ${ loop(v => '{ out(o) = ${ v.asExpr }.asInstanceOf[Object]; o += 1 }.asTerm) }
              if o == 0 then (Empty.INSTANCE: FBase)
              else if o == 1 then new RefOne(out(0))
              else if o == cap then new RefArr(out, o)
              else new RefArr(java.util.Arrays.copyOf(out, o), o)
            }.asTerm

    // --- final assembly: bind source + length once, then state vars, then the loop nest ---
    // A JSON or streaming source has no FBase and no static length; n0 is just an initial output-capacity hint
    // (both always use the growable `needsGrow` assembly), so a fixed estimate is fine.
    if isJson || isStream then
      '{
        val src0: FBase = null.asInstanceOf[FBase]; val n0: Int = 16
        ${ withDone(done => declareSlots(counterStages, Map.empty)(counters => assemble('src0, 'n0, done, counters))).asExpr }
      }
    else
      '{
        val src0: FBase = $srcBase; val n0: Int = src0.length
        ${ withDone(done => declareSlots(counterStages, Map.empty)(counters => assemble('src0, 'n0, done, counters))).asExpr }
      }
