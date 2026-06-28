package farray

import scala.quoted.*

/** Pure `quotes.reflect` AST helpers shared by the fusion engine (`FuseMacro`) and the decomposed-source decoders
  * (e.g. `farray.json.JsonDecode`). These are tiny, stateless walks over `Term`/`TypeRepr` — lambda decomposition,
  * product-field inspection, and field-path collection/substitution — with NO dependency on the fusion optimizer's
  * `Shape`/`Ctx`/`Stage` model, and none on any source format. The one place engine and decoders agree on how to read
  * a record's fields off a lambda.
  */
private[farray] object Ast:
  /** a field path into a product: `(column index, accessor name)` per hop. `p.a.x` → `List((iₐ,"a"),(iₓ,"x"))`;
    * a bare `p` → `Nil` (the param used whole).
    */
  type Path = List[(Int, String)]

  /** peel `Inlined`/`Typed`/empty-`Block` wrappers to the underlying term. */
  def unwrap(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    t match
      case Inlined(_, _, e) => unwrap(e)
      case Typed(e, _)      => unwrap(e)
      case Block(Nil, e)    => unwrap(e)
      case _                => t

  /** the field list of a flat case class / tuple: `Some((name, fieldType)*)`, else `None`. */
  def productFields(using q: Quotes)(tpe: q.reflect.TypeRepr): Option[List[(String, q.reflect.TypeRepr)]] =
    import q.reflect.*
    val sym = tpe.typeSymbol
    if sym.flags.is(Flags.Case) && sym.caseFields.nonEmpty then Some(sym.caseFields.map(f => (f.name, tpe.memberType(f))))
    else None

  /** the index of field `name` in `inner`'s product type, if `inner.name` is a product-field access. */
  def fieldAccess(using q: Quotes)(inner: q.reflect.Term, name: String): Option[Int] =
    productFields(inner.tpe.widen).flatMap { fs =>
      val i = fs.indexWhere(_._1 == name); if i >= 0 then Some(i) else None
    }

  def isFieldSelect(using q: Quotes)(t: q.reflect.Term): Option[(q.reflect.Term, Int, String)] =
    import q.reflect.*
    t match
      case Select(inner, name) => fieldAccess(inner, name).map(i => (inner, i, name))
      case _                   => None

  /** a CANONICAL product construction `C(a, b, …)` / `(a, b)` / `new C(…)` → (product type, field args). Only the
    * product's OWN apply/constructor counts (not an arbitrary factory returning C), so args == fields.
    */
  def isProductCtor(using q: Quotes)(t: q.reflect.Term): Option[(q.reflect.TypeRepr, List[q.reflect.Term])] =
    import q.reflect.*
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

  /** a maximal field PATH rooted at a symbol: `p.a.x` → `(p, List((iₐ,"a"),(iₓ,"x")))`; a bare `p` → `(p, Nil)`. */
  def projPath(using q: Quotes)(t: q.reflect.Term): Option[(q.reflect.Symbol, Path)] =
    import q.reflect.*
    t match
      case id: Ident => Some((id.symbol, Nil))
      case Select(inner, name) =>
        (projPath(inner), fieldAccess(inner, name)) match
          case (Some((s, p)), Some(idx)) => Some((s, p :+ (idx, name)))
          case _                         => None
      case _ => None

  /** a 1-param lambda `p => body` → `(p, body)`. */
  def decomposeLambda(using q: Quotes)(t: q.reflect.Term): Option[(q.reflect.Symbol, q.reflect.Term)] =
    import q.reflect.*
    unwrap(t) match
      case Lambda(List(vd), body) => Some((vd.symbol, body))
      case _                      => None

  /** a 2-param lambda `(p0, p1) => body` → `(p0, p1, body)`. Used to decompose a fold/reduce `op`. */
  def decomposeLambda2(using q: Quotes)(t: q.reflect.Term): Option[(q.reflect.Symbol, q.reflect.Symbol, q.reflect.Term)] =
    import q.reflect.*
    unwrap(t) match
      case Lambda(List(vd0, vd1), body) => Some((vd0.symbol, vd1.symbol, body))
      case _                            => None

  /** every maximal field path from `param` in `body` (a `Nil` path = param used whole). */
  def collectPaths(using q: Quotes)(body: q.reflect.Term, param: q.reflect.Symbol): List[Path] =
    import q.reflect.*
    val buf = scala.collection.mutable.LinkedHashSet.empty[Path]
    (new TreeTraverser:
      override def traverseTree(t: Tree)(owner: Symbol): Unit = t match
        case (_: Ident | _: Select) if projPath(t.asInstanceOf[Term]).exists((s, _) => s == param) =>
          buf += projPath(t.asInstanceOf[Term]).get._2 // maximal: don't descend into inner field reads
        case _ => super.traverseTree(t)(owner)
    ).traverseTree(body)(Symbol.spliceOwner)
    buf.toList

  /** replace every reference to `param` with `repl`. */
  def substParam(using q: Quotes)(body: q.reflect.Term, param: q.reflect.Symbol, repl: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    (new TreeMap:
      override def transformTerm(t: Term)(owner: Symbol): Term = t match
        case id: Ident if id.symbol == param => repl
        case _                               => super.transformTerm(t)(owner)
    ).transformTerm(body)(Symbol.spliceOwner)

  /** replace each maximal param-field path with its resolved column ref. */
  def substPaths(using q: Quotes)(body: q.reflect.Term, param: q.reflect.Symbol, refs: Map[Path, q.reflect.Term]): q.reflect.Term =
    import q.reflect.*
    (new TreeMap:
      override def transformTerm(t: Term)(owner: Symbol): Term = projPath(t) match
        case Some((s, p)) if s == param && refs.contains(p) => refs(p)
        case _                                              => super.transformTerm(t)(owner)
    ).transformTerm(body)(Symbol.spliceOwner)
