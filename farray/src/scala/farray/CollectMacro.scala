package farray

import scala.quoted.*

/** Picks a LITERAL PartialFunction apart at compile time, into the two lambdas the eager machinery already knows how to unbox: a predicate `p` (the pattern +
  * guard) and a transform `f` (the case body). `xs.collect { case x if g => b }` then compiles exactly like `xs.filter(g).map(b)` fused into one pass — no
  * PartialFunction object, no isDefinedAt/apply boxing, guard evaluated once.
  *
  * Shapes:
  *   - single case with a simple binder (`case x if g => b`, `case _ => b`): p = the guard (or const true), f = the body — no match is emitted at all.
  *   - anything else (destructuring, multiple cases): p = the match answering true/false, f = the match producing the body. Patterns and guards then run in
  *     BOTH lambdas, so guards may evaluate more than once per element — the standard purity assumption, stated in the docs.
  *   - not a literal at all (a stored PF value): falls back to the runtime collectImpl unchanged.
  *
  * Hygiene: case trees can't be reused in two lambdas verbatim — their pattern binders are symbols, and one symbol may not be defined twice. `freshened`
  * deep-clones every Bind in the pattern with a fresh symbol and rewrites guard/body references accordingly.
  */
private[farray] object CollectMacro:
  def impl[A: Type, B: Type](xs: Expr[FArray[A]], pf: Expr[PartialFunction[A, B]])(using Quotes): Expr[FArray[B]] =
    import quotes.reflect.*

    def unwrap(t: Term): Term = t match
      case Inlined(_, Nil, b) => unwrap(b)
      case Typed(b, _)        => unwrap(b)
      case Block(Nil, b)      => unwrap(b)
      case _                  => t

    // a PF literal is Block(List(DefDef($anonfun, (x$1), Match(x$1, cases))), Closure(_, PartialFunction))
    def extract(t: Term): Option[(Symbol, List[CaseDef])] = unwrap(t) match
      case Block(stats, _: Closure) =>
        stats.collectFirst { case dd: DefDef => dd }.flatMap { dd =>
          val ps = dd.paramss.flatMap { case TermParamClause(tps) => tps; case _ => Nil }
          dd.rhs.map(unwrap).collect { case Match(_, cs) if ps.nonEmpty => (ps.head.symbol, cs) }
        }
      case _ => None

    def substitute(mapping: Map[Symbol, Term])(t: Term): Term =
      (new TreeMap:
        override def transformTerm(x: Term)(o: Symbol): Term = x match
          case id: Ident if mapping.contains(id.symbol) => mapping(id.symbol)
          case _                                        => super.transformTerm(x)(o)
      ).transformTerm(t)(Symbol.spliceOwner)

    // deep-clone a case's pattern with fresh Bind symbols (owned by `owner`), rewriting guard/body refs
    def freshened(cd: CaseDef, owner: Symbol): (CaseDef, Map[Symbol, Term]) =
      val mapping = scala.collection.mutable.Map.empty[Symbol, Term]
      def clonePat(t: Tree): Tree = t match
        case b @ Bind(name, pat) =>
          val fresh = Symbol.newBind(owner, name, b.symbol.flags, b.symbol.termRef.widen)
          mapping(b.symbol) = Ref(fresh)
          Bind(fresh, clonePat(pat))
        case u @ Unapply(fun, implicits, patterns) => Unapply.copy(u)(fun, implicits, patterns.map(clonePat))
        case t @ Typed(e, tpt)                     => Typed.copy(t)(clonePat(e).asInstanceOf[Term], tpt)
        case a @ Alternatives(ps)                  => Alternatives.copy(a)(ps.map(clonePat))
        case other                                 => other
      val pat = clonePat(cd.pattern)
      val m = mapping.toMap
      (CaseDef(pat, cd.guard.map(substitute(m)), substitute(m)(cd.rhs)), m)

    def lam(res: TypeRepr)(bodyFor: (Term, Symbol) => Term): Term =
      val mt = MethodType(List("a"))(_ => List(TypeRepr.of[A]), _ => res)
      Lambda(Symbol.spliceOwner, mt, (meth, args) => bodyFor(args.head.asInstanceOf[Term], meth).changeOwner(meth))

    def isSimpleBinder(cd: CaseDef): Boolean = cd.pattern match
      case Bind(_, Wildcard()) => true
      case Wildcard()          => true
      case _                   => false

    extract(pf.asTerm) match
      case Some((xSym, cases)) =>
        val (pTerm, fTerm) = cases match
          case List(cd) if isSimpleBinder(cd) =>
            // fast path: no match at all — guard is p, body is f, the binder becomes the lambda param
            def bindX(a: Term)(t: Term): Term =
              val m: Map[Symbol, Term] = cd.pattern match
                case b @ Bind(_, _) => Map(xSym -> a, b.symbol -> a)
                case _              => Map(xSym -> a)
              substitute(m)(t)
            val p = lam(TypeRepr.of[Boolean])((a, _) => cd.guard.map(bindX(a)).getOrElse(Literal(BooleanConstant(true))))
            val f = lam(TypeRepr.of[B])((a, _) => bindX(a)(cd.rhs))
            (p, f)
          case _ =>
            val p = lam(TypeRepr.of[Boolean]) { (a, meth) =>
              val cs = cases.map { cd =>
                val (fcd, _) = freshened(cd, meth)
                CaseDef(fcd.pattern, fcd.guard.map(substitute(Map(xSym -> a))), Literal(BooleanConstant(true)))
              }
              Match(a, cs :+ CaseDef(Wildcard(), None, Literal(BooleanConstant(false))))
            }
            val f = lam(TypeRepr.of[B]) { (a, meth) =>
              val cs = cases.map { cd =>
                val (fcd, _) = freshened(cd, meth)
                CaseDef(fcd.pattern, fcd.guard.map(substitute(Map(xSym -> a))), substitute(Map(xSym -> a))(fcd.rhs))
              }
              val boom = '{ throw new MatchError(${ a.asExprOf[Any] }) }.asTerm
              Match(a, cs :+ CaseDef(Wildcard(), None, boom))
            }
            (p, f)
        '{
          FArrayOps
            .collectPickImpl[A, B](${ xs }.asInstanceOf[FBase])(${ pTerm.asExprOf[A => Boolean] }, ${ fTerm.asExprOf[A => B] })
            .asInstanceOf[FArray[B]]
        }
      case None =>
        '{ FArrayOps.collectImpl[A, B](${ xs }.asInstanceOf[FBase])($pf).asInstanceOf[FArray[B]] }
