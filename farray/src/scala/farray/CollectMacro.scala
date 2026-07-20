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

    def lam(res: TypeRepr)(bodyFor: (Term, Symbol) => Term): Term =
      val mt = MethodType(List("a"))(_ => List(TypeRepr.of[A]), _ => res)
      Lambda(Symbol.spliceOwner, mt, (meth, args) => bodyFor(args.head.asInstanceOf[Term], meth).changeOwner(meth))

    def isSimpleBinder(cd: CaseDef): Boolean = cd.pattern match
      case Bind(_, Wildcard()) => true
      case Wildcard()          => true
      case _                   => false

    // Decompose ONLY a single simple-binder case (`case x if g => b` / `case _ => b`): its predicate/transform
    // just substitute the binder for the loop element and introduce NO fresh pattern-bind symbols. A
    // destructuring or multi-case literal would need pattern binds owned by the generated predicate/transform
    // lambda; when `collectPickImpl` inline-expands (beta-reduces) that lambda, those binds' owner is left
    // dangling and a `-Yexplicit-nulls` caller's LambdaLift crashes ("key not found: method $anonfun" — the
    // Scala 3 compiler's own build hit this on `xs.collect { case B(lo, hi) if … => … }`). Such PFs (and any
    // non-literal PF) fall back to the runtime `collectImpl`, which keeps the PartialFunction's own `$anonfun`
    // live in the tree and is therefore owner-safe (at the cost of the fused single-pass unboxing).
    extract(pf.asTerm) match
      case Some((xSym, List(cd))) if isSimpleBinder(cd) =>
        // fast path: no match at all — guard is p, body is f, the binder becomes the lambda param
        def bindX(a: Term)(t: Term): Term =
          val m: Map[Symbol, Term] = cd.pattern match
            case b @ Bind(_, _) => Map(xSym -> a, b.symbol -> a)
            case _              => Map(xSym -> a)
          substitute(m)(t)
        val p = lam(TypeRepr.of[Boolean])((a, _) => cd.guard.map(bindX(a)).getOrElse(Literal(BooleanConstant(true))))
        val f = lam(TypeRepr.of[B])((a, _) => bindX(a)(cd.rhs))
        '{
          FArrayOps
            .collectPickImpl[A, B](${ xs }.asInstanceOf[FBase])(${ p.asExprOf[A => Boolean] }, ${ f.asExprOf[A => B] })
            .asInstanceOf[FArray[B]]
        }
      case _ =>
        '{ FArrayOps.collectImpl[A, B](${ xs }.asInstanceOf[FBase])($pf).asInstanceOf[FArray[B]] }
