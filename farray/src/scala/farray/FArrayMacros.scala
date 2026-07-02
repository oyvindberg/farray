package farray

import scala.quoted.*

private[farray] object FArrayMacros:

  /** `FArray(a, b, c, …)` with a literal argument list constructs the LEAF NODE directly — arity 0 is
    * `Empty.INSTANCE`, arity 1 a `${K}One`, arity n a typed backing array filled with unrolled stores and
    * wrapped in `new ${K}Arr(out, n)`. No inline-def indirection, no varargs `Seq`, no boxing, no runtime
    * canonicalisation (the arity is a compile-time fact): the expansion IS the optimal code, and it is
    * exactly the shape `FuseMacro.literalFArrayElems` recognizes and splats. A non-literal spread
    * (`FArray(xs*)`) falls back to the runtime `applyImpl`.
    */
  def applyMacro[A: Type](as: Expr[Seq[A]])(using Quotes): Expr[FArray[A]] =
    import quotes.reflect.*

    def asFA(e: Expr[FBase]): Expr[FArray[A]] = '{ $e.asInstanceOf[FArray[A]] }
    def runtimeFallback: Expr[FArray[A]] = '{ FArrayOps.applyImpl[A]($as).asInstanceOf[FArray[A]] }

    as match
      case Varargs(args) =>
        val n = Expr(args.size)
        val aTpe = TypeRepr.of[A]
        def is(t: TypeRepr): Boolean = aTpe =:= t

        // { val out = new Array[T](n); out(0) = e0; …; new ${K}Arr(out, n) } — unrolled stores into a
        // fresh typed array, then the leaf ctor directly (n >= 2 here, so no size canonicalisation).
        def build[T: Type](newArr: Expr[Array[T]], node: Expr[Array[T]] => Expr[FBase]): Expr[FArray[A]] =
          val out = Symbol.newVal(Symbol.spliceOwner, "out", TypeRepr.of[Array[T]], Flags.EmptyFlags, Symbol.noSymbol)
          val ref = Ref(out)
          val stores = args.zipWithIndex.map { (a, i) =>
            Apply(Select.unique(ref, "update"), List(Literal(IntConstant(i)), a.asExprOf[T].asTerm))
          }.toList
          Block(ValDef(out, Some(newArr.asTerm)) :: stores, asFA(node(ref.asExprOf[Array[T]])).asTerm).asExprOf[FArray[A]]

        if args.isEmpty then asFA('{ Empty.INSTANCE })
        else if args.size == 1 then
          val a = args.head
          if is(TypeRepr.of[Int]) then asFA('{ new IntOne(${ a.asExprOf[Int] }) })
          else if is(TypeRepr.of[Long]) then asFA('{ new LongOne(${ a.asExprOf[Long] }) })
          else if is(TypeRepr.of[Double]) then asFA('{ new DoubleOne(${ a.asExprOf[Double] }) })
          else if is(TypeRepr.of[Float]) then asFA('{ new FloatOne(${ a.asExprOf[Float] }) })
          else if is(TypeRepr.of[Short]) then asFA('{ new ShortOne(${ a.asExprOf[Short] }) })
          else if is(TypeRepr.of[Byte]) then asFA('{ new ByteOne(${ a.asExprOf[Byte] }) })
          else if is(TypeRepr.of[Char]) then asFA('{ new CharOne(${ a.asExprOf[Char] }) })
          else if is(TypeRepr.of[Boolean]) then asFA('{ new BooleanOne(${ a.asExprOf[Boolean] }) })
          else if aTpe <:< TypeRepr.of[AnyRef] then asFA('{ new RefOne(${ a.asExprOf[AnyRef] }) })
          else runtimeFallback
        else if is(TypeRepr.of[Int]) then build[Int]('{ new Array[Int]($n) }, out => '{ new IntArr($out, $n) })
        else if is(TypeRepr.of[Long]) then build[Long]('{ new Array[Long]($n) }, out => '{ new LongArr($out, $n) })
        else if is(TypeRepr.of[Double]) then build[Double]('{ new Array[Double]($n) }, out => '{ new DoubleArr($out, $n) })
        else if is(TypeRepr.of[Float]) then build[Float]('{ new Array[Float]($n) }, out => '{ new FloatArr($out, $n) })
        else if is(TypeRepr.of[Short]) then build[Short]('{ new Array[Short]($n) }, out => '{ new ShortArr($out, $n) })
        else if is(TypeRepr.of[Byte]) then build[Byte]('{ new Array[Byte]($n) }, out => '{ new ByteArr($out, $n) })
        else if is(TypeRepr.of[Char]) then build[Char]('{ new Array[Char]($n) }, out => '{ new CharArr($out, $n) })
        else if is(TypeRepr.of[Boolean]) then build[Boolean]('{ new Array[Boolean]($n) }, out => '{ new BooleanArr($out, $n) })
        else if aTpe <:< TypeRepr.of[AnyRef] then
          // reference elements: allocate a TYPED array when a ClassTag is summonable (toArray then takes
          // the unchecked-arraycopy path — same trade as tabulate); plain Object[] otherwise.
          Expr.summon[scala.reflect.ClassTag[A]] match
            case Some(ct) => build[A]('{ $ct.newArray($n) }, out => '{ new RefArr($out.asInstanceOf[Array[Object]], $n) })
            case None     => build[AnyRef]('{ new Array[AnyRef]($n) }, out => '{ new RefArr($out.asInstanceOf[Array[Object]], $n) })
        else runtimeFallback
      case _ => runtimeFallback
