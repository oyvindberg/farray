package farray

import scala.quoted.*

private[farray] object FArrayMacros:

  /** `FArray(a, b, c, …)` with a literal argument list builds the backing array directly with primitive stores — specialised on the element type so an
    * `Array[Int]`/`Long`/`Double` is filled without boxing (an abstract `Array[A]` erases to `Object[]` and would box every store). A non-literal spread
    * (`FArray(xs*)`) falls back to the runtime `applyImpl`.
    */
  def applyMacro[A: Type](as: Expr[Seq[A]])(using Quotes): Expr[FArray[A]] =
    import quotes.reflect.*
    as match
      case Varargs(args) =>
        val n = Expr(args.size)

        def build[T: Type](newArr: Expr[Array[T]]): Expr[FArray[A]] =
          val out = Symbol.newVal(Symbol.spliceOwner, "out", TypeRepr.of[Array[T]], Flags.EmptyFlags, Symbol.noSymbol)
          val ref = Ref(out)
          val stores = args.zipWithIndex.map { (a, i) =>
            Apply(Select.unique(ref, "update"), List(Literal(IntConstant(i)), a.asExprOf[T].asTerm))
          }.toList
          val wrapped = '{ FArray.fromArray[A](${ ref.asExprOf[Array[T]] }.asInstanceOf[Array[A]]) }
          Block(ValDef(out, Some(newArr.asTerm)) :: stores, wrapped.asTerm).asExprOf[FArray[A]]

        TypeRepr.of[A] match
          case t if t =:= TypeRepr.of[Int]    => build[Int]('{ new Array[Int]($n) })
          case t if t =:= TypeRepr.of[Long]   => build[Long]('{ new Array[Long]($n) })
          case t if t =:= TypeRepr.of[Double] => build[Double]('{ new Array[Double]($n) })
          case _                              =>
            Expr.summon[scala.reflect.ClassTag[A]] match
              case Some(ct) => build[A]('{ $ct.newArray($n) }) // reference element: store is a plain ref, no boxing
              case None     => '{ FArrayOps.applyImpl[A]($as).asInstanceOf[FArray[A]] }
      case _ => '{ FArrayOps.applyImpl[A]($as).asInstanceOf[FArray[A]] }
