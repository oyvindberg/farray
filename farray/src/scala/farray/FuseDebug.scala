package farray

import scala.quoted.*

/** Debug aid: pretty-print the EXPANDED code of an expression. For a fused terminal — e.g.
 *  `xs.fuse.map(f).filter(p).toFArray` — this is the generated one-pass loop, which the snapshot tests
 *  check into git so the lowering is visible and reviewable. The expression is shown, never executed. */
object FuseDebug:
  inline def show[A](inline expr: A): String = ${ showImpl[A]('expr) }

  def showImpl[A: Type](expr: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr(expr.asTerm.show(using Printer.TreeShortCode))
