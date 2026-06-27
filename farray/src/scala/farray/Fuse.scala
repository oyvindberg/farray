package farray

/**
 * Fused-pipeline builder. `xs.fuse.map(f).filter(p).take(k).toFArray` compiles to ONE unboxed traversal of
 * `xs` producing ONE output collection — no intermediate `FArray` per stage, no per-element boxing or virtual
 * calls. See `docs/fused-pipeline-design.md`.
 *
 * The combinator methods below are MARKERS: their bodies never run. The terminal methods (`toFArray`, …) are
 * `inline` macros that read the whole `xs.fuse.…` chain off the AST, peel the stage list + the (inlined)
 * lambdas, and emit the fused loop. The `Fuse` wrapper itself is elided by the macro — only `xs` and the
 * lambda bodies survive into the generated code.
 */
final class Fuse[+A](private[farray] val base: FBase):
  // ---- stage markers (bodies irrelevant; the macro reads these calls off the AST) ----
  def map[B](f: A => B): Fuse[B] = this.asInstanceOf[Fuse[B]]
  def filter(p: A => Boolean): Fuse[A] = this
  def filterNot(p: A => Boolean): Fuse[A] = this
  def take(n: Int): Fuse[A] = this
  def drop(n: Int): Fuse[A] = this

  // ---- terminals (macros: rewrite the whole chain into one fused loop) ----
  inline def toFArray: FArray[A] = ${ FuseMacro.toFArrayImpl[A]('this) }
  inline def foreach(inline f: A => Unit): Unit = ${ FuseMacro.foreachImpl[A]('this, 'f) }
  inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = ${ FuseMacro.foldLeftImpl[A, Z]('this, 'z, 'op) }
  /** number of elements surviving the whole pipeline. */
  inline def count: Int = ${ FuseMacro.countImpl[A]('this) }
