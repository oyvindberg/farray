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
 *
 * Semantics — assume PURE stage functions. A fused pipeline is lazy/short-circuiting: `take`, `find`, `head`,
 * `exists`/`forall` stop the traversal as soon as the answer is known, so a stage function (incl. a `flatMap`'s)
 * may run FEWER times than in the equivalent strict `List` pipeline (e.g. `xs.fuse.flatMap(f).take(3)` invokes
 * `f` only until 3 elements are produced). Likewise, when a `map` produces a tuple of independent columns and a
 * later `filter` uses only some of them, the OTHER columns are computed only for elements that pass the filter —
 * "compute-for-survivors" (e.g. `xs.fuse.map(x => (cheap(x), expensive(x))).filter(_._1 > 0).map(_._2)` runs
 * `expensive` only on survivors, and never allocates the tuple). For a pure `f` the result is identical; with a
 * side-effecting or throwing `f` the observable behavior (call count, whether it throws) can differ. The element
 * type must be Int/Long/Double or a reference type (`<: AnyRef`) — a primitive-backed FArray widened to
 * `Any`/`AnyVal` is a compile error, not a silent miscompile.
 */
final class Fuse[+A](private[farray] val base: FBase):
  // ---- stage markers (bodies irrelevant; the macro reads these calls off the AST) ----
  def map[B](f: A => B): Fuse[B] = this.asInstanceOf[Fuse[B]]
  def flatMap[B](f: A => FArray[B]): Fuse[B] = this.asInstanceOf[Fuse[B]]
  def filter(p: A => Boolean): Fuse[A] = this
  def filterNot(p: A => Boolean): Fuse[A] = this
  def take(n: Int): Fuse[A] = this
  def drop(n: Int): Fuse[A] = this
  /** pair each element with its position in the stream at this point (post-upstream-filtering). */
  def zipWithIndex: Fuse[(A, Int)] = this.asInstanceOf[Fuse[(A, Int)]]

  // ---- terminals (macros: rewrite the whole chain into one fused loop) ----
  inline def toFArray: FArray[A] = ${ FuseMacro.toFArrayImpl[A]('this) }
  inline def foreach(inline f: A => Unit): Unit = ${ FuseMacro.foreachImpl[A]('this, 'f) }
  inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = ${ FuseMacro.foldLeftImpl[A, Z]('this, 'z, 'op) }
  /** number of elements surviving the whole pipeline. */
  inline def count: Int = ${ FuseMacro.countImpl[A]('this) }
  // ---- short-circuit terminals: stop as soon as the answer is known (across flatMap nesting) ----
  inline def find(inline p: A => Boolean): Option[A] = ${ FuseMacro.findImpl[A]('this, 'p) }
  inline def exists(inline p: A => Boolean): Boolean = ${ FuseMacro.existsImpl[A]('this, 'p) }
  inline def forall(inline p: A => Boolean): Boolean = ${ FuseMacro.forallImpl[A]('this, 'p) }
  inline def headOption: Option[A] = ${ FuseMacro.headOptionImpl[A]('this) }
  inline def head: A = ${ FuseMacro.headImpl[A]('this) }
