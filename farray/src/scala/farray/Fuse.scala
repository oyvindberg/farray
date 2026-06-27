package farray

import scala.reflect.ClassTag

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
  /** lock-step with another source: pair element k of this pipeline with `that(k)`; stops at the shorter. */
  def zip[B](that: FArray[B]): Fuse[(A, B)] = this.asInstanceOf[Fuse[(A, B)]]
  /** lock-step combine with another source via `f` (like `zip(that).map(f)` but never builds the pair). */
  def map2[B, C](that: FArray[B])(f: (A, B) => C): Fuse[C] = this.asInstanceOf[Fuse[C]]

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

  // ===== derived terminals — pure sugar over the base terminals above; the whole pipeline still fuses =====

  // ---- conversions (one fused pass into a builder via foreach) ----
  inline def toList: List[A] = { val b = List.newBuilder[A]; foreach(b += _); b.result() }
  inline def toVector: Vector[A] = { val b = Vector.newBuilder[A]; foreach(b += _); b.result() }
  inline def toSeq: Seq[A] = toVector
  inline def toSet[B >: A]: Set[B] = { val b = Set.newBuilder[B]; foreach(b += _); b.result() }
  inline def toArray[B >: A](using ClassTag[B]): Array[B] = { val b = Array.newBuilder[B]; foreach(b += _); b.result() }
  inline def toMap[K, V](using ev: A <:< (K, V)): Map[K, V] =
    val b = Map.newBuilder[K, V]; foreach(a => b += ev(a)); b.result()
  inline def mkString(start: String, sep: String, end: String): String =
    val sb = new java.lang.StringBuilder(start); var first = true
    foreach { a => if first then first = false else sb.append(sep); sb.append(String.valueOf(a.asInstanceOf[Object])) }
    sb.append(end).toString
  inline def mkString(sep: String): String = mkString("", sep, "")
  inline def mkString: String = mkString("", "", "")

  // ---- reductions (one fused pass via foldLeft) ----
  inline def fold[B >: A](z: B)(inline op: (B, B) => B): B = foldLeft[B](z)((acc, a) => op(acc, a))
  inline def sum[B >: A](using num: Numeric[B]): B = foldLeft[B](num.zero)((acc, a) => num.plus(acc, a))
  inline def product[B >: A](using num: Numeric[B]): B = foldLeft[B](num.one)((acc, a) => num.times(acc, a))
  inline def reduceLeft[B >: A](inline op: (B, A) => B): B =
    foldLeft[Option[B]](None)((acc, a) => Some(if acc.isEmpty then a else op(acc.get, a)))
      .getOrElse(throw new UnsupportedOperationException("reduceLeft on an empty fused pipeline"))
  inline def reduce[B >: A](inline op: (B, B) => B): B = reduceLeft[B]((acc, a) => op(acc, a))
  inline def min[B >: A](using ord: Ordering[B]): A = reduceLeft[B]((acc, a) => if ord.lteq(acc, a) then acc else a).asInstanceOf[A]
  inline def max[B >: A](using ord: Ordering[B]): A = reduceLeft[B]((acc, a) => if ord.gteq(acc, a) then acc else a).asInstanceOf[A]
  inline def minBy[B](inline f: A => B)(using ord: Ordering[B]): A =
    foldLeft[Option[(A, B)]](None)((acc, a) => { val k = f(a); if acc.isEmpty || ord.lt(k, acc.get._2) then Some((a, k)) else acc })
      .getOrElse(throw new UnsupportedOperationException("minBy on an empty fused pipeline"))._1
  inline def maxBy[B](inline f: A => B)(using ord: Ordering[B]): A =
    foldLeft[Option[(A, B)]](None)((acc, a) => { val k = f(a); if acc.isEmpty || ord.gt(k, acc.get._2) then Some((a, k)) else acc })
      .getOrElse(throw new UnsupportedOperationException("maxBy on an empty fused pipeline"))._1

  // ---- last (one fused pass — keeps the most recent survivor) ----
  inline def lastOption: Option[A] = foldLeft[Option[A]](None)((_, a) => Some(a))
  inline def last: A = lastOption.getOrElse(throw new NoSuchElementException("last of an empty fused pipeline"))

  // ---- predicates / counts ----
  inline def contains[B >: A](elem: B): Boolean = exists(_ == elem)
  inline def isEmpty: Boolean = !exists(_ => true)
  inline def nonEmpty: Boolean = exists(_ => true)
  inline def size: Int = count
  inline def length: Int = count

  // ---- positional / partial (short-circuit, via zipWithIndex.find / find) ----
  inline def indexWhere(inline p: A => Boolean): Int = zipWithIndex.find(t => p(t._1)).map(_._2).getOrElse(-1)
  inline def indexOf[B >: A](elem: B): Int = indexWhere(_ == elem)
  inline def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = find(pf.isDefinedAt).map(pf)

  // ---- generic conversion: build any collection from a std Factory in one fused pass ----
  inline def to[C1](factory: scala.collection.Factory[A, C1]): C1 =
    val b = factory.newBuilder; foreach(b += _); b.result()

  // ---- Option-returning reductions (empty → None instead of throwing) ----
  inline def reduceOption[B >: A](inline op: (B, A) => B): Option[B] =
    foldLeft[Option[B]](None)((acc, a) => Some(if acc.isEmpty then a else op(acc.get, a)))
  inline def reduceLeftOption[B >: A](inline op: (B, A) => B): Option[B] = reduceOption[B](op)
  inline def minOption[B >: A](using ord: Ordering[B]): Option[A] =
    reduceOption[B]((acc, a) => if ord.lteq(acc, a) then acc else a).asInstanceOf[Option[A]]
  inline def maxOption[B >: A](using ord: Ordering[B]): Option[A] =
    reduceOption[B]((acc, a) => if ord.gteq(acc, a) then acc else a).asInstanceOf[Option[A]]

  // ---- groupBy: one fused pass into per-key builders ----
  inline def groupBy[K](inline f: A => K): Map[K, List[A]] =
    val m = scala.collection.mutable.LinkedHashMap.empty[K, scala.collection.mutable.Builder[A, List[A]]]
    foreach(a => m.getOrElseUpdate(f(a), List.newBuilder[A]) += a)
    m.view.mapValues(_.result()).toMap
