package farray

/**
 * `FArray` — opaque type over the sealed core hierarchy [[FBase]] (leaf cores `IntArr`/`RefArr`/…,
 * the `Concat` tree node, unary `…Append`/`…Prepend` nodes). `++`/`:+`/`+:` are O(1) (build a node,
 * base shared).
 *
 * This file is the stable, hand-written API surface. The per-kind specialized implementations — the
 * `erasedValue` dispatch, the explicit-stack `dfs<Kind>` traversals, the `map` read×write matrix —
 * are GENERATED into `FArrayOps` (one template per combinator in `GenCores`), so adding a primitive
 * is one row in `GenCores.opKinds`. Element ops forward into `FArrayOps`; structural ops forward to
 * the tree-aware `FBase` virtuals.
 *
 * Specialize-or-fail: the generated dispatch's reference branch is `case _: AnyRef`, so an abstract
 * `A` cannot reduce → COMPILE ERROR (no silent boxing, no covariant-access CCE).
 */
opaque type FArray[+A] = FBase

object FArray:

  inline def empty[A]: FArray[A] = FArrayOps.emptyImpl[A]
  inline def apply[A](as: A*): FArray[A] = FArrayOps.applyImpl[A](as)
  inline def tabulate[A](n: Int)(inline f: Int => A): FArray[A] = FArrayOps.tabulateImpl[A](n)(f)
  inline def fromArray[A](as: Array[A]): FArray[A] = FArrayOps.fromArrayImpl[A](as)

  extension [A](xs: FArray[A])
    // shape — element type irrelevant
    def length: Int = xs.length
    def size: Int = xs.length
    def isEmpty: Boolean = xs.length == 0
    def nonEmpty: Boolean = xs.length > 0
    def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)

    // structural ops → tree-aware FBase virtuals (O(1)/O(log) over the tree)
    def take(n: Int): FArray[A] = xs.take(n)
    def drop(n: Int): FArray[A] = xs.drop(n)
    def slice(from: Int, until: Int): FArray[A] = xs.slice(from, until)
    def reverse: FArray[A] = xs.reverse
    def init: FArray[A] = xs.init
    def tail: FArray[A] = xs.drop(1)
    def ++[B >: A](that: FArray[B]): FArray[B] = xs.concat(that)

    // element ops → generated per-kind impls
    inline def apply(i: Int): A = FArrayOps.applyAtImpl[A](xs, i)
    inline def head: A = FArrayOps.applyAtImpl[A](xs, 0)
    inline def last: A = FArrayOps.applyAtImpl[A](xs, xs.length - 1)
    inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = FArrayOps.foldLeftImpl[A, Z](xs, z)(op)
    inline def foreach(inline f: A => Unit): Unit = FArrayOps.foreachImpl[A](xs)(f)
    inline def map[B](inline f: A => B): FArray[B] = FArrayOps.mapImpl[A, B](xs)(f)
    inline def filter(inline p: A => Boolean): FArray[A] = FArrayOps.filterImpl[A](xs)(p)
    inline def filterNot(inline p: A => Boolean): FArray[A] = FArrayOps.filterImpl[A](xs)(a => !p(a))
    inline def contains(elem: A): Boolean = FArrayOps.containsImpl[A](xs, elem)
    // f returns FArray[B] (opaque at the external inline site) — cast to the underlying FBase the impl wants.
    inline def flatMap[B](inline f: A => FArray[B]): FArray[B] = FArrayOps.flatMapImpl[A, B](xs)(a => f(a).asInstanceOf[FBase])
    inline def updated[B >: A](index: Int, elem: B): FArray[B] = FArrayOps.updatedImpl[A, B](xs, index, elem)
    inline def :+[B >: A](elem: B): FArray[B] = FArrayOps.appendImpl[A, B](xs, elem)

    def iterator: Iterator[A] =
      val core: FBase = xs
      new Iterator[A]:
        private var i = 0
        def hasNext: Boolean = i < core.length
        def next(): A =
          val r = core.applyBoxed(i).asInstanceOf[A]
          i += 1
          r

  extension [A](elem: A)
    inline def +: (xs: FArray[A]): FArray[A] = FArrayOps.prependImpl[A](elem, xs)
