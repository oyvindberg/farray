package farray

import scala.reflect.ClassTag

/** `FArray` — opaque type over the sealed core hierarchy [[FBase]]. Every lambda-taking op is `inline` with an `inline` function param and applies the lambda
  * through the specialized `foldLeft`/`foreach`/ `map`/`filter`/`take`/`drop` machinery — so the function inlines and primitives stay unboxed, exactly like
  * `map`. Only inherently-materializing structures (the Map/Set behind `groupBy`/`distinct`, the sort order) box; the user's lambda never does.
  */
opaque type FArray[+A] <: AnyRef = FBase

object FArray:

  inline def empty[A]: FArray[A] = FArrayOps.emptyImpl[A]
  // small-arity overloads avoid the varargs Seq + boxing (hot inside flatMap inners); >3 falls back to varargs
  inline def apply[A](a: A): FArray[A] = FArrayOps.fromValues1[A](a)
  inline def apply[A](a: A, b: A): FArray[A] = FArrayOps.fromValues2[A](a, b)
  inline def apply[A](a: A, b: A, c: A): FArray[A] = FArrayOps.fromValues3[A](a, b, c)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A): FArray[A] = FArrayOps.fromValues4[A](p1, p2, p3, p4)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A): FArray[A] = FArrayOps.fromValues5[A](p1, p2, p3, p4, p5)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A): FArray[A] = FArrayOps.fromValues6[A](p1, p2, p3, p4, p5, p6)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A): FArray[A] = FArrayOps.fromValues7[A](p1, p2, p3, p4, p5, p6, p7)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A): FArray[A] = FArrayOps.fromValues8[A](p1, p2, p3, p4, p5, p6, p7, p8)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A): FArray[A] = FArrayOps.fromValues9[A](p1, p2, p3, p4, p5, p6, p7, p8, p9)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A): FArray[A] =
    FArrayOps.fromValues10[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A): FArray[A] =
    FArrayOps.fromValues11[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A): FArray[A] =
    FArrayOps.fromValues12[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A, p13: A): FArray[A] =
    FArrayOps.fromValues13[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A, p13: A, p14: A): FArray[A] =
    FArrayOps.fromValues14[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A, p13: A, p14: A, p15: A): FArray[A] =
    FArrayOps.fromValues15[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A, p13: A, p14: A, p15: A, p16: A): FArray[A] =
    FArrayOps.fromValues16[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A, p13: A, p14: A, p15: A, p16: A, p17: A)
      : FArray[A] = FArrayOps.fromValues17[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
  inline def apply[A](p1: A, p2: A, p3: A, p4: A, p5: A, p6: A, p7: A, p8: A, p9: A, p10: A, p11: A, p12: A, p13: A, p14: A, p15: A, p16: A, p17: A, p18: A)
      : FArray[A] = FArrayOps.fromValues18[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A
  ): FArray[A] = FArrayOps.fromValues19[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A
  ): FArray[A] = FArrayOps.fromValues20[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A
  ): FArray[A] = FArrayOps.fromValues21[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A
  ): FArray[A] = FArrayOps.fromValues22[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A
  ): FArray[A] = FArrayOps.fromValues23[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A
  ): FArray[A] = FArrayOps.fromValues24[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A
  ): FArray[A] = FArrayOps.fromValues25[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A
  ): FArray[A] =
    FArrayOps.fromValues26[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A,
      p27: A
  ): FArray[A] =
    FArrayOps.fromValues27[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A,
      p27: A,
      p28: A
  ): FArray[A] =
    FArrayOps.fromValues28[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A,
      p27: A,
      p28: A,
      p29: A
  ): FArray[A] = FArrayOps
    .fromValues29[A](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29)
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A,
      p27: A,
      p28: A,
      p29: A,
      p30: A
  ): FArray[A] = FArrayOps.fromValues30[A](
    p1,
    p2,
    p3,
    p4,
    p5,
    p6,
    p7,
    p8,
    p9,
    p10,
    p11,
    p12,
    p13,
    p14,
    p15,
    p16,
    p17,
    p18,
    p19,
    p20,
    p21,
    p22,
    p23,
    p24,
    p25,
    p26,
    p27,
    p28,
    p29,
    p30
  )
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A,
      p27: A,
      p28: A,
      p29: A,
      p30: A,
      p31: A
  ): FArray[A] = FArrayOps.fromValues31[A](
    p1,
    p2,
    p3,
    p4,
    p5,
    p6,
    p7,
    p8,
    p9,
    p10,
    p11,
    p12,
    p13,
    p14,
    p15,
    p16,
    p17,
    p18,
    p19,
    p20,
    p21,
    p22,
    p23,
    p24,
    p25,
    p26,
    p27,
    p28,
    p29,
    p30,
    p31
  )
  inline def apply[A](
      p1: A,
      p2: A,
      p3: A,
      p4: A,
      p5: A,
      p6: A,
      p7: A,
      p8: A,
      p9: A,
      p10: A,
      p11: A,
      p12: A,
      p13: A,
      p14: A,
      p15: A,
      p16: A,
      p17: A,
      p18: A,
      p19: A,
      p20: A,
      p21: A,
      p22: A,
      p23: A,
      p24: A,
      p25: A,
      p26: A,
      p27: A,
      p28: A,
      p29: A,
      p30: A,
      p31: A,
      p32: A
  ): FArray[A] = FArrayOps.fromValues32[A](
    p1,
    p2,
    p3,
    p4,
    p5,
    p6,
    p7,
    p8,
    p9,
    p10,
    p11,
    p12,
    p13,
    p14,
    p15,
    p16,
    p17,
    p18,
    p19,
    p20,
    p21,
    p22,
    p23,
    p24,
    p25,
    p26,
    p27,
    p28,
    p29,
    p30,
    p31,
    p32
  )
  // Larger literal arities go through applyMacro, which builds the typed backing array with unrolled
  // primitive stores and wraps it into the leaf node directly (no varargs Seq, no boxing).
  inline def apply[A](as: A*): FArray[A] = ${ FArrayMacros.applyMacro[A]('as) }
  inline def tabulate[A](n: Int)(inline f: Int => A): FArray[A] = FArrayOps.tabulateImpl[A](n)(f)
  inline def fromArray[A](as: Array[A]): FArray[A] = FArrayOps.fromArrayImpl[A](as)
  inline def fromIterable[A](it: Iterable[A]): FArray[A] = FArrayOps.applyImpl[A](it.toSeq)

  def range(start: Int, end: Int, step: Int = 1): FArray[Int] =
    require(step != 0, "step cannot be 0")
    val count =
      if start == end then 0
      else
        val diff = end.toLong - start.toLong
        require((diff > 0) == (step > 0), "range start/end direction does not match step sign")
        val c = diff / step
        (if diff % step != 0 then c + 1 else c).toInt
    if count == 0 then Empty.INSTANCE
    else if count == 1 then new IntOne(start)
    else new RangeNode(start, step, count)

  // ---- factory methods mirroring the List companion (IterableFactory) ----
  /** build from any IterableOnce (Iterator / View / Iterable), like List.from. */
  inline def from[A](it: IterableOnce[A]): FArray[A] = FArrayOps.applyImpl[A](it.iterator.toSeq)

  /** n copies of elem; elem is re-evaluated per element (like List.fill). */
  inline def fill[A](n: Int)(inline elem: A): FArray[A] = FArray.tabulate(n)(_ => elem)

  /** start, f(start), f(f(start)), … — len elements. */
  inline def iterate[A](start: A, len: Int)(inline f: A => A): FArray[A] =
    if len <= 0 then FArray.empty[A]
    else {
      var cur = start;
      FArray.tabulate(len) { i =>
        val r = cur; if i + 1 < len then cur = f(cur); r
      }
    }

  /** concatenate several FArrays; each `++` is an O(1) Concat node. */
  inline def concat[A](xss: FArray[A]*): FArray[A] = {
    var acc = FArray.empty[A]
    var i = 0
    while i < xss.length do { acc = acc ++ xss(i); i += 1 }
    acc
  }

  /** generate from a seed until f returns None, like List.unfold. */
  inline def unfold[A, S](init: S)(inline f: S => Option[(A, S)]): FArray[A] = {
    val b = scala.collection.mutable.ArrayBuffer.empty[A]
    var s = init
    var go = true
    while go do f(s) match { case Some((a, s2)) => b += a; s = s2; case None => go = false }
    FArray.fromIterable(b)
  }

  extension [A](xs: FArray[A])
    // ---- shape ----
    def length: Int = xs.length
    def size: Int = xs.length
    def isEmpty: Boolean = xs.length == 0
    def nonEmpty: Boolean = xs.length > 0
    def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
    def lengthIs: Int = xs.length
    def sizeIs: Int = xs.length
    def indices: Range = 0 until xs.length
    def isDefinedAt(i: Int): Boolean = i >= 0 && i < xs.length

    // ---- structural (tree-aware FBase virtuals) ----
    def take(n: Int): FArray[A] = xs.take(n)
    def drop(n: Int): FArray[A] = xs.drop(n)
    def takeRight(n: Int): FArray[A] = xs.drop(xs.length - (if n < 0 then 0 else n))
    def dropRight(n: Int): FArray[A] = xs.take(xs.length - (if n < 0 then 0 else n))
    def slice(from: Int, until: Int): FArray[A] = xs.slice(from, until)
    def reverse: FArray[A] = xs.reverse
    def init: FArray[A] = xs.init
    def tail: FArray[A] = xs.drop(1)
    def splitAt(n: Int): (FArray[A], FArray[A]) = (xs.take(n), xs.drop(n))
    def ++[B >: A](that: FArray[B]): FArray[B] = xs.concat(that)
    def :::[B >: A](prefix: FArray[B]): FArray[B] = (xs: FBase).concat(prefix)
    def reverse_:::[B >: A](prefix: FArray[B]): FArray[B] = (xs.reverse: FBase).concat(prefix)

    // ---- specialized element ops (lambda inlined, unboxed) ----
    inline def apply(i: Int): A = FArrayOps.applyAtImpl[A](xs, i)
    inline def head: A = FArrayOps.applyAtImpl[A](xs, 0)
    inline def last: A = FArrayOps.applyAtImpl[A](xs, xs.length - 1)
    def headOption: Option[A] = if xs.length == 0 then None else Some((xs: FBase).applyBoxed(0).asInstanceOf[A])
    def lastOption: Option[A] = if xs.length == 0 then None else Some((xs: FBase).applyBoxed(xs.length - 1).asInstanceOf[A])

    /** boxed element read; works for an abstract A (no specialization). Used by ListSyntax's extractor. */
    private[farray] def boxedAt(i: Int): A = (xs: FBase).applyBoxed(i).asInstanceOf[A]
    /** enter a fused pipeline: `xs.fuse.map(..).filter(..).run` compiles to one unboxed loop. */
    inline def fuse: Fuse[A] = new Fuse[A](xs)
    inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = FArrayOps.foldLeftImpl[A, Z](xs, z)(op)
    inline def foreach(inline f: A => Unit): Unit = FArrayOps.foreachImpl[A](xs)(f)

    /** breakable push: applies `f` to each element while it returns true; stops as soon as `f` returns false. */
    inline def foreachWhile(inline f: A => Boolean): Unit = FArrayOps.foreachWhileImpl[A](xs)(f)
    inline def map[B](inline f: A => B): FArray[B] = FArrayOps.mapImpl[A, B](xs)(f)
    inline def filter(inline p: A => Boolean): FArray[A] = FArrayOps.filterImpl[A](xs)(p)
    inline def filterNot(inline p: A => Boolean): FArray[A] = FArrayOps.filterNotImpl[A](xs)(p)
    inline def contains(elem: A): Boolean = FArrayOps.containsImpl[A](xs, elem)
    inline def flatMap[B](inline f: A => FArray[B]): FArray[B] = FArrayOps.flatMapImpl[A, B](xs)(a => f(a).asInstanceOf[FBase])
    inline def updated[B >: A](index: Int, elem: B): FArray[B] = FArrayOps.updatedImpl[A, B](xs, index, elem)
    inline def :+[B >: A](elem: B): FArray[B] = FArrayOps.appendImpl[A, B](xs, elem)

    inline def foldRight[Z](z: Z)(inline op: (A, Z) => Z): Z = FArrayOps.foldRightImpl[A, Z](xs, z)(op)
    inline def fold[B >: A](z: B)(inline op: (B, B) => B): B = xs.foldLeft[B](z)((acc, a) => op(acc, a))
    inline def reduceLeft[B >: A](inline op: (B, A) => B): B =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else xs.drop(1).foldLeft[B](FArrayOps.applyAtImpl[A](xs, 0))(op)
    inline def reduce[B >: A](inline op: (B, B) => B): B =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else xs.drop(1).foldLeft[B](FArrayOps.applyAtImpl[A](xs, 0))((acc, a) => op(acc, a))
    inline def reduceRight[B >: A](inline op: (A, B) => B): B =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0)
      else
        // seed from the last element, foldRight the prefix [0, n-1) — both ride the new Bwd reduce engine
        // (take(n-1) is an O(1) SliceNode the engine traverses unboxed; no skip-flag, no Unit-Z side closure).
        val n = xs.length
        xs.take(n - 1).foldRight[B](FArrayOps.applyAtImpl[A](xs, n - 1))(op)
    inline def reduceOption[B >: A](inline op: (B, B) => B): Option[B] =
      if xs.length == 0 then None else Some(xs.reduce[B](op))

    inline def count(inline p: A => Boolean): Int = FArrayOps.countImpl[A](xs)(p)
    inline def exists(inline p: A => Boolean): Boolean = FArrayOps.existsImpl[A](xs)(p)
    inline def forall(inline p: A => Boolean): Boolean = FArrayOps.forallImpl[A](xs)(p)
    inline def find(inline p: A => Boolean): Option[A] = FArrayOps.findImpl[A](xs)(p)
    inline def indexWhere(inline p: A => Boolean): Int = FArrayOps.indexWhereImpl[A](xs, 0)(p)
    inline def indexWhere(inline p: A => Boolean, from: Int): Int = FArrayOps.indexWhereImpl[A](xs, from)(p)
    inline def indexOf[B >: A](elem: B): Int = FArrayOps.indexOfImpl[A, B](xs, elem, 0)
    inline def indexOf[B >: A](elem: B, from: Int): Int = FArrayOps.indexOfImpl[A, B](xs, elem, from)
    inline def maxBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0)
      else
        var best: A = FArrayOps.applyAtImpl[A](xs, 0); var bk: B = f(best)
        xs.foreach((a: A) => { val k = f(a); if ord.gt(k, bk) then { best = a; bk = k } }); best
    inline def minBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0)
      else
        var best: A = FArrayOps.applyAtImpl[A](xs, 0); var bk: B = f(best)
        xs.foreach((a: A) => { val k = f(a); if ord.lt(k, bk) then { best = a; bk = k } }); best
    // min/max: a Fwd Reduce seeded from element 0 — the running best folds via the new reduce engine (unboxed
    // self-kind acc for a primitive FArray). Re-folding element 0 against itself is idempotent (gt/lt false),
    // so seeding-and-folding-all matches List.max/min exactly.
    inline def max[B >: A](using ord: Ordering[B]): A =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0)
      else xs.foldLeft[B](FArrayOps.applyAtImpl[A](xs, 0))((best, a) => if ord.gt(a, best) then a else best).asInstanceOf[A]
    inline def min[B >: A](using ord: Ordering[B]): A =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0)
      else xs.foldLeft[B](FArrayOps.applyAtImpl[A](xs, 0))((best, a) => if ord.lt(a, best) then a else best).asInstanceOf[A]
    inline def corresponds[B](that: FArray[B])(inline p: (A, B) => Boolean): Boolean =
      xs.length == that.length && FArrayOps.matchAll2Impl[A, B](xs, 0, that, xs.length)(p)
    inline def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = FArrayOps.collectFirstImpl[A, B](xs)(pf)

    // ---- FArray-building lambda ops: lambda applied via filter/take/drop/foreach (unboxed) ----
    inline def takeWhile(inline p: A => Boolean): FArray[A] = xs.take(FArrayOps.prefixLengthImpl[A](xs)(p))
    inline def dropWhile(inline p: A => Boolean): FArray[A] = xs.drop(FArrayOps.prefixLengthImpl[A](xs)(p))
    inline def span(inline p: A => Boolean): (FArray[A], FArray[A]) =
      val n = FArrayOps.prefixLengthImpl[A](xs)(p); (xs.take(n), xs.drop(n))
    inline def partition(inline p: A => Boolean): (FArray[A], FArray[A]) =
      val t = FArrayOps.partitionImpl[A](xs)(p); (t._1.asInstanceOf[FArray[A]], t._2.asInstanceOf[FArray[A]])
    inline def collect[B](pf: PartialFunction[A, B]): FArray[B] = FArrayOps.collectImpl[A, B](xs)(pf).asInstanceOf[FArray[B]]
    inline def distinct: FArray[A] =
      if xs.length <= 1 then xs
      else { val seen = scala.collection.mutable.HashSet.empty[Any]; xs.filter(a => seen.add(a)) }
    inline def distinctBy[B](inline f: A => B): FArray[A] =
      if xs.length <= 1 then xs
      else { val seen = scala.collection.mutable.HashSet.empty[B]; xs.filter(a => seen.add(f(a))) }
    inline def zip[B](that: FArray[B]): FArray[(A, B)] = FArrayOps.zipImpl[A, B](xs, that)
    inline def zipWithIndex: FArray[(A, Int)] = FArrayOps.zipWithIndexImpl[A](xs)
    inline def sortWith(inline lt: (A, A) => Boolean): FArray[A] = FArrayOps.sortWithImpl[A](xs)(lt)
    inline def sortBy[B](inline f: A => B)(using ord: Ordering[B]): FArray[A] = FArrayOps.sortByImpl[A, B](xs)(f)
    inline def sorted[B >: A](using ord: Ordering[B]): FArray[A] = FArrayOps.sortedImpl[A, B](xs)
    // One unboxed pass: each group's elements land directly in their final primitive array (no List builder,
    // no second per-element rebuild, HashMap not LinkedHashMap — groupBy promises no encounter order, matching
    // List/Vector). See FArrayOps.groupByImpl / groupMapAcc.
    inline def groupBy[K](inline f: A => K): Map[K, FArray[A]] =
      FArrayOps.groupByImpl[A, K](xs)(f).asInstanceOf[Map[K, FArray[A]]]
    inline def groupMap[K, B](inline key: A => K)(inline f: A => B): Map[K, FArray[B]] =
      val acc = FArrayOps.groupMapAcc[K, B]
      xs.foreach(a => acc.add(key(a), f(a)))
      acc.result.asInstanceOf[Map[K, FArray[B]]]
    inline def partitionMap[A1, A2](inline f: A => Either[A1, A2]): (FArray[A1], FArray[A2]) =
      val t = FArrayOps.partitionMapImpl[A, A1, A2](xs)(f); (t._1.asInstanceOf[FArray[A1]], t._2.asInstanceOf[FArray[A2]])

    // flattening cursor: O(n) over trees (not O(n·depth)), unboxed leaf reads, reports knownSize.
    inline def iterator: Iterator[A] = FArrayOps.iteratorImpl[A](xs)
    inline def reverseIterator: Iterator[A] = FArrayOps.iteratorImpl[A](xs.reverse)

    // ---- conversions / structure ops (specialized unboxed traversal into the target builder) ----
    // toList is used generically (abstract A, e.g. test harness) so it can't be inline; List boxes anyway.
    inline def toList: List[A] = { val b = List.newBuilder[A]; xs.foreach(a => b += a); b.result() }
    inline def toVector: Vector[A] = { val b = Vector.newBuilder[A]; xs.foreach(a => b += a); b.result() }
    def toSeq: Seq[A] = new FArraySeq[A](xs)
    inline def toSet[B >: A]: Set[B] =
      if xs.length == 0 then Set.empty[B] else { val b = Set.newBuilder[B]; xs.foreach(a => b += a); b.result() }
    inline def toMap[K, V](using ev: A <:< (K, V)): Map[K, V] = { val b = Map.newBuilder[K, V]; xs.foreach(a => b += ev(a)); b.result() }
    inline def toArray[B >: A](using ct: ClassTag[B]): Array[B] = FArrayOps.toArrayImpl[A, B](xs)(ct)
    inline def mkString(start: String, sep: String, end: String): String = FArrayOps.mkStringImpl[A](xs, start, sep, end)
    inline def mkString(sep: String): String = xs.mkString("", sep, "")
    inline def mkString: String = xs.mkString("", "", "")
    inline def startsWith[B >: A](that: FArray[B]): Boolean =
      that.length <= xs.length && FArrayOps.matchAll2Impl[A, B](xs, 0, that, that.length)((a, b) => a == b)
    inline def endsWith[B >: A](that: FArray[B]): Boolean =
      val off = xs.length - that.length
      off >= 0 && FArrayOps.matchAll2Impl[A, B](xs, off, that, that.length)((a, b) => a == b)

    inline def padTo[B >: A](len: Int, elem: B): FArray[B] = FArrayOps.padToImpl[A, B](xs, len, elem)
    inline def diff[B >: A](that: FArray[B]): FArray[A] =
      if xs.length == 0 || that.length == 0 then xs
      else
        val rem = scala.collection.mutable.HashMap.empty[Any, Int]
        that.foreach(b => rem.update(b, rem.getOrElse(b, 0) + 1))
        xs.filter { a => rem.getOrElse(a, 0) match { case 0 => true; case c => rem.update(a, c - 1); false } }
    inline def intersect[B >: A](that: FArray[B]): FArray[A] =
      if xs.length == 0 then xs
      else if that.length == 0 then FArray.empty[A]
      else
        val keep = scala.collection.mutable.HashMap.empty[Any, Int]
        that.foreach(b => keep.update(b, keep.getOrElse(b, 0) + 1))
        xs.filter { a => keep.getOrElse(a, 0) match { case 0 => false; case c => keep.update(a, c - 1); true } }
    inline def lazyZip[B, C](ys: FArray[B], zs: FArray[C]): FArray[(A, B, C)] =
      val n = math.min(xs.length, math.min(ys.length, zs.length))
      FArray.tabulate(n)(i => (FArrayOps.applyAtImpl[A](xs, i), FArrayOps.applyAtImpl[B](ys, i), FArrayOps.applyAtImpl[C](zs, i)))
    inline def unzip[A1, A2](using ev: A <:< (A1, A2)): (FArray[A1], FArray[A2]) =
      FArrayOps.unzipImpl[A, A1, A2](xs)(ev)
    inline def unzip3[A1, A2, A3](using ev: A <:< (A1, A2, A3)): (FArray[A1], FArray[A2], FArray[A3]) =
      FArrayOps.unzip3Impl[A, A1, A2, A3](xs)(ev)
    inline def flatten[B](using ev: A <:< FArray[B]): FArray[B] =
      FArrayOps.flatMapImpl[A, B](xs)(a => ev(a).asInstanceOf[FBase])
    inline def transpose[B](using ev: A <:< FArray[B]): FArray[FArray[B]] =
      val n = xs.length
      if n == 0 then FArray.empty[FArray[B]]
      else
        val inners = new Array[Object](n)
        var k = 0; while k < n do { inners(k) = ev(xs.apply(k)).asInstanceOf[Object]; k += 1 }
        val cols = inners(0).asInstanceOf[FBase].length
        // outer is Ref (FArrays, built directly); each column is tabulated in B's own kind -> unboxed for primitive B
        val outer = new Array[Object](cols)
        var c = 0
        while c < cols do
          val cc = c
          outer(c) = (FArray.tabulate[B](n)(r => FArrayOps.applyAtImpl[B](inners(r).asInstanceOf[FBase], cc)): FArray[B]).asInstanceOf[Object]
          c += 1
        val res: FBase = if cols == 0 then Empty.INSTANCE else if cols == 1 then new RefOne(outer(0)) else new RefArr(outer, cols)
        res.asInstanceOf[FArray[FArray[B]]]
    inline def mapConserve(inline f: A => A): FArray[A] = FArrayOps.mapConserveImpl[A](xs)(f)

    // ---- composed from specialised primitives: no boxed storage, inline lambdas thread through ----
    inline def reduceLeftOption[B >: A](inline op: (B, A) => B): Option[B] =
      if xs.length == 0 then None else Some(xs.reduceLeft[B](op))
    inline def reduceRightOption[B >: A](inline op: (A, B) => B): Option[B] =
      if xs.length == 0 then None else Some(xs.reduceRight[B](op))
    inline def segmentLength(inline p: A => Boolean): Int = FArrayOps.segmentLengthImpl[A](xs, 0)(p)
    inline def segmentLength(inline p: A => Boolean, from: Int): Int = FArrayOps.segmentLengthImpl[A](xs, from)(p)
    // backward leaf scan (empty-body, predicate-in-condition — matches a native Array's lastIndexWhere speed);
    // the short-circuit Bwd traverser walks last-to-first with no ReverseNode + remap round-trip.
    inline def lastIndexWhere(inline p: A => Boolean): Int = FArrayOps.lastIndexWhereImpl[A](xs, xs.length - 1)(p)
    inline def lastIndexWhere(inline p: A => Boolean, end: Int): Int = FArrayOps.lastIndexWhereImpl[A](xs, end)(p)
    inline def lastIndexOf[B >: A](elem: B): Int = FArrayOps.lastIndexOfImpl[A, B](xs, elem, xs.length - 1)
    inline def lastIndexOf[B >: A](elem: B, end: Int): Int = FArrayOps.lastIndexOfImpl[A, B](xs, elem, end)
    inline def sameElements[B >: A](that: FArray[B]): Boolean =
      val n = xs.length
      if n != that.length then false
      else if n == 0 then true
      else if n == 1 then FArrayOps.applyAtImpl[A](xs, 0) == FArrayOps.applyAtImpl[B](that, 0)
      else FArrayOps.matchAll2Impl[A, B](xs, 0, that, n)((a, b) => a == b)
    inline def indexOfSlice[B >: A](that: FArray[B]): Int =
      val m = that.length; val n = xs.length
      if m == 0 then 0
      else if m == 1 then xs.indexOf[B](FArrayOps.applyAtImpl[B](that, 0))
      else
        var i = 0; var res = -1
        while i + m <= n && res < 0 do if FArrayOps.matchAll2Impl[A, B](xs, i, that, m)((a, b) => a == b) then res = i else i += 1
        res
    inline def lastIndexOfSlice[B >: A](that: FArray[B]): Int =
      val m = that.length; val n = xs.length
      if m == 0 then n
      else if m == 1 then xs.lastIndexOf[B](FArrayOps.applyAtImpl[B](that, 0))
      else
        var i = n - m; var res = -1
        while i >= 0 && res < 0 do if FArrayOps.matchAll2Impl[A, B](xs, i, that, m)((a, b) => a == b) then res = i else i -= 1
        res
    inline def containsSlice[B >: A](that: FArray[B]): Boolean = indexOfSlice(that) >= 0
    def grouped(size: Int): Iterator[FArray[A]] =
      val n = xs.length; val step = if size < 1 then 1 else size
      Iterator.range(0, n, step).map(i => xs.slice(i, math.min(i + step, n)))
    def sliding(size: Int, step: Int): Iterator[FArray[A]] =
      val n = xs.length; val sz = if size < 1 then 1 else size; val st = if step < 1 then 1 else step
      if n == 0 then Iterator.empty
      else
        val w = if n <= sz then 1 else (n - sz + st - 1) / st + 1 // ceil((n-sz)/st)+1 windows, matching List.sliding
        Iterator.range(0, w).map(i => xs.slice(i * st, math.min(i * st + sz, n)))
    def sliding(size: Int): Iterator[FArray[A]] = sliding(size, 1)
    def inits: Iterator[FArray[A]] = Iterator.range(xs.length, -1, -1).map(i => xs.take(i))
    def tails: Iterator[FArray[A]] = Iterator.range(0, xs.length + 1).map(i => xs.drop(i))
    def patch[B >: A](from: Int, that: FArray[B], replaced: Int): FArray[B] =
      val f = if from < 0 then 0 else from
      xs.take(f) ++ that ++ xs.drop(f + (if replaced < 0 then 0 else replaced))
    def toIndexedSeq: IndexedSeq[A] = new FArraySeq[A](xs)
    inline def copyToArray[B >: A](dest: Array[B], start: Int, len: Int): Int = FArrayOps.copyToArrayImpl[A, B](xs, dest, start, len)
    // Unboxed: input materialized ONCE to its primitive backing array; each result is generated by indexing
    // into it and writing a fresh same-kind leaf (Int input -> IntArr results, never boxed). Order + distinct
    // handling for equal elements match List exactly (mirrors SeqOps.{CombinationsItr,PermutationsItr}).
    inline def combinations(k: Int): Iterator[FArray[A]] = FArrayOps.combinationsImpl[A](xs, k).asInstanceOf[Iterator[FArray[A]]]
    inline def permutations: Iterator[FArray[A]] = FArrayOps.permutationsImpl[A](xs).asInstanceOf[Iterator[FArray[A]]]
    inline def sum[B >: A](using num: Numeric[B]): B = FArrayOps.sumImpl[A, B](xs)
    inline def product[B >: A](using num: Numeric[B]): B = FArrayOps.productImpl[A, B](xs)
    inline def scanLeft[B](z: B)(inline op: (B, A) => B): FArray[B] = FArrayOps.scanLeftImpl[A, B](xs, z)(op)
    inline def scan[B >: A](z: B)(inline op: (B, B) => B): FArray[B] = xs.scanLeft[B](z)((acc, a) => op(acc, a))
    inline def scanRight[B](z: B)(inline op: (A, B) => B): FArray[B] = FArrayOps.scanRightImpl[A, B](xs, z)(op)

  extension [A](elem: A)
    inline def +:(xs: FArray[A]): FArray[A] = FArrayOps.prependImpl[A](elem, xs)
    inline def ::(xs: FArray[A]): FArray[A] = FArrayOps.prependImpl[A](elem, xs)
