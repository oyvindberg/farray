package farray

import scala.reflect.ClassTag

/**
 * `FArray` — opaque type over the sealed core hierarchy [[FBase]]. Every lambda-taking op is `inline`
 * with an `inline` function param and applies the lambda through the specialized `foldLeft`/`foreach`/
 * `map`/`filter`/`take`/`drop` machinery — so the function inlines and primitives stay unboxed, exactly
 * like `map`. Only inherently-materializing structures (the Map/Set behind `groupBy`/`distinct`, the
 * sort order) box; the user's lambda never does.
 */
opaque type FArray[+A] <: AnyRef = FBase

object FArray:

  inline def empty[A]: FArray[A] = FArrayOps.emptyImpl[A]
  inline def apply[A](as: A*): FArray[A] = FArrayOps.applyImpl[A](as)
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
    new RangeNode(start, step, count)

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
    inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = FArrayOps.foldLeftImpl[A, Z](xs, z)(op)
    inline def foreach(inline f: A => Unit): Unit = FArrayOps.foreachImpl[A](xs)(f)
    inline def map[B](inline f: A => B): FArray[B] = FArrayOps.mapImpl[A, B](xs)(f)
    inline def filter(inline p: A => Boolean): FArray[A] = FArrayOps.filterImpl[A](xs)(p)
    inline def filterNot(inline p: A => Boolean): FArray[A] = FArrayOps.filterImpl[A](xs)(a => !p(a))
    inline def contains(elem: A): Boolean = FArrayOps.containsImpl[A](xs, elem)
    inline def flatMap[B](inline f: A => FArray[B]): FArray[B] = FArrayOps.flatMapImpl[A, B](xs)(a => f(a).asInstanceOf[FBase])
    inline def updated[B >: A](index: Int, elem: B): FArray[B] = FArrayOps.updatedImpl[A, B](xs, index, elem)
    inline def :+[B >: A](elem: B): FArray[B] = FArrayOps.appendImpl[A, B](xs, elem)

    inline def foldRight[Z](z: Z)(inline op: (A, Z) => Z): Z =
      xs.reverse.foldLeft(z)((acc, a) => op(a, acc))
    inline def fold[B >: A](z: B)(inline op: (B, B) => B): B = xs.foldLeft[B](z)((acc, a) => op(acc, a))
    inline def reduceLeft[B >: A](inline op: (B, A) => B): B = xs.drop(1).foldLeft[B](FArrayOps.applyAtImpl[A](xs, 0))(op)
    inline def reduce[B >: A](inline op: (B, B) => B): B = xs.drop(1).foldLeft[B](FArrayOps.applyAtImpl[A](xs, 0))((acc, a) => op(acc, a))
    inline def reduceRight[B >: A](inline op: (A, B) => B): B =
      xs.dropRight(1).foldRight[B](FArrayOps.applyAtImpl[A](xs, xs.length - 1))(op)
    inline def reduceOption[B >: A](inline op: (B, B) => B): Option[B] =
      if xs.length == 0 then None else Some(xs.reduce[B](op))

    inline def count(inline p: A => Boolean): Int =
      var n = 0; xs.foreach(a => if p(a) then n += 1); n
    inline def exists(inline p: A => Boolean): Boolean = FArrayOps.existsImpl[A](xs)(p)
    inline def forall(inline p: A => Boolean): Boolean = FArrayOps.forallImpl[A](xs)(p)
    inline def find(inline p: A => Boolean): Option[A] = FArrayOps.findImpl[A](xs)(p)
    inline def indexWhere(inline p: A => Boolean): Int = FArrayOps.indexWhereImpl[A](xs)(p)
    inline def indexOf[B >: A](elem: B): Int = FArrayOps.indexOfImpl[A, B](xs, elem)
    inline def maxBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      var best: A = FArrayOps.applyAtImpl[A](xs, 0); var bk: B = f(best)
      xs.drop(1).foreach((a: A) => { val k = f(a); if ord.gt(k, bk) then { best = a; bk = k } }); best
    inline def minBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      var best: A = FArrayOps.applyAtImpl[A](xs, 0); var bk: B = f(best)
      xs.drop(1).foreach((a: A) => { val k = f(a); if ord.lt(k, bk) then { best = a; bk = k } }); best
    inline def max[B >: A](using ord: Ordering[B]): A =
      var best: B = FArrayOps.applyAtImpl[A](xs, 0); xs.drop(1).foreach((a: A) => if ord.gt(a, best) then best = a); best.asInstanceOf[A]
    inline def min[B >: A](using ord: Ordering[B]): A =
      var best: B = FArrayOps.applyAtImpl[A](xs, 0); xs.drop(1).foreach((a: A) => if ord.lt(a, best) then best = a); best.asInstanceOf[A]
    inline def corresponds[B](that: FArray[B])(inline p: (A, B) => Boolean): Boolean =
      if xs.length != that.length then false
      else
        var ok = true; var i = 0
        while ok && i < xs.length do { if !p(FArrayOps.applyAtImpl[A](xs, i), FArrayOps.applyAtImpl[B](that, i)) then ok = false; i += 1 }
        ok
    inline def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = FArrayOps.collectFirstImpl[A, B](xs)(pf)

    // ---- FArray-building lambda ops: lambda applied via filter/take/drop/foreach (unboxed) ----
    inline def takeWhile(inline p: A => Boolean): FArray[A] = xs.take(FArrayOps.prefixLengthImpl[A](xs)(p))
    inline def dropWhile(inline p: A => Boolean): FArray[A] = xs.drop(FArrayOps.prefixLengthImpl[A](xs)(p))
    inline def span(inline p: A => Boolean): (FArray[A], FArray[A]) =
      val n = FArrayOps.prefixLengthImpl[A](xs)(p); (xs.take(n), xs.drop(n))
    inline def partition(inline p: A => Boolean): (FArray[A], FArray[A]) = (xs.filter(p), xs.filterNot(p))
    inline def collect[B](pf: PartialFunction[A, B]): FArray[B] = xs.filter(a => pf.isDefinedAt(a)).map(a => pf(a))
    inline def distinct: FArray[A] =
      val seen = scala.collection.mutable.HashSet.empty[Any]; xs.filter(a => seen.add(a))
    inline def distinctBy[B](inline f: A => B): FArray[A] =
      val seen = scala.collection.mutable.HashSet.empty[B]; xs.filter(a => seen.add(f(a)))
    inline def zip[B](that: FArray[B]): FArray[(A, B)] =
      val n = if xs.length < that.length then xs.length else that.length
      FArray.tabulate(n)(i => (FArrayOps.applyAtImpl[A](xs, i), FArrayOps.applyAtImpl[B](that, i)))
    inline def zipWithIndex: FArray[(A, Int)] =
      FArray.tabulate(xs.length)(i => (FArrayOps.applyAtImpl[A](xs, i), i))
    inline def sortWith(inline lt: (A, A) => Boolean): FArray[A] = FArrayOps.sortWithImpl[A](xs)(lt)
    inline def sortBy[B](inline f: A => B)(using ord: Ordering[B]): FArray[A] = FArrayOps.sortByImpl[A, B](xs)(f)
    inline def sorted[B >: A](using ord: Ordering[B]): FArray[A] = FArrayOps.sortedImpl[A, B](xs)
    inline def groupBy[K](inline f: A => K): Map[K, FArray[A]] =
      val m = scala.collection.mutable.LinkedHashMap.empty[K, scala.collection.mutable.Builder[A, List[A]]]
      xs.foreach(a => m.getOrElseUpdate(f(a), List.newBuilder[A]) += a)
      m.iterator.map((k, b) => k -> FArray.fromIterable(b.result())).toMap
    inline def groupMap[K, B](inline key: A => K)(inline f: A => B): Map[K, FArray[B]] =
      val m = scala.collection.mutable.LinkedHashMap.empty[K, scala.collection.mutable.Builder[B, List[B]]]
      xs.foreach(a => m.getOrElseUpdate(key(a), List.newBuilder[B]) += f(a))
      m.iterator.map((k, b) => k -> FArray.fromIterable(b.result())).toMap
    inline def partitionMap[A1, A2](inline f: A => Either[A1, A2]): (FArray[A1], FArray[A2]) =
      val ls = List.newBuilder[A1]; val rs = List.newBuilder[A2]
      xs.foreach(a => f(a) match { case Left(l) => ls += l; case Right(r) => rs += r })
      (FArray.fromIterable(ls.result()), FArray.fromIterable(rs.result()))

    def iterator: Iterator[A] =
      val core: FBase = xs
      new Iterator[A]:
        private var i = 0
        def hasNext: Boolean = i < core.length
        def next(): A = { val r = core.applyBoxed(i).asInstanceOf[A]; i += 1; r }
    def reverseIterator: Iterator[A] =
      val core: FBase = xs
      new Iterator[A]:
        private var i = core.length - 1
        def hasNext: Boolean = i >= 0
        def next(): A = { val r = core.applyBoxed(i).asInstanceOf[A]; i -= 1; r }

    // ---- conversions / structure ops (specialized unboxed traversal into the target builder) ----
    // toList is used generically (abstract A, e.g. test harness) so it can't be inline; List boxes anyway.
    def toList: List[A] =
      val b = List.newBuilder[A]; val c: FBase = xs; var i = 0
      while i < c.length do { b += c.applyBoxed(i).asInstanceOf[A]; i += 1 }
      b.result()
    inline def toVector: Vector[A] = { val b = Vector.newBuilder[A]; xs.foreach(a => b += a); b.result() }
    inline def toSeq: Seq[A] = xs.toVector
    inline def toSet[B >: A]: Set[B] = { val b = Set.newBuilder[B]; xs.foreach(a => b += a); b.result() }
    inline def toMap[K, V](using ev: A <:< (K, V)): Map[K, V] = { val b = Map.newBuilder[K, V]; xs.foreach(a => b += ev(a)); b.result() }
    inline def toArray[B >: A](using ct: ClassTag[B]): Array[B] =
      val arr = ct.newArray(xs.length); var i = 0; xs.foreach(a => { arr(i) = a; i += 1 }); arr
    inline def mkString(start: String, sep: String, end: String): String =
      val sb = new java.lang.StringBuilder(start); var first = true
      xs.foreach(a => { if first then first = false else sb.append(sep); sb.append(String.valueOf(a.asInstanceOf[Object])) }); sb.append(end).toString
    inline def mkString(sep: String): String = xs.mkString("", sep, "")
    inline def mkString: String = xs.mkString("", "", "")
    inline def startsWith[B >: A](that: FArray[B]): Boolean =
      val m = that.length
      if m > xs.length then false
      else
        var i = 0; var ok = true
        while ok && i < m do { if FArrayOps.applyAtImpl[A](xs, i) != FArrayOps.applyAtImpl[B](that, i) then ok = false; i += 1 }
        ok
    inline def endsWith[B >: A](that: FArray[B]): Boolean =
      val m = that.length; val off = xs.length - m
      if off < 0 then false
      else
        var i = 0; var ok = true
        while ok && i < m do { if FArrayOps.applyAtImpl[A](xs, off + i) != FArrayOps.applyAtImpl[B](that, i) then ok = false; i += 1 }
        ok

    inline def padTo[B >: A](len: Int, elem: B): FArray[B] = FArrayOps.padToImpl[A, B](xs, len, elem)
    inline def diff[B >: A](that: FArray[B]): FArray[A] =
      val rem = scala.collection.mutable.HashMap.empty[Any, Int]
      that.foreach(b => rem.update(b, rem.getOrElse(b, 0) + 1))
      xs.filter { a => rem.getOrElse(a, 0) match { case 0 => true; case c => rem.update(a, c - 1); false } }
    inline def intersect[B >: A](that: FArray[B]): FArray[A] =
      val keep = scala.collection.mutable.HashMap.empty[Any, Int]
      that.foreach(b => keep.update(b, keep.getOrElse(b, 0) + 1))
      xs.filter { a => keep.getOrElse(a, 0) match { case 0 => false; case c => keep.update(a, c - 1); true } }
    inline def lazyZip[B, C](ys: FArray[B], zs: FArray[C]): FArray[(A, B, C)] =
      val n = math.min(xs.length, math.min(ys.length, zs.length))
      FArray.tabulate(n)(i => (FArrayOps.applyAtImpl[A](xs, i), FArrayOps.applyAtImpl[B](ys, i), FArrayOps.applyAtImpl[C](zs, i)))
    inline def unzip[A1, A2](using ev: A <:< (A1, A2)): (FArray[A1], FArray[A2]) =
      (FArray.tabulate(xs.length)(i => ev(FArrayOps.applyAtImpl[A](xs, i))._1),
       FArray.tabulate(xs.length)(i => ev(FArrayOps.applyAtImpl[A](xs, i))._2))
    inline def unzip3[A1, A2, A3](using ev: A <:< (A1, A2, A3)): (FArray[A1], FArray[A2], FArray[A3]) =
      (FArray.tabulate(xs.length)(i => ev(FArrayOps.applyAtImpl[A](xs, i))._1),
       FArray.tabulate(xs.length)(i => ev(FArrayOps.applyAtImpl[A](xs, i))._2),
       FArray.tabulate(xs.length)(i => ev(FArrayOps.applyAtImpl[A](xs, i))._3))
    inline def flatten[B](using ev: A <:< FArray[B]): FArray[B] =
      var acc: FBase = FArrayOps.emptyImpl[B]
      xs.foreach(a => acc = acc.concat(ev(a)))
      acc
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
        new RefArr(outer, cols).asInstanceOf[FArray[FArray[B]]]
    inline def mapConserve(inline f: A => A): FArray[A] =
      var changed = false
      val out = xs.map { a => val b = f(a); if !(b.asInstanceOf[AnyRef] eq a.asInstanceOf[AnyRef]) then changed = true; b }
      if changed then out else xs

  extension [A](elem: A)
    inline def +: (xs: FArray[A]): FArray[A] = FArrayOps.prependImpl[A](elem, xs)
    inline def :: (xs: FArray[A]): FArray[A] = FArrayOps.prependImpl[A](elem, xs)
