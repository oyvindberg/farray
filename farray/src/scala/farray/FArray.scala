package farray

import scala.reflect.ClassTag

//start:opaque-type
/** `FArray` — opaque type over the sealed core hierarchy [[FBase]]. Every lambda-taking op is `inline` with an `inline` function param and applies the lambda
  * through the specialized `foldLeft`/`foreach`/ `map`/`filter`/`take`/`drop` machinery — so the function inlines and primitives stay unboxed, exactly like
  * `map`. Only inherently-materializing structures (the Map/Set behind `groupBy`/`distinct`, the sort order) box; the user's lambda never does.
  */
opaque type FArray[+A] <: AnyRef = FBase
//stop:opaque-type

object FArray:

  inline def empty[A]: FArray[A] = FArrayOps.emptyImpl[A]
  // ONE varargs apply: the macro pattern-matches the literal argument list at compile time and emits
  // the construction directly for EVERY arity — Empty / ${K}One / a typed array filled with unrolled
  // stores wrapped in ${K}Arr — no varargs Seq, no boxing, no overload wall, no runtime size checks.
  // The param is `inline` so the inliner never proxies the varargs behind a Seq val (which would hide
  // the literal list from the macro — and from the fuse macro's flatMap splat). A non-literal spread
  // (`FArray(xs*)`) falls back to the runtime applyImpl.
  inline def apply[A](inline as: A*): FArray[A] = ${ FArrayMacros.applyMacro[A]('as) }
  inline def tabulate[A](n: Int)(inline f: Int => A): FArray[A] = FArrayOps.tabulateImpl[A](n)(f)

  /** an unboxed imperative builder: accumulate with `+=`/`++=`, then `result()`. The element kind is resolved at THIS call site so primitive elements are
    * written without boxing — see [[FBuilder]].
    */
  inline def newBuilder[A]: FBuilder[A] = FBuilder[A]()
  inline def fromArray[A](as: Array[A]): FArray[A] = FArrayOps.fromArrayImpl[A](as)
  inline def fromIterable[A](it: Iterable[A]): FArray[A] = FArrayOps.applyImpl[A](it.toSeq)

  /** wrap a boxed `Array[Object]` (e.g. from the fused topN heap) into an FArray, UNBOXING per element kind so a primitive element type lands in a flat
    * primitive leaf rather than a boxed `RefArr`. Internal — used by the macro.
    */
  inline def fromBoxedArray[A](as: Array[Object]): FArray[A] = FArrayOps.fromBoxedArrayImpl[A](as)

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
    inline def fuse: Fuse[A, Chunks] = new Fuse[A, Chunks](xs)
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
    // reduce* seed from the first/last ELEMENT via the seeded leaf methods — the composed form
    // (drop(1)/take(n-1) + fold) allocated a SliceNode + did an applyAt dispatch per call, which
    // dominated small inputs; only genuine trees compose now (inside the impl's cold arm).
    inline def reduceLeft[B >: A](inline op: (B, A) => B): B =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else FArrayOps.reduceLeftImpl[A, B](xs)(op)
    inline def reduce[B >: A](inline op: (B, B) => B): B =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else FArrayOps.reduceLeftImpl[A, B](xs)((acc, a) => op(acc, a))
    inline def reduceRight[B >: A](inline op: (A, B) => B): B =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else FArrayOps.reduceRightImpl[A, B](xs)(op)
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
    // min/max: a Fwd Reduce seeded from element 0 — the running best folds via the reduce engine (unboxed
    // self-kind acc for a primitive FArray; standard orderings compare RAW, others via ord.gt/lt).
    // Re-folding element 0 against itself is idempotent (gt/lt false), so seeding-and-folding-all
    // matches List.max/min exactly.
    inline def max[B >: A](using ord: Ordering[B]): A =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else FArrayOps.maxImpl[A, B](xs)
    inline def min[B >: A](using ord: Ordering[B]): A =
      if xs.length == 1 then FArrayOps.applyAtImpl[A](xs, 0) else FArrayOps.minImpl[A, B](xs)
    inline def corresponds[B](that: FArray[B])(inline p: (A, B) => Boolean): Boolean =
      xs.length == that.length && FArrayOps.matchAll2Impl[A, B](xs, 0, that, xs.length)(p)
    inline def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = FArrayOps.collectFirstImpl[A, B](xs)(pf)

    // ---- FArray-building lambda ops: lambda applied via filter/take/drop/foreach (unboxed) ----
    inline def takeWhile(inline p: A => Boolean): FArray[A] = xs.take(FArrayOps.prefixLengthImpl[A](xs)(p))
    inline def dropWhile(inline p: A => Boolean): FArray[A] = xs.drop(FArrayOps.prefixLengthImpl[A](xs)(p))
    inline def span(inline p: A => Boolean): (FArray[A], FArray[A]) =
      val n = FArrayOps.prefixLengthImpl[A](xs)(p); (xs.take(n), xs.drop(n))
    // the FBase pair IS the FArray pair inside the opaque scope — re-wrapping into a fresh Tuple2 here
    // was the whole cost of partition on an empty/cheap input (the impl's cached emptyPair never survived).
    inline def partition(inline p: A => Boolean): (FArray[A], FArray[A]) =
      FArrayOps.partitionImpl[A](xs)(p).asInstanceOf[(FArray[A], FArray[A])]
    // a LITERAL pattern match is picked apart at compile time (CollectMacro): pattern+guard become
    // the predicate, the case body the transform, both fed inline to the fused one-pass impl — no
    // PartialFunction object, no boxing. A stored PF falls back to the runtime impl.
    inline def collect[B](inline pf: PartialFunction[A, B]): FArray[B] =
      ${ CollectMacro.impl[A, B]('xs, 'pf) }
    // unboxed per-kind seen-tables (domain/offset bitmaps, position-index probes, F14 ctrl bytes for refs),
    // Scala `==` semantics preserved exactly (NaN never collapses, ±0.0 does), identity return when nothing
    // was duplicated. Replaces the boxed mutable.HashSet[Any] + filter pass.
    inline def distinct: FArray[A] = FArrayOps.distinctImpl[A](xs)
    // same seen-tables as distinct, keyed by an unboxed computed f(a) column; survivor indices gather the
    // ORIGINAL elements (first occurrence per key wins, List semantics); identity return when no key repeats.
    inline def distinctBy[B](inline f: A => B): FArray[A] = FArrayOps.distinctByImpl[A, B](xs)(f)
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
      if xs.length == 1 then
        val e = FArrayOps.applyAtImpl[A](xs, 0)
        Map(key(e) -> (FArrayOps.fromValues1[B](f(e)): FArray[B]))
      else
        val acc = FArrayOps.groupMapAcc[K, B]
        xs.foreach(a => acc.add(key(a), f(a)))
        acc.result.asInstanceOf[Map[K, FArray[B]]]
    inline def partitionMap[A1, A2](inline f: A => Either[A1, A2]): (FArray[A1], FArray[A2]) =
      FArrayOps.partitionMapImpl[A, A1, A2](xs)(f).asInstanceOf[(FArray[A1], FArray[A2])]

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
    // diff/intersect are MULTISET ops (each occurrence in `that` consumes at most one match), so a
    // plain set can't back them — the unboxed backend is FMap[A, Int] when it lands. Interim: tiny
    // inputs (n*m <= 64) skip the boxed HashMap entirely — a nested scan with consume-once flags is
    // both faster there and multiset-correct.
    inline def diff[B >: A](that: FArray[B]): FArray[A] =
      if xs.length == 0 || that.length == 0 then xs
      else if xs.length.toLong * that.length <= 64 then
        val m = that.length
        val used = new Array[Boolean](m)
        xs.filter { a =>
          var i = 0; var hit = -1
          while hit < 0 && i < m do
            if !used(i) && FArrayOps.applyAtImpl[B](that, i) == a then hit = i
            i += 1
          if hit >= 0 then { used(hit) = true; false }
          else true
        }
      else
        val rem = scala.collection.mutable.HashMap.empty[Any, Int]
        that.foreach(b => rem.update(b, rem.getOrElse(b, 0) + 1))
        xs.filter { a => rem.getOrElse(a, 0) match { case 0 => true; case c => rem.update(a, c - 1); false } }
    inline def intersect[B >: A](that: FArray[B]): FArray[A] =
      if xs.length == 0 then xs
      else if that.length == 0 then FArray.empty[A]
      else if xs.length.toLong * that.length <= 64 then
        val m = that.length
        val used = new Array[Boolean](m)
        xs.filter { a =>
          var i = 0; var hit = -1
          while hit < 0 && i < m do
            if !used(i) && FArrayOps.applyAtImpl[B](that, i) == a then hit = i
            i += 1
          if hit >= 0 then { used(hit) = true; true }
          else false
        }
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
    // candidate scan: jump between occurrences of the slice's FIRST element with the short-circuit
    // engine's indexOf (a vectorizable empty-body scan for prims), verify each candidate with ONE
    // matchAll2, resume past it — instead of a matchAll2 call per position (position-stepping paid
    // 0.75-0.78x vs IArray's stdlib KMP at 1k-100k).
    inline def indexOfSlice[B >: A](that: FArray[B]): Int =
      val m = that.length; val n = xs.length
      if m == 0 then 0
      else if m == 1 then xs.indexOf[B](FArrayOps.applyAtImpl[B](that, 0))
      else
        val h = FArrayOps.applyAtImpl[B](that, 0)
        val last = n - m
        var i = 0; var res = -1
        while res < 0 && i <= last do
          val c = FArrayOps.indexOfImpl[A, B](xs, h, i)
          if c < 0 || c > last then i = last + 1
          else if FArrayOps.matchAll2Impl[A, B](xs, c, that, m)((a, b) => a == b) then res = c
          else i = c + 1
        res
    inline def lastIndexOfSlice[B >: A](that: FArray[B]): Int =
      val m = that.length; val n = xs.length
      if m == 0 then n
      else if m == 1 then xs.lastIndexOf[B](FArrayOps.applyAtImpl[B](that, 0))
      else
        val h = FArrayOps.applyAtImpl[B](that, 0)
        var i = n - m; var res = -1
        while res < 0 && i >= 0 do
          val c = FArrayOps.lastIndexOfImpl[A, B](xs, h, i)
          if c < 0 then i = -1
          else if FArrayOps.matchAll2Impl[A, B](xs, c, that, m)((a, b) => a == b) then res = c
          else i = c - 1
        res
    inline def containsSlice[B >: A](that: FArray[B]): Boolean = indexOfSlice(that) >= 0
    def grouped(size: Int): Iterator[FArray[A]] =
      val n = xs.length; val step = if size < 1 then 1 else size
      if n == 0 then Iterator.empty
      else Iterator.range(0, n, step).map(i => xs.slice(i, math.min(i + step, n)))
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
