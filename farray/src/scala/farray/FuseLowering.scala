package farray

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/** The TYPECLASS that gives a source shape its fused terminals. An instance is an object mixing in the capability traits below — each provides a family of
  * terminals as `inline` extension methods on `Fuse[A, S]`, every one funneling into the single abstract [[ShapeLowering.lower]] hook the module implements.
  * The instance is published as a given in the shape's companion (so it rides the implicit scope of `Fuse[A, S]`, no imports needed), declared at its SINGLETON
  * type (extension lookup resolves members on the given's declared type).
  *
  * Consequences: the engine ships no decoder knowledge; a decoder module plugs in as an ordinary given; and a terminal a shape doesn't mix in is simply NOT A
  * MEMBER — the scope of each source shape is spelled in the types, at compile time. See `example-json-decoder`'s `Integration.scala` for the whole story in
  * one file.
  */
@implicitNotFound(
  "the fused pipeline over source shape ${S} has no lowering on the classpath — add the module that provides `FuseLowering[${S}]` (for JSON byte sources: example-json-decoder)"
)
trait FuseLowering[S]

/** The base of every lowering: the ONE member a module implements. `lower` receives the pipeline and the reified [[Terminal]]; the implementation is a macro
  * splice forwarding both to [[FuseMacro.lower]] together with the module's [[RecordDecoder]] (or none, for the engine-native shapes) — an ordinary method
  * argument, at macro-expansion time.
  */
trait ShapeLowering[S] extends FuseLowering[S]:
  inline def lower[A, R](inline self: Fuse[A, S], inline t: Terminal[A, R]): R

/** Aggregation terminals + everything that desugars onto them: the largest family, and the workhorse of any shape. One fused pass, one unboxed accumulator per
  * aggregate.
  */
trait AggTerminals[S] extends ShapeLowering[S]:
  extension [A](inline self: Fuse[A, S])
    /** run ONE aggregate, returning its result bare (no Tuple1). The single-aggregate base for most of the sugar below; the whole pipeline still fuses. */
    inline def agg[R1](inline a1: Agg[A, R1]): R1 = lower(self, Terminal.agg1(a1))
    // multi-aggregate: several aggregations in ONE fused pass, carrying one accumulator each. The aggregates'
    // read fields are merged automatically (union computed, shared field once, survivor-gated).
    inline def agg[R1, R2](inline a1: Agg[A, R1], inline a2: Agg[A, R2]): (R1, R2) = lower(self, Terminal.agg2(a1, a2))
    inline def agg[R1, R2, R3](inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3]): (R1, R2, R3) =
      lower(self, Terminal.agg3(a1, a2, a3))
    inline def agg[R1, R2, R3, R4](inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3], inline a4: Agg[A, R4]): (R1, R2, R3, R4) =
      lower(self, Terminal.agg4(a1, a2, a3, a4))
    // aggregate into a CASE CLASS (or any product): the results are passed to `make` (typically a case-class
    // constructor) instead of tupled — e.g. `xs.fuse.aggTo(Summary.apply)(Agg.sum(_.x), Agg.count, Agg.max(_.y))`.
    inline def aggTo[R1, R2, R](inline make: (R1, R2) => R)(inline a1: Agg[A, R1], inline a2: Agg[A, R2]): R =
      lower(self, Terminal.aggTo2(make, a1, a2))
    inline def aggTo[R1, R2, R3, R](inline make: (R1, R2, R3) => R)(inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3]): R =
      lower(self, Terminal.aggTo3(make, a1, a2, a3))
    inline def aggTo[R1, R2, R3, R4, R](
        inline make: (R1, R2, R3, R4) => R
    )(inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3], inline a4: Agg[A, R4]): R =
      lower(self, Terminal.aggTo4(make, a1, a2, a3, a4))

    // ---- the fold/effect family (all single-agg sugar) ----
    inline def foreach(inline f: A => Unit): Unit = self.agg(Agg.foreach[A](f))
    inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = self.agg(Agg.fold[A, Z](z)(op))

    /** number of elements surviving the whole pipeline. */
    inline def count: Int = self.agg(Agg.count[A])

    /** number of elements matching `p` (one fused pass). */
    inline def count(inline p: A => Boolean): Int = self.filter(p).count
    inline def fold[B >: A](z: B)(inline op: (B, B) => B): B = self.foldLeft[B](z)((acc, a) => op(acc, a))
    inline def sum[B >: A](using num: Numeric[B]): B = self.foldLeft[B](num.zero)((acc, a) => num.plus(acc, a))
    inline def product[B >: A](using num: Numeric[B]): B = self.foldLeft[B](num.one)((acc, a) => num.times(acc, a))
    inline def size: Int = self.count
    inline def length: Int = self.count

    // ---- conversions (one fused pass into a builder via foreach) ----
    inline def toList: List[A] = { val b = List.newBuilder[A]; self.foreach(b += _); b.result() }
    inline def toVector: Vector[A] = { val b = Vector.newBuilder[A]; self.foreach(b += _); b.result() }
    inline def toSeq: Seq[A] = self.toVector
    inline def toSet[B >: A]: Set[B] = { val b = Set.newBuilder[B]; self.foreach(b += _); b.result() }
    inline def toArray[B >: A](using ClassTag[B]): Array[B] = { val b = Array.newBuilder[B]; self.foreach(b += _); b.result() }
    inline def toMap[K, V](using ev: A <:< (K, V)): Map[K, V] =
      val b = Map.newBuilder[K, V]; self.foreach(a => b += ev(a)); b.result()
    inline def to[C1](factory: scala.collection.Factory[A, C1]): C1 =
      val b = factory.newBuilder; self.foreach(b += _); b.result()
    inline def mkString(start: String, sep: String, end: String): String =
      val sb = new java.lang.StringBuilder(start); var first = true
      self.foreach { a => if first then first = false else sb.append(sep); sb.append(String.valueOf(a.asInstanceOf[Object])) }
      sb.append(end).toString
    inline def mkString(sep: String): String = self.mkString("", sep, "")
    inline def mkString: String = self.mkString("", "", "")

    // ---- reductions (empty-safe versions desugar to a single-aggregate `agg(...)`) ----
    inline def reduceOption[B >: A](inline op: (B, A) => B): Option[B] = self.agg(Agg.reduceL[A, B](op))
    inline def reduceLeftOption[B >: A](inline op: (B, A) => B): Option[B] = self.reduceOption[B](op)
    inline def reduceLeft[B >: A](inline op: (B, A) => B): B =
      self.reduceOption[B](op).getOrElse(throw new UnsupportedOperationException("reduceLeft on an empty fused pipeline"))
    inline def reduce[B >: A](inline op: (B, B) => B): B =
      self.reduceOption[B]((acc, a) => op(acc, a)).getOrElse(throw new UnsupportedOperationException("reduce on an empty fused pipeline"))
    inline def min[B >: A](using ord: Ordering[B]): A =
      self
        .reduceOption[B]((acc, a) => if ord.lteq(acc, a) then acc else a)
        .getOrElse(throw new UnsupportedOperationException("min of an empty fused pipeline"))
        .asInstanceOf[A]
    inline def max[B >: A](using ord: Ordering[B]): A =
      self
        .reduceOption[B]((acc, a) => if ord.gteq(acc, a) then acc else a)
        .getOrElse(throw new UnsupportedOperationException("max of an empty fused pipeline"))
        .asInstanceOf[A]
    inline def minOption[B >: A](using ord: Ordering[B]): Option[A] =
      self.reduceOption[B]((acc, a) => if ord.lteq(acc, a) then acc else a).asInstanceOf[Option[A]]
    inline def maxOption[B >: A](using ord: Ordering[B]): Option[A] =
      self.reduceOption[B]((acc, a) => if ord.gteq(acc, a) then acc else a).asInstanceOf[Option[A]]
    inline def minByOption[B](inline f: A => B)(using ord: Ordering[B]): Option[A] = self.agg(Agg.minBy[A, B](f))
    inline def maxByOption[B](inline f: A => B)(using ord: Ordering[B]): Option[A] = self.agg(Agg.maxBy[A, B](f))
    inline def minBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      self.minByOption[B](f).getOrElse(throw new UnsupportedOperationException("minBy on an empty fused pipeline"))
    inline def maxBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      self.maxByOption[B](f).getOrElse(throw new UnsupportedOperationException("maxBy on an empty fused pipeline"))
    inline def lastOption: Option[A] = self.reduceOption[A]((_, a) => a)
    inline def last: A = self.reduceOption[A]((_, a) => a).getOrElse(throw new NoSuchElementException("last of an empty fused pipeline"))

    // ---- top-N: the n best elements in ONE fused pass via a bounded size-n heap (O(N log n) time, O(n)
    //      memory — no full sort, no O(N) buffer). Returns FArray[A], best-first. ----
    inline def topNBy[B](n: Int)(inline key: A => B)(using Ordering[B]): FArray[A] = self.agg(Agg.topNBy(n)(key))
    inline def bottomNBy[B](n: Int)(inline key: A => B)(using Ordering[B]): FArray[A] = self.agg(Agg.bottomNBy(n)(key))
    inline def topN[A1 >: A](n: Int)(using Ordering[A1]): FArray[A1] = self.agg(Agg.largest[A1](n))
    inline def bottomN[A1 >: A](n: Int)(using Ordering[A1]): FArray[A1] = self.agg(Agg.smallest[A1](n))

    // ---- multi-output terminals: one fused pass into two (or per-key) builders ----
    inline def partition(inline p: A => Boolean): (FArray[A], FArray[A]) =
      val y = List.newBuilder[A]; val n = List.newBuilder[A]
      self.foreach(a => if p(a) then y += a else n += a)
      (FArray.from(y.result()), FArray.from(n.result()))
    inline def span(inline p: A => Boolean): (FArray[A], FArray[A]) =
      val pre = List.newBuilder[A]; val post = List.newBuilder[A]; var go = true
      self.foreach(a => if go && p(a) then pre += a else { go = false; post += a })
      (FArray.from(pre.result()), FArray.from(post.result()))
    inline def unzip[A1, A2](using ev: A <:< (A1, A2)): (FArray[A1], FArray[A2]) =
      val l = List.newBuilder[A1]; val r = List.newBuilder[A2]
      self.foreach { a =>
        val t = ev(a); l += t._1; r += t._2
      }
      (FArray.from(l.result()), FArray.from(r.result()))
    inline def groupBy[K](inline f: A => K): Map[K, List[A]] =
      val m = scala.collection.mutable.LinkedHashMap.empty[K, scala.collection.mutable.Builder[A, List[A]]]
      self.foreach(a => m.getOrElseUpdate(f(a), List.newBuilder[A]) += a)
      m.view.mapValues(_.result()).toMap
    inline def groupMapReduce[K, B](inline key: A => K)(inline f: A => B)(inline reduce: (B, B) => B): Map[K, B] =
      val m = scala.collection.mutable.LinkedHashMap.empty[K, B]
      self.foreach { a =>
        val k = key(a); val b = f(a); m.updateWith(k) { case Some(o) => Some(reduce(o, b)); case None => Some(b) }
      }
      m.toMap

    // ---- windowing ----
    /** overlapping windows of size `n` stepping by 1; a shorter-than-`n` stream yields one partial window. */
    inline def sliding(n: Int): FArray[FArray[A]] =
      require(n > 0, "sliding size must be > 0")
      val out = List.newBuilder[FArray[A]]; val window = scala.collection.mutable.ArrayDeque.empty[A]; var emitted = false
      self.foreach { a =>
        window += a; if window.length > n then window.removeHead()
        if window.length == n then { out += FArray.from(window.toList); emitted = true }
      }
      if !emitted && window.nonEmpty then out += FArray.from(window.toList)
      FArray.from(out.result())

/** Short-circuit terminals: a done-flag stops the traversal — across sources and `flatMap` nesting alike — the instant the answer is known. */
trait SearchTerminals[S] extends ShapeLowering[S]:
  extension [A](inline self: Fuse[A, S])
    inline def find(inline p: A => Boolean): Option[A] = lower(self, Terminal.find(p))
    inline def exists(inline p: A => Boolean): Boolean = lower(self, Terminal.exists(p))
    inline def forall(inline p: A => Boolean): Boolean = lower(self, Terminal.forall(p))

    /** index of the first survivor satisfying `p` (post-upstream-filtering position), or -1 — a bare position counter, no (value, index) pair. */
    inline def indexWhere(inline p: A => Boolean): Int = lower(self, Terminal.indexWhere(p))
    inline def indexOf[B >: A](elem: B): Int = self.indexWhere(_ == elem)
    inline def contains[B >: A](elem: B): Boolean = self.exists(_ == elem)
    inline def isEmpty: Boolean = !self.exists(_ => true)
    inline def nonEmpty: Boolean = self.exists(_ => true)
    inline def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = self.find(pf.isDefinedAt).map(pf)

/** Group-reduce without the boxing: fold each group as elements stream past, into a primitive-keyed open-addressing table — an Int key and an Int/Long/Double
  * accumulator stay unboxed in the hot loop, boxing once per KEY at the final materialization.
  */
trait GroupTerminals[S] extends ShapeLowering[S]:
  extension [A](inline self: Fuse[A, S])
    inline def groupReduceBy[K, B](inline key: A => K)(inline value: A => B)(inline reduce: (B, B) => B): Map[K, B] =
      lower(self, Terminal.groupReduce(key, value, reduce))

    /** group-reduce with `value = identity` — combine the elements themselves per key. */
    inline def groupReduce[K, A1 >: A](inline key: A => K)(inline reduce: (A1, A1) => A1): Map[K, A1] =
      self.groupReduceBy(key)(a => (a: A1))(reduce)

    /** count elements per key (unboxed Int accumulator). */
    inline def groupCount[K](inline key: A => K): Map[K, Int] =
      self.groupReduceBy(key)(_ => 1)((x, y) => x + y)

    /** sum the projected `value` per key (unboxed for Int/Long/Double values). */
    inline def groupSum[K, B](inline key: A => K)(inline value: A => B)(using num: Numeric[B]): Map[K, B] =
      self.groupReduceBy(key)(value)((x, y) => num.plus(x, y))

/** Plan terminals (testing/diagnostics): a machine-checkable DESCRIPTION of the plan the macro built — the pipeline is analyzed, never executed. */
trait PlanTerminals[S] extends ShapeLowering[S]:
  extension [A](inline self: Fuse[A, S])
    inline def plan: String = lower(self, Terminal.plan)
    inline def planFold[Z](inline op: (Z, A) => Z): String = lower(self, Terminal.planFold(op))
    inline def planAgg(inline aggs: Agg[A, Any]*): String = lower(self, Terminal.planAgg(aggs))

/** Element-materializing terminals — the shapes whose elements can be handed out as-is (the engine-native ones today). */
trait MaterializeTerminals[S] extends ShapeLowering[S]:
  extension [A](inline self: Fuse[A, S])
    inline def run: FArray[A] = lower(self, Terminal.run)
    inline def head: A = lower(self, Terminal.head)
    inline def headOption: Option[A] = lower(self, Terminal.headOption)

/** The standard bundle a record-decoder shape supports: everything except element materialization. */
trait StandardTerminals[S] extends AggTerminals[S], SearchTerminals[S], GroupTerminals[S], PlanTerminals[S]

/** The source shape of engine-native pipelines: an in-memory `FArray` (`xs.fuse`) or a chunked [[Source]] (`src.fuse`). Its lowering — the full terminal set —
  * is [[NativeLowering]], published here so it rides the implicit scope of every `Fuse[A, FuseNative]`.
  */
sealed trait FuseNative
object FuseNative:
  given lowering: NativeLowering.type = NativeLowering

/** The engine's own lowering: every capability, lowered by [[FuseMacro]] with no record decoder attached. */
object NativeLowering extends StandardTerminals[FuseNative], MaterializeTerminals[FuseNative]:
  inline def lower[A, R](inline self: Fuse[A, FuseNative], inline t: Terminal[A, R]): R =
    ${ FuseMacro.lowerNative[A, R]('self, 't) }
