package farray.json

import farray.{Agg, FuseLowering, FuseMacro, Fuse}
import scala.quoted.*
import scala.reflect.ClassTag

/** The source shape of fused-NDJSON pipelines — the phantom type `Json.ndjson[T](bytes).stream` stamps on its `Fuse[T, Ndjson]`. Its companion publishes the
  * [[JsonLowering]] given, so the shape's terminals ride the implicit scope of the pipeline type: no imports at the use site, and no engine-side registry — the
  * decoder plugs into the fusion engine as an ordinary typeclass instance.
  */
sealed trait Ndjson
object Ndjson:
  given lowering: JsonLowering.type = JsonLowering

/** The NDJSON decoder's [[FuseLowering]]: exactly the terminals the record path supports (the v1 field-reading set), each lowered by [[JsonTerminalMacro]],
  * which hands the engine [[JsonDecode]] directly — an ordinary method argument at macro-expansion time. A terminal that isn't here (e.g. `run`) is NOT A
  * MEMBER of a JSON pipeline: the scope of the experiment is spelled in the types.
  */
object JsonLowering extends FuseLowering[Ndjson]:
  extension [A](inline self: Fuse[A, Ndjson])
    // ---- base terminals (macros: rewrite the whole chain into one per-record scanner) ----
    inline def find(inline p: A => Boolean): Option[A] = ${ JsonTerminalMacro.findImpl[A]('self, 'p) }
    inline def exists(inline p: A => Boolean): Boolean = ${ JsonTerminalMacro.existsImpl[A]('self, 'p) }
    inline def forall(inline p: A => Boolean): Boolean = ${ JsonTerminalMacro.forallImpl[A]('self, 'p) }
    inline def indexWhere(inline p: A => Boolean): Int = ${ JsonTerminalMacro.indexWhereImpl[A]('self, 'p) }

    /** a machine-checkable description of the scanner plan (which fields are scanned/decoded/lazy, early-outs) — see the engine's `plan`. */
    inline def plan: String = ${ JsonTerminalMacro.planImpl[A]('self) }
    inline def planFold[Z](inline op: (Z, A) => Z): String = ${ JsonTerminalMacro.planFoldImpl[A, Z]('self, 'op) }
    inline def planAgg(inline aggs: Agg[A, Any]*): String = ${ JsonTerminalMacro.planAggImpl[A]('self, 'aggs) }

    inline def agg[R1](inline a1: Agg[A, R1]): R1 =
      ${ JsonTerminalMacro.aggImpl[A, R1]('self, '{ List(a1) }) }
    inline def agg[R1, R2](inline a1: Agg[A, R1], inline a2: Agg[A, R2]): (R1, R2) =
      ${ JsonTerminalMacro.aggImpl[A, (R1, R2)]('self, '{ List(a1, a2) }) }
    inline def agg[R1, R2, R3](inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3]): (R1, R2, R3) =
      ${ JsonTerminalMacro.aggImpl[A, (R1, R2, R3)]('self, '{ List(a1, a2, a3) }) }
    inline def agg[R1, R2, R3, R4](inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3], inline a4: Agg[A, R4]): (R1, R2, R3, R4) =
      ${ JsonTerminalMacro.aggImpl[A, (R1, R2, R3, R4)]('self, '{ List(a1, a2, a3, a4) }) }
    inline def aggTo[R1, R2, R](inline make: (R1, R2) => R)(inline a1: Agg[A, R1], inline a2: Agg[A, R2]): R =
      ${ JsonTerminalMacro.aggToImpl[A, R]('self, '{ List(a1, a2) }, 'make) }
    inline def aggTo[R1, R2, R3, R](inline make: (R1, R2, R3) => R)(inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3]): R =
      ${ JsonTerminalMacro.aggToImpl[A, R]('self, '{ List(a1, a2, a3) }, 'make) }
    inline def aggTo[R1, R2, R3, R4, R](
        inline make: (R1, R2, R3, R4) => R
    )(inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3], inline a4: Agg[A, R4]): R =
      ${ JsonTerminalMacro.aggToImpl[A, R]('self, '{ List(a1, a2, a3, a4) }, 'make) }

    // ---- derived terminals — the same sugar the native lowering offers, over the base terminals above ----
    inline def foreach(inline f: A => Unit): Unit = self.agg(Agg.foreach[A](f))
    inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = self.agg(Agg.fold[A, Z](z)(op))
    inline def count: Int = self.agg(Agg.count[A])
    inline def count(inline p: A => Boolean): Int = self.filter(p).count
    inline def fold[B >: A](z: B)(inline op: (B, B) => B): B = self.foldLeft[B](z)((acc, a) => op(acc, a))
    inline def sum[B >: A](using num: Numeric[B]): B = self.foldLeft[B](num.zero)((acc, a) => num.plus(acc, a))
    inline def product[B >: A](using num: Numeric[B]): B = self.foldLeft[B](num.one)((acc, a) => num.times(acc, a))

    inline def toList: List[A] = { val b = List.newBuilder[A]; self.foreach(b += _); b.result() }
    inline def toVector: Vector[A] = { val b = Vector.newBuilder[A]; self.foreach(b += _); b.result() }
    inline def toSeq: Seq[A] = self.toVector
    inline def toSet[B >: A]: Set[B] = { val b = Set.newBuilder[B]; self.foreach(b += _); b.result() }
    inline def toArray[B >: A](using ClassTag[B]): Array[B] = { val b = Array.newBuilder[B]; self.foreach(b += _); b.result() }
    inline def to[C1](factory: scala.collection.Factory[A, C1]): C1 =
      val b = factory.newBuilder; self.foreach(b += _); b.result()
    inline def mkString(start: String, sep: String, end: String): String =
      val sb = new java.lang.StringBuilder(start); var first = true
      self.foreach { a => if first then first = false else sb.append(sep); sb.append(String.valueOf(a.asInstanceOf[Object])) }
      sb.append(end).toString
    inline def mkString(sep: String): String = self.mkString("", sep, "")
    inline def mkString: String = self.mkString("", "", "")

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
    inline def minByOption[B](inline f: A => B)(using ord: Ordering[B]): Option[A] = self.agg(Agg.minBy[A, B](f))
    inline def maxByOption[B](inline f: A => B)(using ord: Ordering[B]): Option[A] = self.agg(Agg.maxBy[A, B](f))
    inline def minBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      self.minByOption[B](f).getOrElse(throw new UnsupportedOperationException("minBy on an empty fused pipeline"))
    inline def maxBy[B](inline f: A => B)(using ord: Ordering[B]): A =
      self.maxByOption[B](f).getOrElse(throw new UnsupportedOperationException("maxBy on an empty fused pipeline"))
    inline def lastOption: Option[A] = self.reduceOption[A]((_, a) => a)
    inline def last: A = self.reduceOption[A]((_, a) => a).getOrElse(throw new NoSuchElementException("last of an empty fused pipeline"))

    inline def contains[B >: A](elem: B): Boolean = self.exists(_ == elem)
    inline def isEmpty: Boolean = !self.exists(_ => true)
    inline def nonEmpty: Boolean = self.exists(_ => true)
    inline def size: Int = self.count
    inline def length: Int = self.count
    inline def indexOf[B >: A](elem: B): Int = self.indexWhere(_ == elem)

    /** group by `key`, combine each element's `value` per key with `reduce`, in ONE fused scan — same primitive-keyed table as the native lowering. */
    inline def groupReduceBy[K, B](inline key: A => K)(inline value: A => B)(inline reduce: (B, B) => B): Map[K, B] =
      ${ JsonTerminalMacro.groupReduceByImpl[A, K, B]('self, 'key, 'value, 'reduce) }
    inline def groupReduce[K, A1 >: A](inline key: A => K)(inline reduce: (A1, A1) => A1): Map[K, A1] =
      self.groupReduceBy(key)(a => (a: A1))(reduce)
    inline def groupCount[K](inline key: A => K): Map[K, Int] =
      self.groupReduceBy(key)(_ => 1)((x, y) => x + y)
    inline def groupSum[K, B](inline key: A => K)(inline value: A => B)(using num: Numeric[B]): Map[K, B] =
      self.groupReduceBy(key)(value)((x, y) => num.plus(x, y))

    // ---- top-N via a bounded size-n heap, straight over the byte scan ----
    inline def topNBy[B](n: Int)(inline key: A => B)(using Ordering[B]): farray.FArray[A] = self.agg(Agg.topNBy(n)(key))
    inline def bottomNBy[B](n: Int)(inline key: A => B)(using Ordering[B]): farray.FArray[A] = self.agg(Agg.bottomNBy(n)(key))
    inline def topN[A1 >: A](n: Int)(using Ordering[A1]): farray.FArray[A1] = self.agg(Agg.largest[A1](n))
    inline def bottomN[A1 >: A](n: Int)(using Ordering[A1]): farray.FArray[A1] = self.agg(Agg.smallest[A1](n))

/** The decoder module's macro entries: thin wrappers that call the engine's `…With` entry points, passing [[JsonDecode]] as a plain argument — the whole
  * "discovery" of the decoder is this file's import graph. No registry, no reflection; a pipeline only reaches these through [[JsonLowering]]'s extensions,
  * which only exist on `Fuse[A, Ndjson]`.
  */
private[farray] object JsonTerminalMacro:
  def aggImpl[A: Type, R: Type](self: Expr[Fuse[A, Ndjson]], aggs: Expr[List[Agg[A, Any]]])(using Quotes): Expr[R] =
    FuseMacro.aggImplWith[A, Ndjson, R](self, aggs, JsonDecode)
  def aggToImpl[A: Type, R: Type](self: Expr[Fuse[A, Ndjson]], aggs: Expr[List[Agg[A, Any]]], make: Expr[Any])(using Quotes): Expr[R] =
    FuseMacro.aggToImplWith[A, Ndjson, R](self, aggs, make, JsonDecode)
  def findImpl[A: Type](self: Expr[Fuse[A, Ndjson]], p: Expr[A => Boolean])(using Quotes): Expr[Option[A]] =
    FuseMacro.findImplWith[A, Ndjson](self, p, JsonDecode)
  def existsImpl[A: Type](self: Expr[Fuse[A, Ndjson]], p: Expr[A => Boolean])(using Quotes): Expr[Boolean] =
    FuseMacro.existsImplWith[A, Ndjson](self, p, JsonDecode)
  def forallImpl[A: Type](self: Expr[Fuse[A, Ndjson]], p: Expr[A => Boolean])(using Quotes): Expr[Boolean] =
    FuseMacro.forallImplWith[A, Ndjson](self, p, JsonDecode)
  def indexWhereImpl[A: Type](self: Expr[Fuse[A, Ndjson]], p: Expr[A => Boolean])(using Quotes): Expr[Int] =
    FuseMacro.indexWhereImplWith[A, Ndjson](self, p, JsonDecode)
  def groupReduceByImpl[A: Type, K: Type, B: Type](self: Expr[Fuse[A, Ndjson]], key: Expr[A => K], value: Expr[A => B], reduce: Expr[(B, B) => B])(using
      Quotes
  ): Expr[Map[K, B]] =
    FuseMacro.groupReduceByImplWith[A, Ndjson, K, B](self, key, value, reduce, JsonDecode)
  def planImpl[A: Type](self: Expr[Fuse[A, Ndjson]])(using Quotes): Expr[String] =
    FuseMacro.planImplWith[A, Ndjson](self, JsonDecode)
  def planFoldImpl[A: Type, Z: Type](self: Expr[Fuse[A, Ndjson]], op: Expr[(Z, A) => Z])(using Quotes): Expr[String] =
    FuseMacro.planFoldImplWith[A, Ndjson, Z](self, op, JsonDecode)
  def planAggImpl[A: Type](self: Expr[Fuse[A, Ndjson]], aggs: Expr[Seq[Agg[A, Any]]])(using Quotes): Expr[String] =
    FuseMacro.planAggImplWith[A, Ndjson](self, aggs, JsonDecode)
