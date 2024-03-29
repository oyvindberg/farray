package farray

import farray.FArray.{Empty, newBuilder}

import scala.annotation.static
import scala.collection.{Factory, immutable, mutable}
import scala.quoted.{Expr, Quotes, Type, Varargs, quotes}

object FArray:
  @static val Empty = new FArray(Array.ofDim(0), 0)

  type Builder[A <: AnyRef] = FArrayBuilder[A]
  val Builder = FArrayBuilder

  private[farray] inline def create[A <: AnyRef](as: Array[AnyRef], length: Int): FArray[A] =
    if as.length == 0 then Empty else new FArray[A](as, length)

  private[farray] inline def create[A <: AnyRef](as: Array[AnyRef]): FArray[A] =
    create(as, as.length)

  inline def empty[A <: AnyRef]: FArray[A] =
    Empty

  inline def apply[A <: AnyRef](inline as: A*): FArray[A] =
    ${ applyImpl('{ as }) }

  def applyImpl[A <: AnyRef](as: Expr[Seq[A]])(using Type[A], Quotes): Expr[FArray[A]] = {
    import quotes.reflect.*
    val ret = as match
      case Varargs(Nil) => '{ FArray.empty }
      case Varargs(exprs) =>
        '{
          val ret = new Array[AnyRef](${ Expr(exprs.length) })
          ${
            Expr.block(
              exprs.toList.zipWithIndex.map { case (expr, idx) => '{ ret(${ Expr(idx) }) = ${ expr } } },
              '{ new FArray[A](ret, ${ Expr(exprs.length) }) }
            )
          }

        }
      case _ =>
        '{ fromIterable[A](${ as }) }

//    report.warning(ret.show)
    ret
  }

  def fromOption[A <: AnyRef](oa: Option[A]): FArray[A] =
    oa match
      case Some(a) => create(Array(a))
      case None    => Empty

  inline def fromOptions[A <: AnyRef](as: Option[A]*): FArray[A] =
    apply(as.flatten: _*)

  inline def fromArray[A <: AnyRef](as: Array[A]): FArray[A] =
    create(as.asInstanceOf[Array[AnyRef]])

  inline def fromIterable[A <: AnyRef](as: Iterable[A]): FArray[A] =
    create(as.asInstanceOf[Iterable[AnyRef]].toArray)

  inline def fromIterator[A <: AnyRef](as: Iterator[A]): FArray[A] =
    create(as.asInstanceOf[Iterator[AnyRef]].toArray)

  inline def fromJava[A <: AnyRef](as: java.util.Collection[A]): FArray[A] =
    create(as.toArray)

  inline def fromJavaIterator[A <: AnyRef](as: java.util.Iterator[A]): FArray[A] =
    val b = newBuilder[A]
    as.forEachRemaining(a => b += a)
    b.result()

  inline def fromJavaStream[A <: AnyRef](as: java.util.stream.Stream[A]): FArray[A] =
    create(as.toArray)

  def fill[A <: AnyRef](n: Int)(a: A): FArray[A] =
    create(Array.fill[AnyRef](n)(a))

  inline def tabulate[A <: AnyRef](n: Int)(inline f: Int => A): FArray[A] =
    create(Array.tabulate[AnyRef](n)(f))

  def fromRange(r: Range): FArray[Integer] =
    fromIterator(r.iterator.map(i => i))

  def range(start: Int, end: Int): FArray[Integer] =
    range(start, end, step = 1)

  def range(start: Int, end: Int, step: Int): FArray[Integer] =
    val length = end - start
    require(step != 0, "step must not be 0")
    require(Integer.signum(length) >= 0 == Integer.signum(step) >= 0, "length and step but have same sign")
    val numSteps = math.ceil(length.toDouble / step).toInt
    val a = new Array[AnyRef](numSteps)
    var current = start
    var currentStep = 0
    while currentStep < numSteps do
      a(currentStep) = current: Integer
      current += step
      currentStep += 1
    create[Integer](a)

  object first:
    def unapply[A <: AnyRef](as: FArray[A]): Option[A] = as.headOption
  object firstTwo:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A)] =
      if as.length >= 2 then Some((as(0), as(1))) else None
  object firstThree:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A, A)] =
      if as.length >= 3 then Some((as(0), as(1), as(2))) else None
  object last:
    def unapply[A <: AnyRef](as: FArray[A]): Option[A] = as.lastOption
  object exactlyOne:
    def unapply[A <: AnyRef](as: FArray[A]): Option[A] =
      if as.length == 1 then Some(as(0)) else None
  object exactlyTwo:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A)] =
      if as.length == 2 then Some((as(0), as(1))) else None
  object exactlyThree:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A, A)] =
      if as.length == 3 then Some((as(0), as(1), as(2))) else None
  object exactlyFour:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A, A, A)] =
      if as.length == 4 then Some((as(0), as(1), as(2), as(3))) else None
  object exactlyFive:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A, A, A, A)] =
      if as.length == 5 then Some((as(0), as(1), as(2), as(3), as(4))) else None
  object headTail:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, FArray[A])] =
      if as.length == 0 then None else Some((as.head, as.tail))
  object headHeadTail:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(A, A, FArray[A])] =
      if as.length < 2 then None else Some((as(0), as(1), as.drop(2)))
  object initLast:
    def unapply[A <: AnyRef](as: FArray[A]): Option[(FArray[A], A)] =
      if as.length == 0 then None else Some((as.init, as.last))

  def unapplySeq[A <: AnyRef](as: FArray[A]): UnapplySeq[A] = new UnapplySeq(as)

  def newBuilder[A <: AnyRef]: Builder[A] = FArrayBuilder.empty()
  def newBuilder[A <: AnyRef](initialCapacity: Int): Builder[A] = FArrayBuilder.empty(initialCapacity)

  given ordering[T <: AnyRef: Ordering]: Ordering[FArray[T]] =
    new Ordering[FArray[T]]:
      override def compare(x: FArray[T], y: FArray[T]): Int =
        val xe = x.iterator
        val ye = y.iterator

        while xe.hasNext && ye.hasNext do
          val res = summon[Ordering[T]].compare(xe.next(), ye.next())
          if res != 0 then return res

        summon[Ordering[Boolean]].compare(xe.hasNext, ye.hasNext)

  extension [T <: AnyRef](ts: FArray[FArray[T]])
    def flatten: FArray[T] =
      val newLength: Int =
        var value = 0
        var i = 0
        while i < ts.length do
          value += ts(i).length
          i += 1
        value

      val ret = new Array[AnyRef](newLength)
      var i = 0
      var o = 0
      while i < ts.length do
        val bs: FArray[T] = ts(i)
        var j = 0
        while j < bs.length do
          ret(o) = bs(j)
          j += 1
          o += 1

        i += 1
      create(ret)

  extension [A <: AnyRef](as: FArray[A])
    def contains(t: A): Boolean =
      var idx = 0
      while idx < as.length do
        if as(idx) == t then return true
        idx += 1
      false

  final class SizeCompareOps[A <: AnyRef](val it: FArray[A]) extends AnyVal {

    /** Tests if the size of the collection is less than some value. */
    inline def <(size: Int): Boolean = it.sizeCompare(size) < 0

    /** Tests if the size of the collection is less than or equal to some value. */
    inline def <=(size: Int): Boolean = it.sizeCompare(size) <= 0

    /** Tests if the size of the collection is equal to some value. */
    inline def ==(size: Int): Boolean = it.sizeCompare(size) == 0

    /** Tests if the size of the collection is not equal to some value. */
    inline def !=(size: Int): Boolean = it.sizeCompare(size) != 0

    /** Tests if the size of the collection is greater than or equal to some value. */
    inline def >=(size: Int): Boolean = it.sizeCompare(size) >= 0

    /** Tests if the size of the collection is greater than some value. */
    inline def >(size: Int): Boolean = it.sizeCompare(size) > 0
  }

final class FArray[+A <: AnyRef](underlying: Array[AnyRef], val length: Int):
  self =>

  private[FArray] def get_underlying: Array[AnyRef] = underlying

  def get: this.type = this

  inline def lengthIs[B >: A <: AnyRef]: FArray.SizeCompareOps[B] = new FArray.SizeCompareOps(this)
  inline def size = length
  inline def sizeCompare(otherSize: Int): Int = Integer.compare(length, otherSize)
  inline def sizeIs[B >: A <: AnyRef]: FArray.SizeCompareOps[B] = new FArray.SizeCompareOps(this)
  inline def isEmpty: Boolean = length == 0
  inline def nonEmpty: Boolean = length > 0
  inline def lengthCompare(len: Int): Int = Integer.compare(length, len)

  def apply(n: Int): A = underlying(n).asInstanceOf[A]

  inline def isDefinedAt(n: Int): Boolean = n < length

  inline def map[B <: AnyRef](inline f: A => B): FArray[B] =
    if isEmpty then this.asInstanceOf[FArray[B]]
    else
      val newArray = new Array[AnyRef](length)
      var i = 0
      while i < length do
        newArray(i) = f(apply(i))
        i += 1

      new FArray[B](newArray, length)

  inline def foreach(inline f: A => Unit): Unit =
    var i = 0
    while i < length do
      f(apply(i))
      i += 1

  inline def flatMap[B <: AnyRef](inline f: A => FArray[B]): FArray[B] =
    map(f).flatten

  inline def fold[A1 >: A](z: A1)(inline op: (A1, A1) => A1): A1 =
    foldLeft(z)(op)

  inline def foldLeft[Z](z: Z)(inline f: (Z, A) => Z): Z =
    var current = z
    foreach { a => current = f(current, a) }
    current

  inline def reduce[A1 >: A](inline op: (A1, A1) => A1): A1 =
    if isEmpty then sys.error("reduce on empty list")
    var ret: A1 = apply(0)
    var idx = 1
    while idx < length do
      ret = op(ret, apply(idx))
      idx += 1
    ret

  inline def reduceOption[A1 >: A](inline op: (A1, A1) => A1): Option[A1] =
    if isEmpty then None else Some(reduce(op))

  inline def reduceLeft[B >: A <: AnyRef](inline op: (B, A) => B): B =
    if isEmpty then throw new UnsupportedOperationException("empty.reduceLeft")

    var acc: B = null.asInstanceOf[B]
    var idx = 0
    while idx < length do
      val x = apply(idx)
      if idx == 0 then acc = x
      else acc = op(acc, x)
      idx += 1
    acc

  inline def reduceRight[B >: A](inline op: (A, B) => B): B = {
    if isEmpty then throw new UnsupportedOperationException("empty.reduceRight")

    var acc: B = null.asInstanceOf[B]
    val last = length - 1
    var idx = last
    while idx >= 0 do
      val x = apply(idx)
      if idx == last then acc = x
      else acc = op(x, acc)
      idx -= 1
    acc
  }

  inline def count(inline p: A => Boolean): Int =
    var ret = 0
    foreach { a => if p(a) then ret += 1 }
    ret

  def headOption: Option[A] = if isEmpty then None else Some(apply(0))

  def head: A = if isEmpty then sys.error("head of empty list") else apply(0)

  def tail: FArray[A] = if isEmpty then sys.error("tail of empty list") else drop(1)

  def init: FArray[A] = if isEmpty then sys.error("init of empty list") else dropRight(1)

  def lastOption: Option[A] = if isEmpty then None else Some(apply(length - 1))

  def last: A = if isEmpty then sys.error("last of empty list") else apply(length - 1)

  inline def forall(inline p: A => Boolean): Boolean =
    var idx = 0
    var foundNot = true
    while idx < length && foundNot do
      if !p(apply(idx)) then foundNot = false
      idx += 1
    foundNot

  inline def exists(inline p: A => Boolean): Boolean =
    var idx = 0
    var found = false
    while idx < length do
      if p(apply(idx)) then
        found = true
        idx = Int.MaxValue // break
      else idx += 1
    found

  inline def collectFirst[B](inline pf: PartialFunction[A, B]): Option[B] =
    var idx = 0
    var found: Option[B] = None
    while idx < length && found.isEmpty do
      val a = apply(idx)
      if pf.isDefinedAt(a) then found = Some(pf(a))
      idx += 1
    found

  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  def indexOf[B >: A](elem: B, from: Int): Int =
    var i = math.max(from, 0)
    while i < length do
      if elem == apply(i) then return i
      i += 1
    -1

  inline def indexWhere(inline p: A => Boolean): Int = indexWhere(p, 0)

  inline def indexWhere(inline p: A => Boolean, from: Int): Int =
    var i = math.max(from, 0)
    var ret = -1
    while i < length do
      if p(apply(i)) then
        ret = i
        i = Int.MaxValue // break
      else i += 1
    ret

  inline def find(inline f: A => Boolean): Option[A] =
    var idx = 0
    var found: Option[A] = None
    while idx < length do
      val a = apply(idx)
      if f(a) then
        found = Some(a)
        idx = Int.MaxValue // break
      else idx += 1
    found

  def collect[B <: AnyRef](f: PartialFunction[A, B]): FArray[B] =
    if isEmpty then this.asInstanceOf[FArray[B]]
    else
      val ret = new Array[AnyRef](length)
      var o = 0
      foreach { a =>
        if f.isDefinedAt(a) then
          ret(o) = f(a)
          o += 1
      }
      FArray.create(ret, o)

  inline def filterNot(inline f: A => Boolean): FArray[A] =
    filter(a => !f(a))

  inline def filter(inline f: A => Boolean): FArray[A] =
    if isEmpty then this
    else
      val ret = new Array[AnyRef](length)
      var o = 0
      foreach { a =>
        if f(a) then
          ret(o) = a
          o += 1
      }
      FArray.create(ret, o)

  inline def withFilter(inline f: A => Boolean): FArray[A] =
    filter(f)

  def ++[B >: A <: AnyRef](that: FArray[B]): FArray[B] = appendedAll(that)

  def appendedAll[B >: A <: AnyRef](that: FArray[B]): FArray[B] =
    if isEmpty then that
    else if that.isEmpty then this
    else
      val newLength = length + that.length
      val ret = new Array[AnyRef](newLength)
      System.arraycopy(underlying, 0, ret, 0, length)
      System.arraycopy(that.get_underlying, 0, ret, length, that.length)
      FArray.create[A](ret)

  // prepend
  def +:[B >: A <: AnyRef](elem: B): FArray[B] =
    val newLength = length + 1
    val ret = new Array[AnyRef](newLength)
    ret(0) = elem
    System.arraycopy(underlying, 0, ret, 1, length)
    FArray.create[B](ret)

  // append
  def :+[B >: A <: AnyRef](elem: B): FArray[B] =
    val newLength = length + 1
    val ret = new Array[AnyRef](newLength)
    System.arraycopy(underlying, 0, ret, 0, length)
    ret(length) = elem
    FArray.create[B](ret)

  // append
  def ::[B >: A <: AnyRef](elem: B): FArray[B] =
    this.+:(elem)

  def :::[B >: A <: AnyRef](prefix: FArray[B]): FArray[B] =
    prefix ++ this

  def take(n: Int): FArray[A] =
    val newLength = math.max(0, math.min(length, n))
    if newLength == 0 then FArray.Empty
    else if newLength == length then this
    else
      val ret = new Array[AnyRef](newLength)
      System.arraycopy(underlying, 0, ret, 0, newLength)
      FArray.create[A](ret)

  def takeRight(n: Int): FArray[A] =
    val newLength = math.max(0, math.min(length, n))
    if newLength == 0 then FArray.Empty
    else if newLength == length then this
    else
      val ret = new Array[AnyRef](newLength)
      System.arraycopy(underlying, length - newLength, ret, 0, newLength)
      FArray.create[A](ret)

  inline def takeWhile(inline p: A => Boolean): FArray[A] =
    if isEmpty then this
    else
      var i = 0
      var ret = this
      while i < length do
        if p(apply(i)) then i += 1
        else
          ret = take(i)
          i = Int.MaxValue // break
      ret

  def drop(n0: Int): FArray[A] =
    val n = math.max(0, n0)
    val newLength = math.max(0, length - n)
    if newLength == length then return this
    else if newLength == 0 then return Empty
    val ret = new Array[AnyRef](newLength)
    System.arraycopy(underlying, n, ret, 0, newLength)
    FArray.create[A](ret)

  def dropRight(n0: Int): FArray[A] =
    val n = math.max(0, n0)
    val newLength = math.max(0, length - n)
    if newLength == length then return this
    FArray.create[A](underlying, newLength)

  inline def dropWhile(inline p: A => Boolean): FArray[A] =
    if isEmpty then this
    else
      var idx = 0
      var continue = true
      while idx < length && continue do
        if !p(apply(idx)) then continue = false
        else idx += 1
      drop(idx)

  def splitAt(n: Int): (FArray[A], FArray[A]) =
    (take(n), drop(n))

  def reverse: FArray[A] =
    if isEmpty then return FArray.Empty
    val ret = new Array[AnyRef](length)
    var idx = 0
    while idx < length do
      ret(idx) = underlying(length - 1 - idx)
      idx += 1
    FArray.create[A](ret)

  // todo: optimize later
  def reverse_:::[B >: A <: AnyRef](prefix: FArray[B]): FArray[B] =
    prefix.reverse ++ this

  def zip[B <: AnyRef](other: FArray[B]): FArray[(A, B)] =
    val newLength = math.min(length, other.length)
    if newLength == 0 then return FArray.Empty

    val ret = new Array[AnyRef](newLength)
    var idx = 0
    while idx < newLength do
      ret(idx) = (underlying(idx), other(idx))
      idx += 1
    FArray.create[(A, B)](ret)

  def lazyZip[B <: AnyRef](other: FArray[B]): FArray[(A, B)] =
    zip(other)

  def lazyZip[B <: AnyRef, C <: AnyRef](bs: FArray[B], cs: FArray[C]): FArray[(A, B, C)] =
    val newLength = math.min(math.min(length, bs.length), cs.length)
    if newLength == 0 then return FArray.Empty

    val ret = new Array[AnyRef](newLength)
    var idx = 0
    while idx < newLength do
      ret(idx) = (underlying(idx), bs(idx), cs(idx))
      idx += 1
    FArray.create[(A, B, C)](ret)

  def zipWithIndex: FArray[(A, Int)] =
    if isEmpty then return FArray.Empty

    val ret = new Array[AnyRef](length)
    var idx = 0
    while idx < length do
      ret(idx) = (underlying(idx), idx)
      idx += 1
    FArray.create[(A, Int)](ret)

  inline def partition(inline f: A => Boolean): (FArray[A], FArray[A]) =
    val lefts = new Array[AnyRef](size)
    val rights = new Array[AnyRef](size)
    var i = 0
    var ol = 0
    var or = 0
    while i < length do
      val current = apply(i)
      if f(current) then
        lefts(ol) = current
        ol += 1
      else
        rights(or) = current
        or += 1
      i += 1

    (FArray.create(lefts, ol), FArray.create(rights, or))

  inline def partitionMap[A1 <: AnyRef, A2 <: AnyRef](f: A => Either[A1, A2]): (FArray[A1], FArray[A2]) = {
    val l = FArray.newBuilder[A1](length)
    val r = FArray.newBuilder[A2](length)
    foreach { x =>
      f(x) match {
        case Left(x1)  => l += x1
        case Right(x2) => r += x2
      }
    }
    (l.result(), r.result())
  }

  def iterator: Iterator[A] =
    new Iterator[A]:
      var idx = 0

      override def hasNext: Boolean = idx < self.length

      override def next(): A =
        val ret = underlying(idx).asInstanceOf[A]
        idx += 1
        ret

  def reverseIterator: Iterator[A] =
    new Iterator[A]:
      var idx = self.length - 1

      override def hasNext: Boolean = idx >= 0

      override def next(): A =
        val ret = underlying(idx).asInstanceOf[A]
        idx -= 1
        ret

  def indices: Range = 0 until length

  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): FArray[A] =
    sorted(ord.on(f))

  def sorted[B >: A <: AnyRef](implicit ord: Ordering[B]): FArray[A] =
    if length < 2 then this
    else
      val ret = new Array[AnyRef](length)
      System.arraycopy(underlying, 0, ret, 0, length)
      java.util.Arrays.sort(ret, ord.asInstanceOf[Ordering[AnyRef]])
      FArray.create(ret)

  def sortWith(lt: (A, A) => Boolean): FArray[A] =
    sorted(Ordering.fromLessThan(lt))

  def min[B >: A](implicit cmp: Ordering[B]): A =
    if isEmpty then sys.error("min on empty FList")

    reduce((x, y) => if cmp.lteq(x, y) then x else y)

  def max[B >: A](implicit cmp: Ordering[B]): A =
    if isEmpty then sys.error("max on empty FList")

    reduce((x, y) => if cmp.gteq(x, y) then x else y)

  inline def maxBy[B](inline f: A => B)(implicit cmp: Ordering[B]): A =
    if isEmpty then sys.error("maxBy on empty FList")

    var maxF: B = null.asInstanceOf[B]
    var maxElem: A = null.asInstanceOf[A]
    var first = true

    for elem <- this do
      val fx = f(elem)
      if first || cmp.gt(fx, maxF) then
        maxElem = elem
        maxF = fx
        first = false
    maxElem

  inline def minBy[B](inline f: A => B)(implicit cmp: Ordering[B]): A =
    if isEmpty then sys.error("minBy on empty FList")

    var minF: B = null.asInstanceOf[B]
    var minElem: A = null.asInstanceOf[A]
    var first = true

    for elem <- this do
      val fx = f(elem)
      if first || cmp.lt(fx, minF) then
        minElem = elem
        minF = fx
        first = false
    minElem

  inline def distinctBy[B](inline f: A => B): FArray[A] = {
    if (lengthCompare(1) <= 0) this
    else {
      val ret = new Array[AnyRef](length)
      var o = 0
      val seen = mutable.HashSet.empty[B]
      var different = false
      foreach { next =>
        if (seen.add(f(next)))
          ret(o) = next
          o += 1
        else different = true
      }
      if (different) FArray.create(ret, o) else this
    }
  }

  def distinct: FArray[A] =
    if length < 2 then this
    else
      val ret = new Array[AnyRef](length)
      val seen = new mutable.HashSet[A]()
      seen.sizeHint(length)
      var idx = 0
      var different = false
      var o = 0
      while idx < length do
        val next = apply(idx)
        if seen.add(next) then
          ret(o) = next
          o += 1
        else different = true
        idx += 1
      if different then FArray.create(ret, o) else this

  // todo: optimize
  def slice(from: Int, until: Int): FArray[A] = {
    val lo = math.max(from, 0)
    if (until <= lo || isEmpty) Empty
    else this drop lo take (until - lo)
  }

  def to[C1](factory: Factory[A, C1]): C1 = {
    val b = factory.newBuilder
    b.sizeHint(length)
    foreach(b.addOne)
    b.result()
  }

  def toSet[AA >: A]: Set[AA] = to(Set)

  def toList: List[A] = to(List)

  def toSeq: Seq[A] =
    new AsIndexedSeq(this)

  def toIndexedSeq: AsIndexedSeq[A] =
    new AsIndexedSeq(this)

  def toVector: Vector[A] = to(Vector)

  def toArray[AA >: A](implicit CT: reflect.ClassTag[AA]): Array[AA] =
    val ret = CT.newArray(length)
    System.arraycopy(underlying, 0, ret, 0, length)
    ret

  inline def toMap[T, U](implicit inline ev: A <:< (T, U)): Map[T, U] =
    val ret = Map.newBuilder[T, U]
    ret.sizeHint(length)
    var idx = 0
    while idx < length do
      ret += ev(apply(idx))
      idx += 1

    ret.result()

  inline def groupBy[K](inline f: A => K): Map[K, FArray[A]] =
    val builder = mutable.HashMap.empty[K, FArray[A]]
    var idx = 0
    while idx < length do
      val a = apply(idx)
      val key = f(a)
      val newEntry = builder.get(key) match
        case Some(existing) => existing :+ a
        case None           => FArray[A](a)
      builder.put(key, newEntry)
      idx += 1
    builder.toMap

  inline def groupMap[K, B <: AnyRef](inline key: A => K)(inline f: A => B): immutable.Map[K, FArray[B]] = {
    val m = mutable.Map.empty[K, FArrayBuilder[B]]
    for (elem <- this) {
      val k = key(elem)
      val bldr = m.getOrElseUpdate(k, FArray.newBuilder[B])
      bldr += f(elem)
    }
    val result = immutable.Map.newBuilder[K, FArray[B]]
    m.foreach { case (k, v) => result.addOne(k, v.result()) }
    result.result()
  }

  def transpose[B <: AnyRef](implicit asTraversable: A => FArray[B]): FArray[FArray[B]] =
    if isEmpty then return FArray.Empty

    def fail() = sys.error("transpose requires all collections have the same size")

    val headSize = asTraversable(head).length
    val bs = Array.fill(headSize)(FArray.newBuilder[B])
    for xs <- this do
      var i = 0
      for x <- asTraversable(xs) do
        if i >= headSize then fail()
        bs(i) += x
        i += 1
      if i != headSize then fail()
    FArray.fromArray(bs.map(_.result()))

  def startsWith[B <: AnyRef](that: FArray[B]): Boolean = startsWith(that, 0)

  def startsWith[B <: AnyRef](that: FArray[B], offset: Int): Boolean =
    var i = offset
    var j = 0
    while i < length && j < that.length && underlying(i) == that(j) do
      i += 1
      j += 1
    j == that.length

  def mkString(init: String, sep: String, post: String): String =
    val sb = new java.lang.StringBuilder()
    sb.append(init)
    var i = 0
    while i < length do
      if i != 0 then sb.append(sep)
      sb.append(underlying(i))
      i += 1
    sb.append(post)
    sb.toString

  def mkString: String =
    mkString("", "", "")

  def mkString(sep: String): String =
    mkString("", sep, "")

  def updated[B >: A <: AnyRef](index: Int, elem: B): FArray[B] =
    require(index >= 0)
    require(index < length)
    if length == 0 then return FArray.Empty
    val ret = new Array[AnyRef](length)
    System.arraycopy(underlying, 0, ret, 0, length)
    ret(index) = elem
    FArray.create[A](ret)

  inline def unzip[A1 <: AnyRef, A2 <: AnyRef](implicit inline asPair: A => (A1, A2)): (FArray[A1], FArray[A2]) =
    val b1 = new Array[AnyRef](length)
    val b2 = new Array[AnyRef](length)
    var idx = 0
    while idx < length do
      val (x, y) = asPair(apply(idx))
      b1(idx) = x.asInstanceOf[AnyRef]
      b2(idx) = y.asInstanceOf[AnyRef]
      idx += 1
    (FArray.create(b1), FArray.create(b2))

  inline def unzip3[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef](implicit
      inline asTriple: A => (A1, A2, A3)
  ): (FArray[A1], FArray[A2], FArray[A3]) =

    val b1 = new Array[AnyRef](length)
    val b2 = new Array[AnyRef](length)
    val b3 = new Array[AnyRef](length)
    var idx = 0
    while idx < length do
      val (x, y, z) = asTriple(apply(idx))
      b1(idx) = x.asInstanceOf[AnyRef]
      b2(idx) = y.asInstanceOf[AnyRef]
      b3(idx) = z.asInstanceOf[AnyRef]
      idx += 1
    (FArray.create(b1), FArray.create(b2), FArray.create(b3))

  override def toString: String =
    mkString("List(", ", ", ")")

  override def hashCode: Int =
    var idx = 0
    val prime = 31
    var result = 1
    while idx < length do
      result = prime * result + apply(idx).##
      idx += 1
    result

  override def equals(obj: Any): Boolean =
    obj match
      case other: FArray[_] if other.length == length =>
        var idx = 0
        while idx < length do
          if apply(idx) != other(idx) then return false
          idx += 1
        true
      case _ => false

  // todo: optimize
  def corresponds[B <: AnyRef](that: FArray[B])(p: (A, B) => Boolean): Boolean =
    val a = iterator
    val b = that.iterator

    while a.hasNext && b.hasNext do if !p(a.next(), b.next()) then return false

    a.hasNext == b.hasNext

  // todo: optimize
  def span(p: A => Boolean): (FArray[A], FArray[A]) =
    val first = FArray.newBuilder[A](length)
    val second = FArray.newBuilder[A](length)
    val it = iterator
    var inFirst = true
    while it.hasNext && inFirst do
      val a = it.next()
      if p(a) then first += a
      else
        second += a
        inFirst = false
    while it.hasNext do second += it.next()
    (first.result(), second.result())

  private def occCounts[B <: AnyRef](sq: FArray[B]): mutable.Map[B, Int] =
    val occ = new mutable.HashMap[B, Int]().withDefaultValue(0)
    for y <- sq do occ(y) += 1
    occ

  def diff[B >: A <: AnyRef](that: FArray[B]): FArray[B] =
    val occ = occCounts(that)
    val ret = new Array[AnyRef](length)
    var o = 0
    for x <- this do
      val ox = occ(x) // Avoid multiple map lookups
      if ox == 0 then
        ret(o) = x
        o += 1
      else occ(x) = ox - 1
    FArray.create(ret, o)

  def intersect[B >: A <: AnyRef](that: FArray[B]): FArray[B] =
    val occ = occCounts(that)
    val ret = new Array[AnyRef](length)
    var o = 0
    for x <- this do
      val ox = occ(x) // Avoid multiple map lookups
      if ox > 0 then
        ret(o) = x
        o += 1
        occ(x) = ox - 1
    FArray.create(ret, o)

  def endsWith[B >: A <: AnyRef](that: FArray[B]): Boolean =
    if that.isEmpty then true
    else
      val i = iterator.drop(length - that.size)
      val j = that.iterator
      while i.hasNext && j.hasNext do if i.next() != j.next() then return false

      !j.hasNext

  def padTo[B >: A <: AnyRef](len: Int, elem: B): FArray[B] =
    val newLength = math.max(length, len)
    if (newLength == length) this
    else
      val ret = new Array[AnyRef](newLength)
      System.arraycopy(underlying, 0, ret, 0, length)
      var i = length
      while i < newLength do
        ret(i) = elem
        i += 1
      FArray.create(ret, newLength)

  inline def foldRight[Z](z: Z)(inline f: (A, Z) => Z): Z =
    iterator.foldRight(z)(f)

  def copyToArray[B >: A](xs: Array[B]): Int = copyToArray(xs, 0, Int.MaxValue)

  def copyToArray[B >: A](xs: Array[B], start: Int): Int = copyToArray(xs, start, Int.MaxValue)

  // todo: optimize later
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val it = iterator
    var i = start
    val end = start + math.min(len, xs.length - start)
    while (i < end && it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    i - start
  }
