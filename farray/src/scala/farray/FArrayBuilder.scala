package farray

import java.util
import scala.jdk.CollectionConverters.*

opaque type FArrayBuilder[A <: AnyRef] = util.ArrayList[A]

object FArrayBuilder:
  def empty[A <: AnyRef](): FArrayBuilder[A] = empty(32)

  def empty[A <: AnyRef](initialCapacity: Int): FArrayBuilder[A] = new FArrayBuilder[A](initialCapacity)

  def from[A <: AnyRef](as: FArray[A]): FArrayBuilder[A] =
    from(as, as.length)

  def from[A <: AnyRef](as: FArray[A], initialCapacity: Int): FArrayBuilder[A] =
    val ret = new FArrayBuilder[A](initialCapacity)
    ret ++= as
    ret

  extension [A <: AnyRef](buf: FArrayBuilder[A])
    def size: Int =
      buf.size()

    def length: Int =
      buf.size()

    def +=(elem: A): FArrayBuilder[A] =
      buf.add(elem)
      buf

    def append(elem: A): FArrayBuilder[A] =
      buf.add(elem)
      buf

    def clear(): Unit =
      buf.clear()

    def result(): FArray[A] =
      FArray.create(buf.toArray)

    def ++=(as: FArray[A]): FArrayBuilder[A] =
      var idx = 0
      while idx < as.length do
        buf += as(idx)
        idx += 1
      buf

    def ++=(as: Seq[A]): FArrayBuilder[A] =
      var idx = 0
      while idx < as.length do
        buf.add(as(idx))
        idx += 1
      buf

    def ++=(as: Array[A]): FArrayBuilder[A] =
      var idx = 0
      while idx < as.length do
        buf.add(as(idx))
        idx += 1
      buf

    def ++=(it: Iterator[A]): FArrayBuilder[A] =
      while it.hasNext do buf.add(it.next())
      buf

    def ++=(as: Option[A]): FArrayBuilder[A] =
      as match
        case Some(a) => buf.add(a)
        case None    => ()
      buf

    def ++=(fb: FArrayBuilder[A]): FArrayBuilder[A] =
      buf.addAll(fb)
      buf

    def apply(idx: Int): A = buf.get(idx)

    def update(idx: Int, value: A): Unit = buf.set(idx, value)

    // todo: causes issues when called `apply`
    def get(idx: Int): A = buf.get(idx)

    def head: A = buf.get(0)

    def last: A = buf.get(buf.size() - 1)

    inline def foreach(inline f: A => Unit): Unit =
      var i = 0
      val size = buf.size()
      while i < size do
        f(get(i))
        i += 1

    inline def exists(inline p: A => Boolean): Boolean =
      var idx = 0
      var found = false
      val size = buf.size()
      while idx < size && !found do
        if p(buf.get(idx)) then found = true
        idx += 1
      found

    inline def isEmpty: Boolean =
      buf.size() == 0

    inline def nonEmpty: Boolean =
      !isEmpty

    def javaIterator: util.Iterator[A] =
      buf.iterator()

    def iterator: Iterator[A] =
      javaIterator.asScala

    def dropRightInPlace(n: Int): Unit = {
      var left = n
      while (left > 0 && nonEmpty) {
        buf.remove(length - 1)
        left -= 1
      }
    }
