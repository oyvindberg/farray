package farray

import java.util

opaque type Builder[A <: AnyRef] = util.ArrayList[A]

object Builder {
  def empty[A <: AnyRef](): Builder[A] = empty(32)

  def empty[A <: AnyRef](initialCapacity: Int): Builder[A] = new Builder[A](initialCapacity)

  def from[A <: AnyRef](as: FArray[A]): Builder[A] =
    from(as, as.length)

  def from[A <: AnyRef](as: FArray[A], initialCapacity: Int): Builder[A] = {
    val ret = new Builder[A](initialCapacity)
    ret ++= as
    ret
  }

  extension [A <: AnyRef](buf: Builder[A]) {

    def size =
      buf.size()

    def length =
      buf.size()

    def +=(elem: A): this.type = {
      buf.add(elem)
      this
    }

    def append(elem: A): this.type = {
      buf.add(elem)
      this
    }

    def clear(): Unit =
      buf.clear()

    def result(): FArray[A] =
      FArray.create(buf.toArray)

    def ++=(as: FArray[A]): this.type = {
      var idx = 0
      while idx < as.length do {
        buf += as(idx)
        idx += 1
      }
      this
    }

    def ++=(as: Seq[A]): this.type = {
      var idx = 0
      while idx < as.length do {
        buf.add(as(idx))
        idx += 1
      }
      this
    }

    def ++=(as: Array[A]): this.type = {
      var idx = 0
      while idx < as.length do {
        buf.add(as(idx))
        idx += 1
      }
      this
    }

    def ++=(it: Iterator[A]): this.type = {
      while it.hasNext do {
        buf.add(it.next())
      }
      this
    }

    def ++=(as: Option[A]): this.type = {
      as match {
        case Some(a) => buf.add(a)
        case None => ()
      }
      this
    }

    def ++=(fb: Builder[A]): this.type = {
      buf.addAll(fb)
      this
    }

    def update(idx: Int, value: A): Unit = buf.set(idx, value)

    // todo: causes issues when called `apply`
    def get(idx: Int): A = buf.get(idx)

    def head: A = buf.get(0)

    def last: A = buf.get(buf.size() - 1)

    inline def foreach(inline f: A => Unit): Unit = {
      var i = 0
      val size = buf.size()
      while i < size do {
        f(get(i))
        i += 1
      }
    }

    inline def exists(inline p: A => Boolean): Boolean = {
      var idx = 0
      var found = false
      val size = buf.size()
      while idx < size && !found do {
        if p(buf.get(idx)) then {
          found = true
        }
        idx += 1
      }
      found
    }

    inline def isEmpty: Boolean =
      buf.size() == 0

    inline def nonEmpty: Boolean =
      !isEmpty
  }
}
