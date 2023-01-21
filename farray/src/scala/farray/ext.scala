package farray

extension [A <: AnyRef](as: FArray[A]) {
  inline def mapNotNone[B <: AnyRef](inline f: A => Option[B]): FArray[B] =
    as.map(f).collect { case Some(b) => b }

  inline def firstDefined[B](inline f: A => Option[B]): Option[B] = {
    var idx = 0
    var ret = Option.empty[B]
    while (idx < as.length && ret.isEmpty) {
      val ob = f(as(idx))
      if (ob.isDefined) ret = ob
      idx += 1
    }
    ret
  }

  inline def filterConserve(inline p: A => Boolean): FArray[A] = {
    if as.isEmpty then as
    else
      val ret = FArray.newBuilder[A](as.length)
      var i = 0
      var changed = false
      while i < as.length do
        val a = as(i)
        if p(a) then {
          ret += a
          changed = true
        }
        i += 1

      if (changed) ret.result() else as
  }

  inline def mapConserve[B <: AnyRef](inline f: A => B): FArray[B] =
    if as.isEmpty then as.asInstanceOf[FArray[B]]
    else
      val newArray = new Array[AnyRef](as.length)
      var i = 0
      var foundDifferent = false
      while i < as.length do
        val before: A = as(i)
        val after: B = f(before)
        if before ne after then foundDifferent = true

        newArray(i) = after
        i += 1

      if foundDifferent then FArray.create[B](newArray) else as.asInstanceOf[FArray[B]]

  def zipWithConserve[U <: AnyRef, V <: A](ys: FArray[U])(f: (A, U) => V): FArray[V] = {
    if as.isEmpty then as.asInstanceOf[FArray[V]]
    else
      val newArray = new Array[AnyRef](as.length)
      var i = 0
      var foundDifferent = false
      while i < as.length do
        val before: A = as(i)
        val u: U = ys(i)
        val after: V = f(before, u)
        if before ne after then foundDifferent = true

        newArray(i) = after
        i += 1

      if foundDifferent then FArray.create[V](newArray) else as.asInstanceOf[FArray[V]]

  }
}
