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
}
