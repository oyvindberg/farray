package farray

/** Inline-only markers behind `xs.lazyZip(ys)` / `xs.lazyZip(ys).lazyZip(zs)` (stdlib `lazyZip`
  * shape: the operation lambdas are multi-parameter — `(a, b) => c`, `(a, b, c) => d` — NOT tuple).
  *
  * Every terminal is `inline` and compiles to ONE `while`/`tabulate` loop over the MIN length of the
  * inputs, reading each side through the kind-specialized `apply` (unboxed for a primitive element
  * type) and inlining the user lambda — no intermediate collection, and no tuple is allocated unless
  * the user's own lambda takes one (`collect`'s `PartialFunction`, and the materializing
  * `zipWithIndex` / `toFArray`). The wrapper itself never escapes a call site.
  */
final class FLazyZip2[A, B](private[farray] val xs: FArray[A], private[farray] val ys: FArray[B]):
  private inline def n: Int = math.min(xs.length, ys.length)

  inline def map[C](inline f: (A, B) => C): FArray[C] =
    FArray.tabulate(n)(i => f(xs(i), ys(i)))

  inline def foreach(inline f: (A, B) => Unit): Unit =
    val len = n; var i = 0
    while i < len do { f(xs(i), ys(i)); i += 1 }

  inline def forall(inline p: (A, B) => Boolean): Boolean =
    val len = n; var i = 0; var r = true
    while r && i < len do { r = p(xs(i), ys(i)); i += 1 }
    r

  inline def exists(inline p: (A, B) => Boolean): Boolean =
    val len = n; var i = 0; var r = false
    while !r && i < len do { r = p(xs(i), ys(i)); i += 1 }
    r

  inline def foldLeft[Z](z: Z)(inline op: (Z, A, B) => Z): Z =
    val len = n; var acc = z; var i = 0
    while i < len do { acc = op(acc, xs(i), ys(i)); i += 1 }
    acc

  inline def flatMap[C](inline f: (A, B) => FArray[C]): FArray[C] =
    val len = n; var acc: FArray[C] = FArray.empty[C]; var i = 0
    while i < len do { acc = acc ++ f(xs(i), ys(i)); i += 1 }
    acc

  /** the pair requires a tuple (the user `PartialFunction` matches on it); one pass over the pairs. */
  inline def collect[C](pf: PartialFunction[(A, B), C]): FArray[C] = this.toFArray.collect(pf)

  inline def zipWithIndex: FArray[(A, B, Int)] =
    FArray.tabulate(n)(i => (xs(i), ys(i), i))

  inline def toFArray: FArray[(A, B)] =
    FArray.tabulate(n)(i => (xs(i), ys(i)))

  inline def lazyZip[C](zs: FArray[C]): FLazyZip3[A, B, C] = new FLazyZip3(xs, ys, zs)

final class FLazyZip3[A, B, C](
    private[farray] val xs: FArray[A],
    private[farray] val ys: FArray[B],
    private[farray] val zs: FArray[C]
):
  private inline def n: Int = math.min(xs.length, math.min(ys.length, zs.length))

  inline def map[D](inline f: (A, B, C) => D): FArray[D] =
    FArray.tabulate(n)(i => f(xs(i), ys(i), zs(i)))

  inline def foreach(inline f: (A, B, C) => Unit): Unit =
    val len = n; var i = 0
    while i < len do { f(xs(i), ys(i), zs(i)); i += 1 }

  inline def forall(inline p: (A, B, C) => Boolean): Boolean =
    val len = n; var i = 0; var r = true
    while r && i < len do { r = p(xs(i), ys(i), zs(i)); i += 1 }
    r

  inline def exists(inline p: (A, B, C) => Boolean): Boolean =
    val len = n; var i = 0; var r = false
    while !r && i < len do { r = p(xs(i), ys(i), zs(i)); i += 1 }
    r

  inline def foldLeft[Z](z: Z)(inline op: (Z, A, B, C) => Z): Z =
    val len = n; var acc = z; var i = 0
    while i < len do { acc = op(acc, xs(i), ys(i), zs(i)); i += 1 }
    acc

  inline def flatMap[D](inline f: (A, B, C) => FArray[D]): FArray[D] =
    val len = n; var acc: FArray[D] = FArray.empty[D]; var i = 0
    while i < len do { acc = acc ++ f(xs(i), ys(i), zs(i)); i += 1 }
    acc

  inline def collect[D](pf: PartialFunction[(A, B, C), D]): FArray[D] = this.toFArray.collect(pf)

  inline def zipWithIndex: FArray[(A, B, C, Int)] =
    FArray.tabulate(n)(i => (xs(i), ys(i), zs(i), i))

  inline def toFArray: FArray[(A, B, C)] =
    FArray.tabulate(n)(i => (xs(i), ys(i), zs(i)))
