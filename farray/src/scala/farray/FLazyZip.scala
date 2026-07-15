package farray

import scala.compiletime.summonFrom

/** One fused `collect` pass shared by [[FLazyZip2]] / [[FLazyZip3]]: walk `0 until len`, build the
  * per-element tuple via `pair` (the PartialFunction inherently matches a tuple — that per-element
  * tuple is acceptable; there is NO intermediate tuple array), test it with `isDefinedAt` and, when
  * matched, `apply` the result into a KIND-SPECIALIZED builder so a primitive output `C` lands in a
  * primitive backing array (unwrapped once via the `Repr` evidence — no boxed result array). */
private inline def lazyZipCollect[T, C](len: Int, inline pair: Int => T, pf: PartialFunction[T, C]): FArray[C] =
  summonFrom {
    case r: IntRepr[C] =>
      val b = new IntFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: LongRepr[C] =>
      val b = new LongFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: DoubleRepr[C] =>
      val b = new DoubleFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: FloatRepr[C] =>
      val b = new FloatFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: ShortRepr[C] =>
      val b = new ShortFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: ByteRepr[C] =>
      val b = new ByteFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: CharRepr[C] =>
      val b = new CharFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case r: BooleanRepr[C] =>
      val b = new BooleanFArrayBuilder; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(r.unwrap(pf(t))); i += 1 }
      b.result().asInstanceOf[FArray[C]]
    case _ =>
      val b = new RefFArrayBuilder[C]; var i = 0
      while i < len do { val t = pair(i); if pf.isDefinedAt(t) then b.addOne(pf(t)); i += 1 }
      b.result().asInstanceOf[FArray[C]]
  }

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

  /** ONE fused pass over the pairs — no intermediate tuple ARRAY. The `PartialFunction` inherently
    * receives a per-element tuple `(A, B)` (acceptable); each is tested with `isDefinedAt` and, when
    * matched, `apply`d into a kind-specialized builder (unboxed output for a primitive `C`). */
  inline def collect[C](pf: PartialFunction[(A, B), C]): FArray[C] =
    lazyZipCollect[(A, B), C](n, i => (xs(i), ys(i)), pf)

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

  /** ONE fused pass over the triples — no intermediate tuple ARRAY (mirrors [[FLazyZip2.collect]]). */
  inline def collect[D](pf: PartialFunction[(A, B, C), D]): FArray[D] =
    lazyZipCollect[(A, B, C), D](n, i => (xs(i), ys(i), zs(i)), pf)

  inline def zipWithIndex: FArray[(A, B, C, Int)] =
    FArray.tabulate(n)(i => (xs(i), ys(i), zs(i), i))

  inline def toFArray: FArray[(A, B, C)] =
    FArray.tabulate(n)(i => (xs(i), ys(i), zs(i)))
