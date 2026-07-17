package farray

/** COMPILE-ONLY gate (project `ycheck-tests`, built with `-Ycheck:all`). Each def below is a DIRECT `xs.lazyZip(ys).<terminal>(…)` (and 3-ary chained) call
  * site whose inline expansion previously lifted the `FLazyZip2`/`FLazyZip3` marker receiver into a synthetic val with a positionless TypeTree, tripping
  * dotty's "position not set for farray.FLazyZip2[…]" assertion under -Ycheck:all when the compiler's own sources were recompiled. If this project compiles
  * clean, the marker no longer materializes as a val at inline expansion, so direct lazyZip terminals are safe to use inside a codebase built with -Ycheck.
  */
object YCheckCompile:

  // ---- 2-ary terminals (mirror the known dotty repro sites: foreach/forall/map/foldLeft/…) ----
  def zipForeach(xs: FArray[Int], ys: FArray[String]): Int =
    var s = 0; xs.lazyZip(ys).foreach((a, b) => s += a + b.length); s
  def zipForall(xs: FArray[Int], ys: FArray[Int]): Boolean =
    xs.lazyZip(ys).forall((a, b) => a <= b)
  def zipExists(xs: FArray[Int], ys: FArray[Int]): Boolean =
    xs.lazyZip(ys).exists((a, b) => a == b)
  def zipMap(xs: FArray[Int], ys: FArray[Int]): FArray[Int] =
    xs.lazyZip(ys).map((a, b) => a + b)
  def zipMapRef(xs: FArray[String], ys: FArray[Int]): FArray[String] =
    xs.lazyZip(ys).map((a, b) => a * b)
  def zipFold(xs: FArray[Int], ys: FArray[Int]): Int =
    xs.lazyZip(ys).foldLeft(0)((z, a, b) => z + a * b)
  def zipFlatMap(xs: FArray[Int], ys: FArray[Int]): FArray[Int] =
    xs.lazyZip(ys).flatMap((a, b) => FArray(a, b))
  def zipCollect(xs: FArray[Int], ys: FArray[Int]): FArray[Int] =
    xs.lazyZip(ys).collect { case (a, b) if a > b => a - b }
  def zipWithIndex(xs: FArray[Int], ys: FArray[String]): FArray[(Int, String, Int)] =
    xs.lazyZip(ys).zipWithIndex
  def zipToFArray(xs: FArray[Int], ys: FArray[String]): FArray[(Int, String)] =
    xs.lazyZip(ys).toFArray

  // ---- 3-ary chained terminals (FLazyZip3) ----
  def zip3Map(xs: FArray[Int], ys: FArray[Int], zs: FArray[Int]): FArray[Int] =
    xs.lazyZip(ys).lazyZip(zs).map((a, b, c) => a + b + c)
  def zip3Foreach(xs: FArray[Int], ys: FArray[Int], zs: FArray[Int]): Int =
    var s = 0; xs.lazyZip(ys).lazyZip(zs).foreach((a, b, c) => s += a + b + c); s
  def zip3Forall(xs: FArray[Int], ys: FArray[Int], zs: FArray[Int]): Boolean =
    xs.lazyZip(ys).lazyZip(zs).forall((a, b, c) => a <= b && b <= c)
  def zip3Fold(xs: FArray[Int], ys: FArray[Int], zs: FArray[Int]): Int =
    xs.lazyZip(ys).lazyZip(zs).foldLeft(0)((acc, a, b, c) => acc + a + b + c)
  def zip3ToFArray(xs: FArray[Int], ys: FArray[Int], zs: FArray[Int]): FArray[(Int, Int, Int)] =
    xs.lazyZip(ys).lazyZip(zs).toFArray

  // ---- val-bound marker (Desugar.scala:2342 shape): the marker is explicitly named, then a terminal ----
  def valBound(xs: FArray[Int], ys: FArray[Int]): FArray[Int] =
    val z = xs.lazyZip(ys)
    z.map((a, b) => a + b)

  // ---- generic element types (abstract A/B): boxed-fallback path still must not leak the marker ----
  def zipGeneric[A, B](xs: FArray[A], ys: FArray[B]): Int =
    var s = 0; xs.lazyZip(ys).foreach((a, b) => s += 1); s

  // ---- FArray.collect with a DESTRUCTURING pattern (non-simple-binder -> the CollectMacro match/lambda
  // path) whose guard AND body capture an outer free var. This is the CheckRealizable.scala shape that
  // crashed the reference compiler in LambdaLift (`key not found: method $anonfun`) — the freshened
  // guard/body must be re-owned to the generated lambda, not left under spliceOwner. ----
  case class Bounds(lo: Int, hi: Int)
  def collectDestructure(xs: FArray[Bounds], base: Int): FArray[Int] =
    xs.collect { case b @ Bounds(lo, hi) if lo < hi => lo + hi + base }
  def collectTuple(xs: FArray[(Int, String)], k: Int): FArray[String] =
    xs.collect { case (n, s) if n > k => s + n }
  def collectMultiCase(xs: FArray[Bounds], base: Int): FArray[Int] =
    xs.collect { case Bounds(0, hi) => hi + base; case b @ Bounds(lo, _) if lo > base => lo }

  // faithful CheckRealizable shape: guard calls a method taking a `(using Ctx)` implicit (captured), the
  // collect sits inside an outer `match` whose bound var is captured in the body, all under a method with
  // its own `(using Ctx)`.
  class Ctx
  extension (b: Bounds) def ok(using Ctx): Boolean = b.lo <= b.hi
  def collectUsingGuard(base: Bounds, arg: FArray[Bounds])(using Ctx): FArray[Bounds] =
    base match
      case Bounds(0, 0) => FArray()
      case other        =>
        arg.collect { case b @ Bounds(lo, hi) if !b.ok => Bounds(lo + other.lo, hi) }
