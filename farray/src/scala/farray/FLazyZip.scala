package farray

/** Inline-only markers behind `xs.lazyZip(ys)` / `xs.lazyZip(ys).lazyZip(zs)` (stdlib `lazyZip`
  * shape: the operation lambdas are multi-parameter — `(a, b) => c`, `(a, b, c) => d` — NOT tuple).
  *
  * Every terminal is `inline` and compiles to ONE `while`/`tabulate` loop over the MIN length of the
  * inputs, reading each side through the kind-specialized `apply` (unboxed for a primitive element
  * type) and inlining the user lambda — no intermediate collection, and no tuple is allocated unless
  * the user's own lambda takes one (`collect`'s `PartialFunction`, and the materializing
  * `zipWithIndex` / `toFArray`). The wrapper itself never escapes a call site.
  *
  * The terminals are `extension` methods (in the companion), NOT instance methods, ON PURPOSE. When
  * dotty inlines a direct `xs.lazyZip(ys).<terminal>(…)`, the receiver `new FLazyZip2(xs, ys)` is bound
  * once at expansion. As an INSTANCE-method receiver that binding is a synthetic `this`-proxy val whose
  * `FLazyZip2[A,B]` TypeTree is created from the class type with NO source position — tripping dotty's
  * `-Ycheck:all` "position not set for farray.FLazyZip2[…]" assertion (Inliner.computeThisBindings:
  * `ValDef(selfSym, …).withSpan(selfSym.span)`, and the synthetic this-proxy sym has no span). As an
  * EXTENSION receiver the same value is bound through `Inliner.paramBindingDef` — a normal inline
  * parameter proxy that carries the argument's span — so no positionless marker TypeTree is ever
  * synthesized. (ycheck-tests reproduces the old assertion and gates the fix.) */
final class FLazyZip2[A, B](private[farray] val xs: FArray[A], private[farray] val ys: FArray[B])

object FLazyZip2:
  extension [A, B](inline self: FLazyZip2[A, B])
    private inline def n: Int = math.min(self.xs.length, self.ys.length)

    inline def map[C](inline f: (A, B) => C): FArray[C] =
      FArray.tabulate(self.n)(i => f(self.xs(i), self.ys(i)))

    inline def foreach(inline f: (A, B) => Unit): Unit =
      val len = self.n; var i = 0
      while i < len do { f(self.xs(i), self.ys(i)); i += 1 }

    inline def forall(inline p: (A, B) => Boolean): Boolean =
      val len = self.n; var i = 0; var r = true
      while r && i < len do { r = p(self.xs(i), self.ys(i)); i += 1 }
      r

    inline def exists(inline p: (A, B) => Boolean): Boolean =
      val len = self.n; var i = 0; var r = false
      while !r && i < len do { r = p(self.xs(i), self.ys(i)); i += 1 }
      r

    inline def foldLeft[Z](z: Z)(inline op: (Z, A, B) => Z): Z =
      val len = self.n; var acc = z; var i = 0
      while i < len do { acc = op(acc, self.xs(i), self.ys(i)); i += 1 }
      acc

    inline def flatMap[C](inline f: (A, B) => FArray[C]): FArray[C] =
      val len = self.n; var acc: FArray[C] = FArray.empty[C]; var i = 0
      while i < len do { acc = acc ++ f(self.xs(i), self.ys(i)); i += 1 }
      acc

    /** ONE fused pass over the pairs — no intermediate tuple ARRAY. Fuses over the O(1) `RangeNode` of
      * indices (no backing int[]): each index maps to its per-element tuple `(A, B)` (acceptable — the
      * `PartialFunction` inherently receives one), and `fuse.collect` filters+applies straight into the
      * unboxed result (primitive output `C` lands in a primitive leaf). */
    inline def collect[C](inline pf: PartialFunction[(A, B), C]): FArray[C] =
      // ONE fused lock-step pass: `fuse.zip` pairs the two sources without building the pair array, and
      // `inline pf` splices the PartialFunction LITERAL into `fuse.collect`, so the macro picks its
      // pattern/guard/body apart (no runtime PF, no boxing). Both `that` args are plain FArray values —
      // no lambda captures an inline `p$proxy` out of the macro's scope (which a `.map(i => (xs(i),…))`
      // over the inline receiver's fields, or a by-value PF, would).
      self.xs.fuse.zip(self.ys).collect(pf).run

    inline def zipWithIndex: FArray[(A, B, Int)] =
      FArray.tabulate(self.n)(i => (self.xs(i), self.ys(i), i))

    inline def toFArray: FArray[(A, B)] =
      FArray.tabulate(self.n)(i => (self.xs(i), self.ys(i)))

    inline def lazyZip[C](zs: FArray[C]): FLazyZip3[A, B, C] = new FLazyZip3(self.xs, self.ys, zs)

final class FLazyZip3[A, B, C](
    private[farray] val xs: FArray[A],
    private[farray] val ys: FArray[B],
    private[farray] val zs: FArray[C]
)

object FLazyZip3:
  extension [A, B, C](inline self: FLazyZip3[A, B, C])
    private inline def n: Int = math.min(self.xs.length, math.min(self.ys.length, self.zs.length))

    inline def map[D](inline f: (A, B, C) => D): FArray[D] =
      FArray.tabulate(self.n)(i => f(self.xs(i), self.ys(i), self.zs(i)))

    inline def foreach(inline f: (A, B, C) => Unit): Unit =
      val len = self.n; var i = 0
      while i < len do { f(self.xs(i), self.ys(i), self.zs(i)); i += 1 }

    inline def forall(inline p: (A, B, C) => Boolean): Boolean =
      val len = self.n; var i = 0; var r = true
      while r && i < len do { r = p(self.xs(i), self.ys(i), self.zs(i)); i += 1 }
      r

    inline def exists(inline p: (A, B, C) => Boolean): Boolean =
      val len = self.n; var i = 0; var r = false
      while !r && i < len do { r = p(self.xs(i), self.ys(i), self.zs(i)); i += 1 }
      r

    inline def foldLeft[Z](z: Z)(inline op: (Z, A, B, C) => Z): Z =
      val len = self.n; var acc = z; var i = 0
      while i < len do { acc = op(acc, self.xs(i), self.ys(i), self.zs(i)); i += 1 }
      acc

    inline def flatMap[D](inline f: (A, B, C) => FArray[D]): FArray[D] =
      val len = self.n; var acc: FArray[D] = FArray.empty[D]; var i = 0
      while i < len do { acc = acc ++ f(self.xs(i), self.ys(i), self.zs(i)); i += 1 }
      acc

    /** Fused `collect` over the triples. A 3-way lock-step needs the intermediate element type `(A, B, C)`
      * to survive a second fuse stage, but `Fuse[+A]`'s covariance widens it to `Any` there (the element
      * sits contravariantly in a stage lambda). So we materialize the triples once via `toFArray` and fuse
      * the single `collect` stage, whose `inline pf` literal pins the element type and is picked apart by
      * the macro (no runtime PF, no boxing). */
    inline def collect[D](inline pf: PartialFunction[(A, B, C), D]): FArray[D] =
      self.toFArray.fuse.collect(pf).run

    inline def zipWithIndex: FArray[(A, B, C, Int)] =
      FArray.tabulate(self.n)(i => (self.xs(i), self.ys(i), self.zs(i), i))

    inline def toFArray: FArray[(A, B, C)] =
      FArray.tabulate(self.n)(i => (self.xs(i), self.ys(i), self.zs(i)))
