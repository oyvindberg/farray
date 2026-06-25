package farray

/** PROTOTYPE surface for the shared-Java-traverse design (`Proto.java`) — evaluated A/B against the
  * committed inlined-walk `mapImpl`/`foldLeftImpl`. `inline` so the caller's op splices into the
  * anonymous `run`/`one` bodies and primitives stay unboxed; the small two-method object is allocated
  * once per call (the accepted cost). Int->Int map and Int->Ref(Z) foldLeft only (the slice). */
object Prototype:

  inline def mapProtoInt(xs: FBase)(inline g: Int => Int): FBase =
    xs match
      case leaf: IntArr =>
        val a = leaf.arr
        val out = new Array[Int](a.length)
        var i = 0
        while i < a.length do { out(i) = g(a(i)); i += 1 }
        new IntArr(out, a.length)
      case o: IntOne => new IntOne(g(o.elem))
      case e: Empty  => e
      case _         => Proto.traverseMapIntInt(xs, xs.length, (v: Int) => g(v))

  inline def foldLeftProtoInt[Z](xs: FBase, z: Z)(inline op: (Z, Int) => Z): Z =
    xs match
      case leaf: IntArr =>
        val a = leaf.arr
        val n = leaf.length
        var acc = z
        var i = 0
        while i < n do { acc = op(acc, a(i)); i += 1 }
        acc
      case o: IntOne => op(z, o.elem)
      case _: Empty  => z
      case _         => Proto.traverseFoldIntRef(xs, z, (acc: Z, v: Int) => op(acc, v))

  inline def mapProtoRef[A, B](xs: FBase)(inline g: A => B): FBase =
    xs match
      case leaf: RefArr =>
        val a = leaf.arr
        val out = new Array[Object](a.length)
        var i = 0
        while i < a.length do
          out(i) = g(a(i).asInstanceOf[A]).asInstanceOf[Object]
          i += 1
        new RefArr(out, a.length)
      case one: RefOne =>
        new RefOne(g(one.elem.asInstanceOf[A]).asInstanceOf[Object])
      case e: Empty =>
        e
      case _ =>
        // tree fallback (not exercised by the flat megamorphic benchmark); correctness via applyBoxed
        val n = xs.length
        val out = new Array[Object](n)
        var i = 0
        while i < n do
          out(i) = g(xs.applyBoxed(i).asInstanceOf[A]).asInstanceOf[Object]
          i += 1
        if n == 0 then Empty.INSTANCE
        else if n == 1 then new RefOne(out(0))
        else new RefArr(out, n)

  // SHARED (non-inline) Ref leaf-map: the loop lives in ONE method, called per map site like Array.map,
  // NOT inlined. Hypothesis: JIT inlines it for a lone monomorphic site (single map stays fast) but
  // keeps it shared across many sites (no per-call bloat -> no @100k multi-map cliff).
  def mapRefShared(xs: FBase, f: Object => Object): FBase =
    xs match
      case leaf: RefArr =>
        val a = leaf.arr
        val out = new Array[Object](a.length)
        var i = 0
        while i < a.length do
          out(i) = f(a(i))
          i += 1
        new RefArr(out, a.length)
      case one: RefOne =>
        new RefOne(f(one.elem))
      case e: Empty =>
        e
      case _ =>
        val n = xs.length
        val out = new Array[Object](n)
        var i = 0
        while i < n do
          out(i) = f(xs.applyBoxed(i))
          i += 1
        if n == 0 then Empty.INSTANCE
        else if n == 1 then new RefOne(out(0))
        else new RefArr(out, n)

  inline def mapProtoRefShared[A, B](xs: FBase)(inline g: A => B): FBase =
    mapRefShared(xs, (v: Object) => g(v.asInstanceOf[A]).asInstanceOf[Object])

  // SMALLER inline: ONLY the leaf-array case is inlined; Empty/One/tree fall to the shared method.
  // Less call-site bytecode than the full Empty/One/leaf match — does the 8-map @100k cliff shrink, or
  // is the inlined LOOP itself the cause (in which case this still cliffs)?
  inline def mapProtoRefSmall[A, B](xs: FBase)(inline g: A => B): FBase =
    xs match
      case leaf: RefArr =>
        val a = leaf.arr
        val out = new Array[Object](a.length)
        var i = 0
        while i < a.length do
          out(i) = g(a(i).asInstanceOf[A]).asInstanceOf[Object]
          i += 1
        new RefArr(out, a.length)
      case _ =>
        mapRefShared(xs, (v: Object) => g(v.asInstanceOf[A]).asInstanceOf[Object])

  // INT shared (not-inline) leaf-map — does NOT-INLINE stay UNBOXED for primitives? `f: Int => Int`
  // compiles to the specialized Function1 (apply$mcII$sp(int): int), so even a megamorphic virtual call
  // is int->int with NO boxing. Measure the B/op: shared should alloc only int[] outputs, like inlined.
  def mapIntShared(xs: FBase, f: Int => Int): FBase =
    xs match
      case leaf: IntArr =>
        val a = leaf.arr
        val out = new Array[Int](a.length)
        var i = 0
        while i < a.length do
          out(i) = f(a(i))
          i += 1
        new IntArr(out, a.length)
      case _ =>
        val n = xs.length
        val out = new Array[Int](n)
        var i = 0
        while i < n do
          out(i) = f(xs.applyBoxed(i).asInstanceOf[Int])
          i += 1
        if n == 0 then Empty.INSTANCE
        else if n == 1 then new IntOne(out(0))
        else new IntArr(out, n)

  inline def mapProtoIntShared(xs: FBase)(inline g: Int => Int): FBase =
    mapIntShared(xs, (v: Int) => g(v))
