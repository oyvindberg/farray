package farray

import scala.collection.mutable

/** `FBuilder[A]` — the imperative, **unboxed** accumulator for `FArray`. Opaque over the generated sealed `FBuilderBase`; the concrete backing buffer is a
  * per-element-kind `${K}Group` chosen ONCE at `FArray.newBuilder[A]` by `summonFrom` on `${K}Repr[A]` (compile-time, zero runtime dispatch).
  *
  * Why not `scala.collection.mutable.Builder[A, _]`? Its `addOne(elem: A)` is generic — `A` erases to `Object`, so every `Int`/`Long`/`Double` element is boxed
  * (`Integer.valueOf(...)`) at the call boundary before it ever reaches the backing array. `FBuilder`'s `+=`/`addOne` are `inline` and dispatch the element
  * kind statically, so the primitive is written straight into a primitive array — the same no-boxing machinery as `map`/`foldLeft`.
  *
  * The `scala.collection.mutable.Builder` interop (`.asScala`) exists for `Factory`/`to(...)` integration and is explicitly the SECOND-CLASS, boxing path.
  */
opaque type FBuilder[A] <: FBuilderBase = FBuilderBase

object FBuilder:

  /** a fresh empty builder for element kind `A` (Int/Long/Double/… or a reference type). */
  inline def apply[A](): FBuilder[A] = FArrayOps.newBuilderImpl[A]

  /** a fresh empty builder pre-sized to hold at least `n` elements without a regrow. */
  inline def apply[A](sizeHint: Int): FBuilder[A] =
    val b = FArrayOps.newBuilderImpl[A]; b.sizeHint(sizeHint); b

  extension [A](b: FBuilder[A])
    /** append one element — UNBOXED for primitives (the whole point). */
    inline def addOne(elem: A): FBuilder[A] = { FArrayOps.builderAddImpl[A](b, elem); b }

    /** `b += x` — alias for [[addOne]]. */
    inline def +=(elem: A): FBuilder[A] = { FArrayOps.builderAddImpl[A](b, elem); b }

    /** bulk-append every element of `xs`. Whole leaves are `System.arraycopy`-ed; genuine trees are materialized ONCE then arraycopy-ed — never
      * element-by-element, never boxed.
      */
    inline def addAll(xs: FArray[A]): FBuilder[A] = { b.addNode(xs.asInstanceOf[FBase]); b }

    /** `b ++= xs` — alias for [[addAll]]. */
    inline def ++=(xs: FArray[A]): FBuilder[A] = { b.addNode(xs.asInstanceOf[FBase]); b }

    /** grow the backing array so at least `n` elements fit without a further regrow. */
    inline def sizeHint(n: Int): Unit = b.sizeHint(n)

    /** reset to empty, KEEPING the backing array (so a reused builder pays no re-alloc). */
    inline def clear(): Unit = b.clear()

    /** number of elements accumulated so far. */
    inline def length: Int = b.knownSize

    inline def knownSize: Int = b.knownSize
    inline def isEmpty: Boolean = b.knownSize == 0
    inline def nonEmpty: Boolean = b.knownSize > 0

    /** hand the accumulated buffer to an `FArray` leaf WITHOUT a defensive copy — the house slack rule (`trimLeaf`: keep the buffer verbatim unless less than a
      * quarter full, else one `Arrays.copyOf`). After `result()` the builder still owns the array; keep filling only after a [[clear]].
      */
    inline def result(): FArray[A] = b.result.asInstanceOf[FArray[A]]

    /** SECOND-CLASS interop: a real `scala.collection.mutable.Builder[A, FArray[A]]` for `Factory` / `to(FArray)` integration. Its `addOne` takes an erased `A`
      * and therefore BOXES each primitive element — use the native `FBuilder` surface on hot paths.
      */
    def asScala: mutable.Builder[A, FArray[A]] = new mutable.Builder[A, FArray[A]]:
      def addOne(elem: A): this.type = { b.addBoxed(elem); this }
      override def addAll(xs: IterableOnce[A]): this.type =
        val it = xs.iterator; while it.hasNext do b.addBoxed(it.next()); this
      override def sizeHint(n: Int): Unit = b.sizeHint(n)
      def clear(): Unit = b.clear()
      def result(): FArray[A] = b.result.asInstanceOf[FArray[A]]
