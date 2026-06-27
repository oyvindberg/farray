package farray

/** `FSet[A]` — an immutable Set, opaque type over the sealed Java core [[SBase]] (zero wrapper allocation, the
  * no-import binding preserved). A sibling of `FArray`, sharing FArray's proven runtime: per-element-kind
  * specialization (Int/Long/Double stored unboxed in primitive arrays, Ref a typed `Object[]`), `summonFrom`
  * kind dispatch on the REUSED `Repr[A]`, and the "small shared non-inline leaf method" discipline — the leaf
  * membership walk / build / size live in `object FSetOps`, the inline surface only dispatches the kind.
  *
  * INVARIANT in `A` (not `+A`): a hash/equality set uses `A` in `contains(a: A)` (contravariant) and iteration
  * (covariant), so it cannot be soundly covariant — the design's settled choice (a covariant `contains(elem: A)`
  * does not typecheck). The slice's `contains`/`incl`/`excl` take `A` directly; widening (`contains[B >: A]`,
  * etc.) is a NEXT-STEP if a covariant surface is wanted.
  *
  * SCAFFOLD/M1 SCOPE — the green vertical slice: construction (`empty`/`apply`/`fromArray`/`fromFArray`/
  * `from`), `contains`/`apply`, `size`/`isEmpty`/`nonEmpty`, `incl`/`excl` (with `+`/`-` infix aliases). The
  * lazy algebra
  * (`++`/`&`/`diff`/`xor`), the materialize/traversal ops (`foreach`/`iterator`/`map`/`filter`/`toFArray`),
  * the Hash/Dense leaves, and the `FSetView`/`FSortedSet` siblings are intentionally NOT here yet.
  */
opaque type FSet[A] <: AnyRef = SBase

object FSet:

  inline def empty[A]: FSet[A] = FSetOps.emptyImpl[A]
  // small-arity overloads avoid the varargs Seq + boxing (mirrors FArray); >3 falls back to varargs `from`.
  inline def apply[A](a: A): FSet[A] = FSetOps.fromValues1[A](a)
  inline def apply[A](a: A, b: A): FSet[A] = FSetOps.fromValues2[A](a, b)
  inline def apply[A](a: A, b: A, c: A): FSet[A] = FSetOps.fromValues3[A](a, b, c)
  inline def apply[A](a: A, b: A, c: A, rest: A*): FSet[A] =
    val b0 = scala.collection.mutable.ArrayBuffer[A](a, b, c)
    b0 ++= rest
    FSetOps.fromImpl[A](b0)

  inline def fromArray[A](as: Array[A]): FSet[A] = FSetOps.fromArrayImpl[A](as)
  // M1 bridge via fa.iterator (the opaque FArray can only be widened to FBase inside FArray.scala's own scope,
  // not in FSetOps); a direct unboxed leaf read off the FArray's ${K}Arr is a NEXT-STEP zero-copy bridge.
  inline def fromFArray[A](fa: FArray[A]): FSet[A] = FSetOps.fromImpl[A](fa.iterator)
  inline def from[A](it: IterableOnce[A]): FSet[A] = FSetOps.fromImpl[A](it)
  inline def fromIterable[A](it: Iterable[A]): FSet[A] = FSetOps.fromImpl[A](it)

  // ---- predicate / range factories (§2.5) — O(1), no element storage. `above`/`below`/`universal` fall out
  // of the [lo,hi] bounds. These are MEMBERSHIP-ONLY (possibly infinite): contains works and distributes
  // through the algebra; size/iterator/materialize throw (you cannot enumerate an infinite set). ----
  def range(lo: Int, hi: Int): FSet[Int] = if lo > hi then SEmpty.INSTANCE else new SIntRange(lo, hi)
  def range(lo: Long, hi: Long): FSet[Long] = if lo > hi then SEmpty.INSTANCE else new SLongRange(lo, hi)
  def above(k: Int): FSet[Int] = if k == Int.MaxValue then SEmpty.INSTANCE else new SIntRange(k + 1, Int.MaxValue)
  def below(k: Int): FSet[Int] = if k == Int.MinValue then SEmpty.INSTANCE else new SIntRange(Int.MinValue, k - 1)
  def universalInt: FSet[Int] = new SIntRange(Int.MinValue, Int.MaxValue)

  extension [A](xs: FSet[A])
    // ---- query (the hot read path) ----
    // Each op dispatches the element kind at the concrete call site via `summonFrom` inside the inline impl,
    // realizes the unboxed leaf path, and routes through the shared non-inline `FSetOps` helper. The opaque
    // receiver `xs` flows into the `SBase`-typed impl param; the opaque⇒SBase coercion happens at the parameter
    // boundary. (The FSet core node classes are `S`-prefixed — SIntOne/SUnion/… — to avoid colliding with
    // FArray's same-package IntOne/etc.; a collision there made the dependent tests fail to see `SIntOne <:
    // SBase`. Keep new node names `S`-prefixed.)
    inline def contains(elem: A): Boolean = FSetOps.containsImpl[A](xs, elem)
    inline def apply(elem: A): Boolean = FSetOps.containsImpl[A](xs, elem)
    inline def size: Int = FSetOps.sizeImpl[A](xs)
    inline def isEmpty: Boolean = FSetOps.isEmptyImpl[A](xs)
    inline def nonEmpty: Boolean = !FSetOps.isEmptyImpl[A](xs)
    // subsetOf: streams this set's elements against `that`'s membership (works for a lazy `that`).
    inline def subsetOf(that: FSet[A]): Boolean = FSetOps.subsetOfImpl[A](xs, that)

    // ---- algebra (lazy, O(1) construct) — the green slice ships single-element incl/excl ----
    // `incl(elem)` = SUnion(this, SOne(elem)); `excl(elem)` = SDiff(this, SOne(elem)) — both O(1), share the
    // base, and return `this` (a sound alias by immutability) when the element is already-present / -absent.
    inline def incl(elem: A): FSet[A] = FSetOps.inclImpl[A, A](xs, elem)
    inline def excl(elem: A): FSet[A] = FSetOps.exclImpl[A, A](xs, elem)
    // `+`/`-` infix convenience aliases.
    inline def +(elem: A): FSet[A] = xs.incl(elem)
    inline def -(elem: A): FSet[A] = xs.excl(elem)

    // ---- bulk algebra (lazy, O(1) construct — share both operands; the merge is deferred to materialization
    // and `contains` distributes over the node, short-circuiting). Kind-agnostic node construction; inside this
    // scope FSet[A] = SBase so the operands flow straight into the Java algebra nodes. ----
    def union(that: FSet[A]): FSet[A] = new SUnion(xs, that)
    def intersect(that: FSet[A]): FSet[A] = new SInter(xs, that)
    def diff(that: FSet[A]): FSet[A] = new SDiff(xs, that)
    def xor(that: FSet[A]): FSet[A] = new SXor(xs, that)
    inline def ++(that: FSet[A]): FSet[A] = xs.union(that)
    inline def &(that: FSet[A]): FSet[A] = xs.intersect(that)
    inline def &~(that: FSet[A]): FSet[A] = xs.diff(that)
    inline def ^(that: FSet[A]): FSet[A] = xs.xor(that)
    // complement: !contains — a membership-only (co-finite) set; contains distributes, enumeration throws.
    def complement: FSet[A] = new SComplement(xs)
    inline def unary_~ : FSet[A] = xs.complement

    // ---- materialize / iterate — force the deferred merge once (memoized on the node), then walk the leaf ----
    // For prim kinds the merge is the unboxed sorted×sorted pass (§3.2), so iteration is ORDERED; Ref is
    // unordered (no Ordering threaded yet). `materialize` folds the lazy tree to a single leaf (no static type
    // change yet — the Option C FSet→FSetMaterialized transition is a NEXT-STEP).
    inline def iterator: Iterator[A] = FSetOps.iteratorImpl[A](xs)
    inline def toList: List[A] = FSetOps.iteratorImpl[A](xs).toList
    // traversal — the user's lambda inlines into a kind-specialized SAM (primitives stay unboxed). Each element
    // visited once (a lazy set is materialized first); forall/exists short-circuit.
    inline def foreach(inline f: A => Unit): Unit = FSetOps.foreachImpl[A](xs)(f)
    inline def forall(inline p: A => Boolean): Boolean = FSetOps.forallImpl[A](xs)(p)
    inline def exists(inline p: A => Boolean): Boolean = FSetOps.existsImpl[A](xs)(p)
    inline def count(inline p: A => Boolean): Int = FSetOps.countImpl[A](xs)(p)
    inline def filter(inline p: A => Boolean): FSet[A] = FSetOps.filterImpl[A](xs)(p)
    inline def filterNot(inline p: A => Boolean): FSet[A] = FSetOps.filterImpl[A](xs)(a => !p(a))
    // map[B]: apply f to each element (read kind A × write kind B, unboxed) and DEDUP into a kind-B set.
    inline def map[B](inline f: A => B): FSet[B] = FSetOps.mapImpl[A, B](xs)(f)
    inline def materialize: FSet[A] = FSetOps.materializeImpl[A](xs)

    // ---- value equals / hashCode — MATERIALIZED-ONLY (throws on a still-lazy set; call .materialize first).
    // Order- and shape-independent: two materialized sets with the same elements are `===` and hash-equal. This
    // is the design's FSetMaterialized-only capability, enforced at runtime until the Option C lattice lands. ----
    inline def sameElements(that: FSet[A]): Boolean = FSetOps.sameElementsImpl[A](xs, that)
    inline def ===(that: FSet[A]): Boolean = xs.sameElements(that)
    inline def setHashCode: Int = FSetOps.hashCodeImpl[A](xs)

    // ---- ordered extras (the FSortedSet capability) — O(1) on the materialized sorted leaf; prim natural order ----
    inline def min: A = FSetOps.minImpl[A](xs)
    inline def max: A = FSetOps.maxImpl[A](xs)
