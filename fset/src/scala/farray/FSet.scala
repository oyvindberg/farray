package farray

/** The FSet type lattice — Option C: REAL opaque subtyping over the sealed Java core [[SBase]] (zero wrapper;
  * verified on Scala 3.8.3). Capability split enforced by the types:
  * {{{
  *   FSet[A]              <: AnyRef          membership + lazy algebra (no enumeration)
  *    └ FSetFinite[A]     <: FSet[A]         finite (the .finite target; materialize)
  *       └ FSetMaterialized[A] <: FSetFinite enumerable: size/iterator/foreach/map/filter/equals/min/max
  *          └ FSetSorted[A]    <: FSetMaterialized   ordered extras (later)
  *   FSetInfinite[-A]     <: AnyRef          predicate-only (range/above/below/universal/complement)
  * }}}
  * `FSetInfinite` is SEPARATE (contravariant — an infinite set is a predicate `A => Boolean`) and bridges to
  * `FSet` by a no-import `Conversion`, so you **cannot enumerate an infinite set**: `FSet.above(5).size` does
  * not compile. Construction yields an `FSetMaterialized`; the lazy algebra (`++`/`&`/`+`/…) yields an `FSet`,
  * which you `.materialize` (→ `FSetMaterialized`) to enumerate. Invariant in `A` except `FSetInfinite[-A]`.
  */
opaque type FSet[A] <: AnyRef = SBase
opaque type FSetFinite[A] <: FSet[A] = SBase
opaque type FSetMaterialized[A] <: FSetFinite[A] = SBase
opaque type FSetSorted[A] <: FSetMaterialized[A] = SBase
opaque type FSetInfinite[-A] <: AnyRef = SBase

object FSet:

  // ---- construction → a MATERIALIZED set (built = enumerable) ----
  inline def empty[A]: FSetMaterialized[A] = FSetOps.emptyImpl[A]
  inline def apply[A](a: A): FSetMaterialized[A] = FSetOps.fromValues1[A](a)
  inline def apply[A](a: A, b: A): FSetMaterialized[A] = FSetOps.fromValues2[A](a, b)
  inline def apply[A](a: A, b: A, c: A): FSetMaterialized[A] = FSetOps.fromValues3[A](a, b, c)
  inline def apply[A](a: A, b: A, c: A, rest: A*): FSetMaterialized[A] =
    val b0 = scala.collection.mutable.ArrayBuffer[A](a, b, c)
    b0 ++= rest
    FSetOps.fromImpl[A](b0)
  inline def fromArray[A](as: Array[A]): FSetMaterialized[A] = FSetOps.fromArrayImpl[A](as)
  inline def fromFArray[A](fa: FArray[A]): FSetMaterialized[A] = FSetOps.fromImpl[A](fa.iterator)
  inline def from[A](it: IterableOnce[A]): FSetMaterialized[A] = FSetOps.fromImpl[A](it)
  inline def fromIterable[A](it: Iterable[A]): FSetMaterialized[A] = FSetOps.fromImpl[A](it)

  // ---- predicate / range factories → an INFINITE (membership-only) set (§2.5). No size/iterator/materialize. ----
  def range(lo: Int, hi: Int): FSetInfinite[Int] = if lo > hi then SEmpty.INSTANCE else new SIntRange(lo, hi)
  def range(lo: Long, hi: Long): FSetInfinite[Long] = if lo > hi then SEmpty.INSTANCE else new SLongRange(lo, hi)
  def above(k: Int): FSetInfinite[Int] = if k == Int.MaxValue then SEmpty.INSTANCE else new SIntRange(k + 1, Int.MaxValue)
  def below(k: Int): FSetInfinite[Int] = if k == Int.MinValue then SEmpty.INSTANCE else new SIntRange(Int.MinValue, k - 1)
  def universalInt: FSetInfinite[Int] = new SIntRange(Int.MinValue, Int.MaxValue)

  // ---- FSet (top): membership + lazy algebra. Inherited by every finite subtype (and reached by an
  // FSetInfinite via the Conversion). The opaque receiver flows into the SBase-typed impl at the boundary. ----
  extension [A](xs: FSet[A])
    inline def contains(elem: A): Boolean = FSetOps.containsImpl[A](xs, elem)
    inline def apply(elem: A): Boolean = FSetOps.containsImpl[A](xs, elem)
    inline def subsetOf(that: FSet[A]): Boolean = FSetOps.subsetOfImpl[A](xs, that)
    // algebra — O(1) lazy nodes, share both operands; result is a (possibly lazy) FSet, `.materialize` to enumerate.
    inline def incl(elem: A): FSet[A] = FSetOps.inclImpl[A, A](xs, elem)
    inline def excl(elem: A): FSet[A] = FSetOps.exclImpl[A, A](xs, elem)
    inline def +(elem: A): FSet[A] = xs.incl(elem)
    inline def -(elem: A): FSet[A] = xs.excl(elem)
    def union(that: FSet[A]): FSet[A] = new SUnion(xs, that)
    def intersect(that: FSet[A]): FSet[A] = new SInter(xs, that)
    def diff(that: FSet[A]): FSet[A] = new SDiff(xs, that)
    def xor(that: FSet[A]): FSet[A] = new SXor(xs, that)
    inline def ++(that: FSet[A]): FSet[A] = xs.union(that)
    inline def &(that: FSet[A]): FSet[A] = xs.intersect(that)
    inline def &~(that: FSet[A]): FSet[A] = xs.diff(that)
    inline def ^(that: FSet[A]): FSet[A] = xs.xor(that)
    // complement of a set is co-finite → an INFINITE (membership-only) set.
    def complement: FSetInfinite[A] = new SComplement(xs)
    inline def unary_~ : FSetInfinite[A] = xs.complement
    // materialize: fold the lazy algebra into one leaf (memoized). Throws at runtime if the tree laundered an
    // infinite leaf in (e.g. finite ∪ above(k)); a directly-infinite set is an FSetInfinite and can't reach here.
    inline def materialize: FSetMaterialized[A] = FSetOps.materializeImpl[A](xs)

  // ---- FSetMaterialized: the enumerable face — size / iterate / transform / value-equality / ordered ----
  extension [A](xs: FSetMaterialized[A])
    inline def size: Int = FSetOps.sizeImpl[A](xs)
    inline def isEmpty: Boolean = FSetOps.isEmptyImpl[A](xs)
    inline def nonEmpty: Boolean = !FSetOps.isEmptyImpl[A](xs)
    inline def iterator: Iterator[A] = FSetOps.iteratorImpl[A](xs)
    inline def toList: List[A] = FSetOps.iteratorImpl[A](xs).toList
    inline def foreach(inline f: A => Unit): Unit = FSetOps.foreachImpl[A](xs)(f)
    inline def forall(inline p: A => Boolean): Boolean = FSetOps.forallImpl[A](xs)(p)
    inline def exists(inline p: A => Boolean): Boolean = FSetOps.existsImpl[A](xs)(p)
    inline def count(inline p: A => Boolean): Int = FSetOps.countImpl[A](xs)(p)
    inline def filter(inline p: A => Boolean): FSetMaterialized[A] = FSetOps.filterImpl[A](xs)(p)
    inline def filterNot(inline p: A => Boolean): FSetMaterialized[A] = FSetOps.filterImpl[A](xs)(a => !p(a))
    inline def map[B](inline f: A => B): FSetMaterialized[B] = FSetOps.mapImpl[A, B](xs)(f)
    // value equals / hashCode — now a COMPILE-TIME guarantee (only materialized sets have them).
    inline def sameElements(that: FSetMaterialized[A]): Boolean = FSetOps.sameElementsImpl[A](xs, that)
    inline def ===(that: FSetMaterialized[A]): Boolean = xs.sameElements(that)
    inline def setHashCode: Int = FSetOps.hashCodeImpl[A](xs)
    // ordered extras (prim natural order; the FSetSorted level is where these formally live — populated later).
    inline def min: A = FSetOps.minImpl[A](xs)
    inline def max: A = FSetOps.maxImpl[A](xs)

object FSetInfinite:
  // an infinite set IS a membership predicate — `contains` + the algebra; bridges to FSet (so it flows into a
  // finite set's algebra too). Combining with anything yields a (non-enumerable) FSet — still no size/iterate.
  given infToFSet[A]: Conversion[FSetInfinite[A], FSet[A]] = s => s
  extension [A](xs: FSetInfinite[A])
    inline def contains(elem: A): Boolean = FSetOps.containsImpl[A](xs, elem)
    inline def apply(elem: A): Boolean = FSetOps.containsImpl[A](xs, elem)
    def union(that: FSet[A]): FSet[A] = new SUnion(xs, that)
    def intersect(that: FSet[A]): FSet[A] = new SInter(xs, that)
    def diff(that: FSet[A]): FSet[A] = new SDiff(xs, that)
    def xor(that: FSet[A]): FSet[A] = new SXor(xs, that)
    inline def ++(that: FSet[A]): FSet[A] = xs.union(that)
    inline def &(that: FSet[A]): FSet[A] = xs.intersect(that)
    inline def &~(that: FSet[A]): FSet[A] = xs.diff(that)
    inline def ^(that: FSet[A]): FSet[A] = xs.xor(that)
    def complement: FSetInfinite[A] = new SComplement(xs)
