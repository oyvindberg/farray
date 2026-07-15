package farray

/** Growable, kind-specialized builder behind `FArray.newBuilder[A]`. Primitive backing arrays
  * (no per-element boxing for a primitive element type), doubling growth, and `result()` that wraps
  * the backing array WITHOUT a copy when `size == capacity` (else trims with one `Arrays.copyOf`).
  *
  * `FArray.newBuilder` is `transparent inline`, so the concrete per-kind builder type is retained at
  * the call site — `b += x` on an `FArray.newBuilder[Int]` calls the `Int`-specialized `addOne`
  * unboxed. Reference and abstract element types share `RefFArrayBuilder` (Object backing; refs land
  * directly, an abstract-primitive `A` boxes on add — inherent to its erasure).
  *
  * dotty-style protocol: `val b = FArray.newBuilder[T]; b += x; b ++= xs; b.result()`.
  */
sealed abstract class FArrayBuilder[A]:
  /** append one element */
  def addOne(elem: A): this.type
  final inline def +=(elem: A): this.type = addOne(elem)
  /** append all of an FArray (O(1) size hint, unboxed traversal for a primitive kind) */
  def addAll(xs: FArray[A]): this.type
  /** append all of any IterableOnce (boxes for a primitive kind — generic iterator) */
  def addAll(it: IterableOnce[A]): this.type
  final inline def ++=(xs: FArray[A]): this.type = addAll(xs)
  final inline def ++=(it: IterableOnce[A]): this.type = addAll(it)
  /** pre-grow the backing array so at least `size` elements fit without realloc */
  def sizeHint(size: Int): Unit
  /** reset to empty, keeping the allocated backing array */
  def clear(): Unit
  /** number of elements added so far */
  def knownSize: Int
  /** materialize; wraps the backing array without copy when full, else trims */
  def result(): FArray[A]

object FArrayBuilder:
  // shared zero-length backing arrays so a fresh (unused) builder allocates nothing
  private[farray] val emptyInt: Array[Int]         = new Array[Int](0)
  private[farray] val emptyLong: Array[Long]       = new Array[Long](0)
  private[farray] val emptyDouble: Array[Double]   = new Array[Double](0)
  private[farray] val emptyFloat: Array[Float]     = new Array[Float](0)
  private[farray] val emptyShort: Array[Short]     = new Array[Short](0)
  private[farray] val emptyByte: Array[Byte]       = new Array[Byte](0)
  private[farray] val emptyChar: Array[Char]       = new Array[Char](0)
  private[farray] val emptyBoolean: Array[Boolean] = new Array[Boolean](0)
  private[farray] val emptyRef: Array[Object]      = new Array[Object](0)
  private[farray] def grownCap(cur: Int, need: Int): Int =
    var nc = if cur == 0 then 16 else cur
    while nc < need do nc <<= 1
    nc

final class IntFArrayBuilder extends FArrayBuilder[Int]:
  private var arr: Array[Int] = FArrayBuilder.emptyInt
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Int): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Int]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Int]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Int] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new IntOne(arr(0))
     else if n == arr.length then new IntArr(arr, n)
     else new IntArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Int]]

final class LongFArrayBuilder extends FArrayBuilder[Long]:
  private var arr: Array[Long] = FArrayBuilder.emptyLong
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Long): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Long]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Long]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Long] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new LongOne(arr(0))
     else if n == arr.length then new LongArr(arr, n)
     else new LongArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Long]]

final class DoubleFArrayBuilder extends FArrayBuilder[Double]:
  private var arr: Array[Double] = FArrayBuilder.emptyDouble
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Double): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Double]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Double]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Double] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new DoubleOne(arr(0))
     else if n == arr.length then new DoubleArr(arr, n)
     else new DoubleArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Double]]

final class FloatFArrayBuilder extends FArrayBuilder[Float]:
  private var arr: Array[Float] = FArrayBuilder.emptyFloat
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Float): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Float]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Float]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Float] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new FloatOne(arr(0))
     else if n == arr.length then new FloatArr(arr, n)
     else new FloatArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Float]]

final class ShortFArrayBuilder extends FArrayBuilder[Short]:
  private var arr: Array[Short] = FArrayBuilder.emptyShort
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Short): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Short]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Short]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Short] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new ShortOne(arr(0))
     else if n == arr.length then new ShortArr(arr, n)
     else new ShortArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Short]]

final class ByteFArrayBuilder extends FArrayBuilder[Byte]:
  private var arr: Array[Byte] = FArrayBuilder.emptyByte
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Byte): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Byte]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Byte]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Byte] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new ByteOne(arr(0))
     else if n == arr.length then new ByteArr(arr, n)
     else new ByteArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Byte]]

final class CharFArrayBuilder extends FArrayBuilder[Char]:
  private var arr: Array[Char] = FArrayBuilder.emptyChar
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Char): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Char]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Char]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Char] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new CharOne(arr(0))
     else if n == arr.length then new CharArr(arr, n)
     else new CharArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Char]]

final class BooleanFArrayBuilder extends FArrayBuilder[Boolean]:
  private var arr: Array[Boolean] = FArrayBuilder.emptyBoolean
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: Boolean): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem; n += 1; this
  def addAll(xs: FArray[Boolean]): this.type =
    val need = n + xs.length; if need > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, need))
    xs.foreach(v => { arr(n) = v; n += 1 }); this
  def addAll(it: IterableOnce[Boolean]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[Boolean] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new BooleanOne(arr(0))
     else if n == arr.length then new BooleanArr(arr, n)
     else new BooleanArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[Boolean]]

/** Reference / abstract element builder (Object backing). Used for every `A` that is not a concrete
  * primitive kind, so it also backs generic `newBuilder[A]` in abstract contexts. */
final class RefFArrayBuilder[A] extends FArrayBuilder[A]:
  private var arr: Array[Object] = FArrayBuilder.emptyRef
  private var n: Int = 0
  def knownSize: Int = n
  def sizeHint(size: Int): Unit = if size > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, size))
  def addOne(elem: A): this.type =
    if n == arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + 1))
    arr(n) = elem.asInstanceOf[Object]; n += 1; this
  def addAll(xs: FArray[A]): this.type =
    val base = xs.asInstanceOf[FBase]; val len = base.length
    if n + len > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + len))
    var i = 0; while i < len do { arr(n) = base.applyBoxed(i); n += 1; i += 1 }; this
  def addAll(it: IterableOnce[A]): this.type =
    val ks = it.knownSize; if ks > 0 && n + ks > arr.length then arr = java.util.Arrays.copyOf(arr, FArrayBuilder.grownCap(arr.length, n + ks))
    val i = it.iterator; while i.hasNext do addOne(i.next()); this
  def clear(): Unit = n = 0
  def result(): FArray[A] =
    (if n == 0 then Empty.INSTANCE
     else if n == 1 then new RefOne(arr(0))
     else if n == arr.length then new RefArr(arr, n)
     else new RefArr(java.util.Arrays.copyOf(arr, n), n)).asInstanceOf[FArray[A]]
