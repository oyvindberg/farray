package farray

/** Kind-specialized value-class views backing `FArray.unapplySeq`, so `case FArray(a, b)` /
  * `case FArray(x, rest*)` / `case FArray()` compile through the name-based sequence-extractor
  * protocol. The matcher reads the real members `lengthCompare` / `apply` / `drop` / `toSeq`.
  *
  * `unapplySeq` is `transparent inline` and dispatches on the element kind (the `${K}Repr`
  * machinery), so on `FArray[Int]` the positional bindings (`apply(i)`) hand back a raw `Int` — no
  * box per bound variable. The vararg suffix (`rest*`) is materialized through `drop(n): Seq[A]`,
  * which DOES box (a `Seq` erases its element) — that boxing is inherent to the `_*` binding and
  * matches how a `Vector(x, rest*)` suffix boxes too, so it is acceptable.
  */
object SeqSyntax

final class IntSeqView(private val xs: FArray[Int]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Int = FArrayOps.intAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Int] = xs.drop(n).toSeq
  def toSeq: Seq[Int] = xs.toSeq

final class LongSeqView(private val xs: FArray[Long]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Long = FArrayOps.longAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Long] = xs.drop(n).toSeq
  def toSeq: Seq[Long] = xs.toSeq

final class DoubleSeqView(private val xs: FArray[Double]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Double = FArrayOps.doubleAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Double] = xs.drop(n).toSeq
  def toSeq: Seq[Double] = xs.toSeq

final class FloatSeqView(private val xs: FArray[Float]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Float = FArrayOps.floatAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Float] = xs.drop(n).toSeq
  def toSeq: Seq[Float] = xs.toSeq

final class ShortSeqView(private val xs: FArray[Short]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Short = FArrayOps.shortAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Short] = xs.drop(n).toSeq
  def toSeq: Seq[Short] = xs.toSeq

final class ByteSeqView(private val xs: FArray[Byte]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Byte = FArrayOps.byteAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Byte] = xs.drop(n).toSeq
  def toSeq: Seq[Byte] = xs.toSeq

final class CharSeqView(private val xs: FArray[Char]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Char = FArrayOps.charAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Char] = xs.drop(n).toSeq
  def toSeq: Seq[Char] = xs.toSeq

final class BooleanSeqView(private val xs: FArray[Boolean]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): Boolean = FArrayOps.booleanAt(xs.asInstanceOf[FBase], i)
  def drop(n: Int): Seq[Boolean] = xs.drop(n).toSeq
  def toSeq: Seq[Boolean] = xs.toSeq

final class RefSeqView[A](private val xs: FArray[A]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): A = FArrayOps.refAt(xs.asInstanceOf[FBase], i).asInstanceOf[A]
  def drop(n: Int): Seq[A] = xs.drop(n).toSeq
  def toSeq: Seq[A] = xs.toSeq

/** boxed fallback for an abstract element type (no `${K}Repr` in scope). */
final class AnySeqView[A](private val xs: FArray[A]) extends AnyVal:
  def lengthCompare(len: Int): Int = Integer.compare(xs.length, len)
  def apply(i: Int): A = xs.asInstanceOf[FBase].applyBoxed(i).asInstanceOf[A]
  def drop(n: Int): Seq[A] = xs.drop(n).toSeq
  def toSeq: Seq[A] = xs.toSeq
