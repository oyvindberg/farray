package farray

import scala.compiletime.summonFrom

/** Top-level name-based extractors so `import farray.{`+:`, `:+`}` enables `case h +: t` / `case init :+ last` on an `FArray`, shadowing `scala.+:` /
  * `scala.:+`.
  *
  * Both mirror `ListSyntax.::`: a `transparent inline unapply` dispatches on the element kind (the `${K}Repr` machinery) to a per-kind `value class` view whose
  * `isEmpty` / `_1` / `_2` the matcher reads — zero allocation, and on a primitive `FArray` the decomposed element binds RAW (no box).
  *
  *   - `+:` decomposes head / tail: `_1: A` (element 0 — O(1) `${K}Prepend.elem` when the array was built by prepend, else an element read), `_2: FArray[A]`
  *     (O(1) `Prepend.base`, else `tail`). Reuses `ListSyntax`'s cons views (identical head/tail shape).
  *   - `:+` decomposes init / last: `_1: FArray[A]` (O(1) `${K}Append.base`, else `init`), `_2: A` (O(1) `${K}Append.elem`, else the last element read).
  *
  * Nested/deep patterns (`case a +: b +: rest`, `case xs :+ y :+ z`) work by recursion through the O(1) tail/init. The empty `FArray` matches neither
  * (`isEmpty`).
  */
object `+:`:
  transparent inline def unapply[A](xs: FArray[A]) = summonFrom {
    case _: IntRepr[A]     => new IntHead(xs.asInstanceOf[FArray[Int]])
    case _: LongRepr[A]    => new LongHead(xs.asInstanceOf[FArray[Long]])
    case _: DoubleRepr[A]  => new DoubleHead(xs.asInstanceOf[FArray[Double]])
    case _: FloatRepr[A]   => new FloatHead(xs.asInstanceOf[FArray[Float]])
    case _: ShortRepr[A]   => new ShortHead(xs.asInstanceOf[FArray[Short]])
    case _: ByteRepr[A]    => new ByteHead(xs.asInstanceOf[FArray[Byte]])
    case _: CharRepr[A]    => new CharHead(xs.asInstanceOf[FArray[Char]])
    case _: BooleanRepr[A] => new BooleanHead(xs.asInstanceOf[FArray[Boolean]])
    // NB: no `RefRepr[A]` arm. Reference (and abstract) element types fall through to the catch-all
    // `Head[A]`, which handles refs correctly (RefPrepend fast path first) and — crucially — summons
    // NO given. Materializing the PARAMETERIZED `refRepr[A]` given inside this transparent-inline
    // extractor makes dotty's LambdaLift fail to proxy it when an inline op (e.g. `t.map`, `t.toList`)
    // is then called on the bound tail — the pervasive `case h +: t => t.<inlineOp>` shape. The
    // concrete primitive givens (intRepr, …) don't take a type param, so they don't hit the bug.
    case _ => new Head[A](xs)
  }

final class IntHead(private val xs: FArray[Int]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: IntHead = this
  def _1: Int = xs.asInstanceOf[FBase] match
    case p: IntPrepend => p.elem
    case node          => FArrayOps.intAt(node, 0)
  def _2: FArray[Int] = xs.asInstanceOf[FBase] match
    case p: IntPrepend => p.base.asInstanceOf[FArray[Int]]
    case l: IntArr     => (if l.length == 2 then new IntOne(l.data(l.offset + 1)) else new IntArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Int]]
    case _: IntOne     => Empty.INSTANCE.asInstanceOf[FArray[Int]]
    case _             => xs.tail

final class LongHead(private val xs: FArray[Long]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: LongHead = this
  def _1: Long = xs.asInstanceOf[FBase] match
    case p: LongPrepend => p.elem
    case node           => FArrayOps.longAt(node, 0)
  def _2: FArray[Long] = xs.asInstanceOf[FBase] match
    case p: LongPrepend => p.base.asInstanceOf[FArray[Long]]
    case l: LongArr => (if l.length == 2 then new LongOne(l.data(l.offset + 1)) else new LongArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Long]]
    case _: LongOne => Empty.INSTANCE.asInstanceOf[FArray[Long]]
    case _          => xs.tail

final class DoubleHead(private val xs: FArray[Double]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: DoubleHead = this
  def _1: Double = xs.asInstanceOf[FBase] match
    case p: DoublePrepend => p.elem
    case node             => FArrayOps.doubleAt(node, 0)
  def _2: FArray[Double] = xs.asInstanceOf[FBase] match
    case p: DoublePrepend => p.base.asInstanceOf[FArray[Double]]
    case l: DoubleArr     =>
      (if l.length == 2 then new DoubleOne(l.data(l.offset + 1)) else new DoubleArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Double]]
    case _: DoubleOne => Empty.INSTANCE.asInstanceOf[FArray[Double]]
    case _            => xs.tail

final class FloatHead(private val xs: FArray[Float]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: FloatHead = this
  def _1: Float = xs.asInstanceOf[FBase] match
    case p: FloatPrepend => p.elem
    case node            => FArrayOps.floatAt(node, 0)
  def _2: FArray[Float] = xs.asInstanceOf[FBase] match
    case p: FloatPrepend => p.base.asInstanceOf[FArray[Float]]
    case l: FloatArr     =>
      (if l.length == 2 then new FloatOne(l.data(l.offset + 1)) else new FloatArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Float]]
    case _: FloatOne => Empty.INSTANCE.asInstanceOf[FArray[Float]]
    case _           => xs.tail

final class ShortHead(private val xs: FArray[Short]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: ShortHead = this
  def _1: Short = xs.asInstanceOf[FBase] match
    case p: ShortPrepend => p.elem
    case node            => FArrayOps.shortAt(node, 0)
  def _2: FArray[Short] = xs.asInstanceOf[FBase] match
    case p: ShortPrepend => p.base.asInstanceOf[FArray[Short]]
    case l: ShortArr     =>
      (if l.length == 2 then new ShortOne(l.data(l.offset + 1)) else new ShortArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Short]]
    case _: ShortOne => Empty.INSTANCE.asInstanceOf[FArray[Short]]
    case _           => xs.tail

final class ByteHead(private val xs: FArray[Byte]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: ByteHead = this
  def _1: Byte = xs.asInstanceOf[FBase] match
    case p: BytePrepend => p.elem
    case node           => FArrayOps.byteAt(node, 0)
  def _2: FArray[Byte] = xs.asInstanceOf[FBase] match
    case p: BytePrepend => p.base.asInstanceOf[FArray[Byte]]
    case l: ByteArr => (if l.length == 2 then new ByteOne(l.data(l.offset + 1)) else new ByteArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Byte]]
    case _: ByteOne => Empty.INSTANCE.asInstanceOf[FArray[Byte]]
    case _          => xs.tail

final class CharHead(private val xs: FArray[Char]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: CharHead = this
  def _1: Char = xs.asInstanceOf[FBase] match
    case p: CharPrepend => p.elem
    case node           => FArrayOps.charAt(node, 0)
  def _2: FArray[Char] = xs.asInstanceOf[FBase] match
    case p: CharPrepend => p.base.asInstanceOf[FArray[Char]]
    case l: CharArr => (if l.length == 2 then new CharOne(l.data(l.offset + 1)) else new CharArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Char]]
    case _: CharOne => Empty.INSTANCE.asInstanceOf[FArray[Char]]
    case _          => xs.tail

final class BooleanHead(private val xs: FArray[Boolean]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: BooleanHead = this
  def _1: Boolean = xs.asInstanceOf[FBase] match
    case p: BooleanPrepend => p.elem
    case node              => FArrayOps.booleanAt(node, 0)
  def _2: FArray[Boolean] = xs.asInstanceOf[FBase] match
    case p: BooleanPrepend => p.base.asInstanceOf[FArray[Boolean]]
    case l: BooleanArr     =>
      (if l.length == 2 then new BooleanOne(l.data(l.offset + 1)) else new BooleanArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[Boolean]]
    case _: BooleanOne => Empty.INSTANCE.asInstanceOf[FArray[Boolean]]
    case _             => xs.tail

/** Reference / abstract element head-tail view (also the primitive fallback). Handles every prepend node kind; refs hit the `RefPrepend` arm first. Reads box
  * only for a genuinely abstract `A`.
  */
final class Head[A](private val xs: FArray[A]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: Head[A] = this
  def _1: A = xs.asInstanceOf[FBase] match
    case p: RefPrepend     => p.elem.asInstanceOf[A]
    case p: IntPrepend     => p.elem.asInstanceOf[A]
    case p: LongPrepend    => p.elem.asInstanceOf[A]
    case p: DoublePrepend  => p.elem.asInstanceOf[A]
    case p: FloatPrepend   => p.elem.asInstanceOf[A]
    case p: ShortPrepend   => p.elem.asInstanceOf[A]
    case p: BytePrepend    => p.elem.asInstanceOf[A]
    case p: CharPrepend    => p.elem.asInstanceOf[A]
    case p: BooleanPrepend => p.elem.asInstanceOf[A]
    case _                 => xs.boxedAt(0)
  def _2: FArray[A] = xs.asInstanceOf[FBase] match
    case p: RefPrepend     => p.base.asInstanceOf[FArray[A]]
    case p: IntPrepend     => p.base.asInstanceOf[FArray[A]]
    case p: LongPrepend    => p.base.asInstanceOf[FArray[A]]
    case p: DoublePrepend  => p.base.asInstanceOf[FArray[A]]
    case p: FloatPrepend   => p.base.asInstanceOf[FArray[A]]
    case p: ShortPrepend   => p.base.asInstanceOf[FArray[A]]
    case p: BytePrepend    => p.base.asInstanceOf[FArray[A]]
    case p: CharPrepend    => p.base.asInstanceOf[FArray[A]]
    case p: BooleanPrepend => p.base.asInstanceOf[FArray[A]]
    case l: RefArr => (if l.length == 2 then new RefOne(l.data(l.offset + 1)) else new RefArr(l.data, l.offset + 1, l.length - 1)).asInstanceOf[FArray[A]]
    case _: RefOne => Empty.INSTANCE.asInstanceOf[FArray[A]]
    case _         => xs.tail

object `:+`:
  transparent inline def unapply[A](xs: FArray[A]) = summonFrom {
    case _: IntRepr[A]     => new IntSnoc(xs.asInstanceOf[FArray[Int]])
    case _: LongRepr[A]    => new LongSnoc(xs.asInstanceOf[FArray[Long]])
    case _: DoubleRepr[A]  => new DoubleSnoc(xs.asInstanceOf[FArray[Double]])
    case _: FloatRepr[A]   => new FloatSnoc(xs.asInstanceOf[FArray[Float]])
    case _: ShortRepr[A]   => new ShortSnoc(xs.asInstanceOf[FArray[Short]])
    case _: ByteRepr[A]    => new ByteSnoc(xs.asInstanceOf[FArray[Byte]])
    case _: CharRepr[A]    => new CharSnoc(xs.asInstanceOf[FArray[Char]])
    case _: BooleanRepr[A] => new BooleanSnoc(xs.asInstanceOf[FArray[Boolean]])
    // NB: no `RefRepr[A]` arm — see the note on `+:`. Refs/abstract fall through to `Snoc[A]`.
    case _ => new Snoc[A](xs)
  }

final class IntSnoc(private val xs: FArray[Int]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: IntSnoc = this
  def _1: FArray[Int] = xs.asInstanceOf[FBase] match
    case a: IntAppend => a.base.asInstanceOf[FArray[Int]]
    case l: IntArr    => (if l.length == 2 then new IntOne(l.data(l.offset)) else new IntArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Int]]
    case _: IntOne    => Empty.INSTANCE.asInstanceOf[FArray[Int]]
    case _            => xs.init
  def _2: Int = xs.asInstanceOf[FBase] match
    case a: IntAppend => a.elem
    case node         => FArrayOps.intAt(node, node.length - 1)

final class LongSnoc(private val xs: FArray[Long]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: LongSnoc = this
  def _1: FArray[Long] = xs.asInstanceOf[FBase] match
    case a: LongAppend => a.base.asInstanceOf[FArray[Long]]
    case l: LongArr    => (if l.length == 2 then new LongOne(l.data(l.offset)) else new LongArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Long]]
    case _: LongOne    => Empty.INSTANCE.asInstanceOf[FArray[Long]]
    case _             => xs.init
  def _2: Long = xs.asInstanceOf[FBase] match
    case a: LongAppend => a.elem
    case node          => FArrayOps.longAt(node, node.length - 1)

final class DoubleSnoc(private val xs: FArray[Double]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: DoubleSnoc = this
  def _1: FArray[Double] = xs.asInstanceOf[FBase] match
    case a: DoubleAppend => a.base.asInstanceOf[FArray[Double]]
    case l: DoubleArr => (if l.length == 2 then new DoubleOne(l.data(l.offset)) else new DoubleArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Double]]
    case _: DoubleOne => Empty.INSTANCE.asInstanceOf[FArray[Double]]
    case _            => xs.init
  def _2: Double = xs.asInstanceOf[FBase] match
    case a: DoubleAppend => a.elem
    case node            => FArrayOps.doubleAt(node, node.length - 1)

final class FloatSnoc(private val xs: FArray[Float]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: FloatSnoc = this
  def _1: FArray[Float] = xs.asInstanceOf[FBase] match
    case a: FloatAppend => a.base.asInstanceOf[FArray[Float]]
    case l: FloatArr    => (if l.length == 2 then new FloatOne(l.data(l.offset)) else new FloatArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Float]]
    case _: FloatOne    => Empty.INSTANCE.asInstanceOf[FArray[Float]]
    case _              => xs.init
  def _2: Float = xs.asInstanceOf[FBase] match
    case a: FloatAppend => a.elem
    case node           => FArrayOps.floatAt(node, node.length - 1)

final class ShortSnoc(private val xs: FArray[Short]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: ShortSnoc = this
  def _1: FArray[Short] = xs.asInstanceOf[FBase] match
    case a: ShortAppend => a.base.asInstanceOf[FArray[Short]]
    case l: ShortArr    => (if l.length == 2 then new ShortOne(l.data(l.offset)) else new ShortArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Short]]
    case _: ShortOne    => Empty.INSTANCE.asInstanceOf[FArray[Short]]
    case _              => xs.init
  def _2: Short = xs.asInstanceOf[FBase] match
    case a: ShortAppend => a.elem
    case node           => FArrayOps.shortAt(node, node.length - 1)

final class ByteSnoc(private val xs: FArray[Byte]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: ByteSnoc = this
  def _1: FArray[Byte] = xs.asInstanceOf[FBase] match
    case a: ByteAppend => a.base.asInstanceOf[FArray[Byte]]
    case l: ByteArr    => (if l.length == 2 then new ByteOne(l.data(l.offset)) else new ByteArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Byte]]
    case _: ByteOne    => Empty.INSTANCE.asInstanceOf[FArray[Byte]]
    case _             => xs.init
  def _2: Byte = xs.asInstanceOf[FBase] match
    case a: ByteAppend => a.elem
    case node          => FArrayOps.byteAt(node, node.length - 1)

final class CharSnoc(private val xs: FArray[Char]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: CharSnoc = this
  def _1: FArray[Char] = xs.asInstanceOf[FBase] match
    case a: CharAppend => a.base.asInstanceOf[FArray[Char]]
    case l: CharArr    => (if l.length == 2 then new CharOne(l.data(l.offset)) else new CharArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Char]]
    case _: CharOne    => Empty.INSTANCE.asInstanceOf[FArray[Char]]
    case _             => xs.init
  def _2: Char = xs.asInstanceOf[FBase] match
    case a: CharAppend => a.elem
    case node          => FArrayOps.charAt(node, node.length - 1)

final class BooleanSnoc(private val xs: FArray[Boolean]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: BooleanSnoc = this
  def _1: FArray[Boolean] = xs.asInstanceOf[FBase] match
    case a: BooleanAppend => a.base.asInstanceOf[FArray[Boolean]]
    case l: BooleanArr    =>
      (if l.length == 2 then new BooleanOne(l.data(l.offset)) else new BooleanArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[Boolean]]
    case _: BooleanOne => Empty.INSTANCE.asInstanceOf[FArray[Boolean]]
    case _             => xs.init
  def _2: Boolean = xs.asInstanceOf[FBase] match
    case a: BooleanAppend => a.elem
    case node             => FArrayOps.booleanAt(node, node.length - 1)

/** Reference / abstract element init-last view (also the primitive fallback). Handles every append node kind; refs hit the `RefAppend` arm first. Reads box
  * only for a genuinely abstract `A`.
  */
final class Snoc[A](private val xs: FArray[A]) extends AnyVal:
  def isEmpty: Boolean = xs.length == 0
  def get: Snoc[A] = this
  def _1: FArray[A] = xs.asInstanceOf[FBase] match
    case a: RefAppend     => a.base.asInstanceOf[FArray[A]]
    case a: IntAppend     => a.base.asInstanceOf[FArray[A]]
    case a: LongAppend    => a.base.asInstanceOf[FArray[A]]
    case a: DoubleAppend  => a.base.asInstanceOf[FArray[A]]
    case a: FloatAppend   => a.base.asInstanceOf[FArray[A]]
    case a: ShortAppend   => a.base.asInstanceOf[FArray[A]]
    case a: ByteAppend    => a.base.asInstanceOf[FArray[A]]
    case a: CharAppend    => a.base.asInstanceOf[FArray[A]]
    case a: BooleanAppend => a.base.asInstanceOf[FArray[A]]
    case l: RefArr        => (if l.length == 2 then new RefOne(l.data(l.offset)) else new RefArr(l.data, l.offset, l.length - 1)).asInstanceOf[FArray[A]]
    case _: RefOne        => Empty.INSTANCE.asInstanceOf[FArray[A]]
    case _                => xs.init
  def _2: A = xs.asInstanceOf[FBase] match
    case a: RefAppend     => a.elem.asInstanceOf[A]
    case a: IntAppend     => a.elem.asInstanceOf[A]
    case a: LongAppend    => a.elem.asInstanceOf[A]
    case a: DoubleAppend  => a.elem.asInstanceOf[A]
    case a: FloatAppend   => a.elem.asInstanceOf[A]
    case a: ShortAppend   => a.elem.asInstanceOf[A]
    case a: ByteAppend    => a.elem.asInstanceOf[A]
    case a: CharAppend    => a.elem.asInstanceOf[A]
    case a: BooleanAppend => a.elem.asInstanceOf[A]
    case node             => xs.boxedAt(node.length - 1)
