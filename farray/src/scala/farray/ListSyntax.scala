package farray

import scala.compiletime.summonFrom

/** `import farray.ListSyntax.*` to use `FArray` with `List` syntax:
  *
  * {{{
  *   val xs = 1 :: 2 :: 3 :: Nil          // cons — O(1) prepend nodes (FArray already provides `::`)
  *   xs match
  *     case h :: t => ...                  // deconstruct head / tail — no Option, no tuple, no allocation
  *     case _      => ...
  * }}}
  *
  * `case h :: t` goes through a name-based extractor over a `value class` view, so it allocates nothing, and on a cons-built chain `tail` is the O(1) `Prepend`
  * base — head/tail recursion is as cheap as `List`.
  *
  * The extractor is `transparent inline` and dispatches on the element kind (the same `${K}Repr` machinery as every other op), so on `FArray[Int]` the head
  * binds as a raw `Int` — no `Integer.valueOf` box per element. (The boxed-`Cons` path measured 0.48x of `List` on head/tail recursion @1000: ~16 B/element of
  * surviving box allocations; `List[Int]` reads its cells' pre-boxed values with zero allocation.) Each specialized view also needs only its own `${K}Prepend`
  * arm — one instanceof + two field reads per element, the same shape as `List`'s own `::` match.
  *
  * Keep FArray and `List` pattern matches in separate scopes: importing this shadows `scala.::`.
  */
object ListSyntax:
  /** the empty FArray */
  val Nil: FArray[Nothing] = FArray.empty[Nothing]

  /** `case h :: t` — inlines to a kind-specialized value-class view; the matcher reads its real `isEmpty`/`_1`/`_2`. */
  object `::`:
    transparent inline def unapply[A](xs: FArray[A]) = summonFrom {
      case _: IntRepr[A]     => new IntCons(xs.asInstanceOf[FArray[Int]])
      case _: LongRepr[A]    => new LongCons(xs.asInstanceOf[FArray[Long]])
      case _: DoubleRepr[A]  => new DoubleCons(xs.asInstanceOf[FArray[Double]])
      case _: FloatRepr[A]   => new FloatCons(xs.asInstanceOf[FArray[Float]])
      case _: ShortRepr[A]   => new ShortCons(xs.asInstanceOf[FArray[Short]])
      case _: ByteRepr[A]    => new ByteCons(xs.asInstanceOf[FArray[Byte]])
      case _: CharRepr[A]    => new CharCons(xs.asInstanceOf[FArray[Char]])
      case _: BooleanRepr[A] => new BooleanCons(xs.asInstanceOf[FArray[Boolean]])
      case _: RefRepr[A]     => new RefCons[A](xs)
      case _                 => new Cons[A](xs)
    }

  final class IntCons(private val xs: FArray[Int]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: IntCons = this
    def _1: Int = xs.asInstanceOf[FBase] match
      case p: IntPrepend => p.elem
      case node          => FArrayOps.intAt(node, 0)
    def _2: FArray[Int] = xs.asInstanceOf[FBase] match
      case p: IntPrepend => p.base.asInstanceOf[FArray[Int]]
      case _             => xs.tail

  final class LongCons(private val xs: FArray[Long]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: LongCons = this
    def _1: Long = xs.asInstanceOf[FBase] match
      case p: LongPrepend => p.elem
      case node           => FArrayOps.longAt(node, 0)
    def _2: FArray[Long] = xs.asInstanceOf[FBase] match
      case p: LongPrepend => p.base.asInstanceOf[FArray[Long]]
      case _              => xs.tail

  final class DoubleCons(private val xs: FArray[Double]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: DoubleCons = this
    def _1: Double = xs.asInstanceOf[FBase] match
      case p: DoublePrepend => p.elem
      case node             => FArrayOps.doubleAt(node, 0)
    def _2: FArray[Double] = xs.asInstanceOf[FBase] match
      case p: DoublePrepend => p.base.asInstanceOf[FArray[Double]]
      case _                => xs.tail

  final class FloatCons(private val xs: FArray[Float]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: FloatCons = this
    def _1: Float = xs.asInstanceOf[FBase] match
      case p: FloatPrepend => p.elem
      case node            => FArrayOps.floatAt(node, 0)
    def _2: FArray[Float] = xs.asInstanceOf[FBase] match
      case p: FloatPrepend => p.base.asInstanceOf[FArray[Float]]
      case _               => xs.tail

  final class ShortCons(private val xs: FArray[Short]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: ShortCons = this
    def _1: Short = xs.asInstanceOf[FBase] match
      case p: ShortPrepend => p.elem
      case node            => FArrayOps.shortAt(node, 0)
    def _2: FArray[Short] = xs.asInstanceOf[FBase] match
      case p: ShortPrepend => p.base.asInstanceOf[FArray[Short]]
      case _               => xs.tail

  final class ByteCons(private val xs: FArray[Byte]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: ByteCons = this
    def _1: Byte = xs.asInstanceOf[FBase] match
      case p: BytePrepend => p.elem
      case node           => FArrayOps.byteAt(node, 0)
    def _2: FArray[Byte] = xs.asInstanceOf[FBase] match
      case p: BytePrepend => p.base.asInstanceOf[FArray[Byte]]
      case _              => xs.tail

  final class CharCons(private val xs: FArray[Char]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: CharCons = this
    def _1: Char = xs.asInstanceOf[FBase] match
      case p: CharPrepend => p.elem
      case node           => FArrayOps.charAt(node, 0)
    def _2: FArray[Char] = xs.asInstanceOf[FBase] match
      case p: CharPrepend => p.base.asInstanceOf[FArray[Char]]
      case _              => xs.tail

  final class BooleanCons(private val xs: FArray[Boolean]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: BooleanCons = this
    def _1: Boolean = xs.asInstanceOf[FBase] match
      case p: BooleanPrepend => p.elem
      case node              => FArrayOps.booleanAt(node, 0)
    def _2: FArray[Boolean] = xs.asInstanceOf[FBase] match
      case p: BooleanPrepend => p.base.asInstanceOf[FArray[Boolean]]
      case _                 => xs.tail

  final class RefCons[A](private val xs: FArray[A]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: RefCons[A] = this
    def _1: A = xs.asInstanceOf[FBase] match
      case p: RefPrepend => p.elem.asInstanceOf[A]
      case node          => FArrayOps.refAt(node, 0).asInstanceOf[A]
    def _2: FArray[A] = xs.asInstanceOf[FBase] match
      case p: RefPrepend => p.base.asInstanceOf[FArray[A]]
      case _             => xs.tail

  /** boxed fallback for an abstract element type (no `${K}Repr` in scope) — the pre-specialization view. */
  final class Cons[A](private val xs: FArray[A]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: Cons[A] = this
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
      case _                 => xs.tail
