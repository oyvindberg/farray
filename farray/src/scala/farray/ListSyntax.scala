package farray

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
  * Keep FArray and `List` pattern matches in separate scopes: importing this shadows `scala.::`.
  */
object ListSyntax:
  /** the empty FArray */
  val Nil: FArray[Nothing] = FArray.empty[Nothing]

  /** `case h :: t` — returns a value-class view; the matcher reads its real `isEmpty`/`_1`/`_2`. */
  object `::`:
    def unapply[A](xs: FArray[A]): Cons[A] = new Cons(xs)

  /** name-based-extractor view; `AnyVal` so deconstruction allocates nothing. */
  final class Cons[A](private val xs: FArray[A]) extends AnyVal:
    def isEmpty: Boolean = xs.length == 0
    def get: Cons[A] = this
    // On a cons-built chain the node IS a ${K}Prepend — peel it with field reads (like List's own
    // `::` match) instead of the virtual applyBoxed/drop pair, which measured 0.34x of List on
    // head/tail recursion. Generic (A abstract), so prim elems box — exactly like List[Int].
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
