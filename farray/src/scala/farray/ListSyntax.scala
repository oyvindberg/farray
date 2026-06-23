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
    // generic (A abstract here), so read via the boxed accessor — like List[Int], which is boxed anyway
    def _1: A = xs.boxedAt(0)
    def _2: FArray[A] = xs.tail
