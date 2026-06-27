package farray

import scala.annotation.compileTimeOnly

/** A single aggregation for the `agg` multi-aggregate terminal: `xs.fuse.agg(Agg.sum(_.x), Agg.count, …)` runs all of them in ONE fused pass, carrying one
  * accumulator per aggregate. Each aggregate's element lambda is decomposed against the shared element by the optimizer, so the columns the aggregates read are
  * MERGED automatically — the union is computed (dead fields are skipped), a field read by several aggregates is computed once (CSE), and a field behind a
  * filter is computed only for survivors (the sink).
  *
  * `Agg.xxx(...)` is SYNTAX, not a value: it is a marker the `agg`/`aggTo` macro reads off the AST (so it can inline the aggregate's lambda into the fused loop
  * — the no-boxing / column-DCE win). It is NOT a usable runtime value — you cannot bind it to a `val`, store it, return it from a non-inline `def`, or build a
  * `List` of them and pass them dynamically. Every factory is `@compileTimeOnly`, so any such misuse is a clear COMPILE ERROR pointing here, rather than a
  * silent no-op. `R` is the aggregate's result type.
  */
final class Agg[-A, +R] private ():
  // phantom: the macro never constructs/reads instances — it pattern-matches the `Agg.sum(...)` call site.
  ()

object Agg:
  // message shared by every factory's @compileTimeOnly: fires iff the call SURVIVES macro expansion, i.e. it
  // wasn't consumed directly by `agg`/`aggTo` (e.g. bound to a `val` / used at runtime).
  private final inline val Msg =
    "Agg.* is aggregation SYNTAX for `agg`/`aggTo`, not a value — pass it directly, e.g. " +
      "`xs.fuse.agg(Agg.sum(_.x), Agg.count)`. It cannot be bound to a `val`, stored, or built at runtime."

  /** Σ f(a) — sum the projected numeric field. */
  @compileTimeOnly(Msg) def sum[A, B](f: A => B)(using Numeric[B]): Agg[A, B] = new Agg()

  /** number of (surviving) elements. */
  @compileTimeOnly(Msg) def count[A]: Agg[A, Int] = new Agg()

  /** minimum / maximum of the projected field, or None if empty. (For a primitive field the comparison is unboxed; the `Option` wraps only the final result —
    * so a case-class field receiving a min/max via `aggTo` must be typed `Option[B]`.)
    */
  @compileTimeOnly(Msg) def min[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[B]] = new Agg()
  @compileTimeOnly(Msg) def max[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[B]] = new Agg()

  /** like `min`/`max` but for a KNOWN-NON-EMPTY input: the result is `B`, not `Option[B]` (so a case-class field can be a plain primitive). Throws
    * `NoSuchElementException` if the pipeline is empty.
    */
  @compileTimeOnly(Msg) def min1[A, B](f: A => B)(using Ordering[B]): Agg[A, B] = new Agg()
  @compileTimeOnly(Msg) def max1[A, B](f: A => B)(using Ordering[B]): Agg[A, B] = new Agg()

  /** the general aggregate: a seeded left fold over the (surviving) elements. */
  @compileTimeOnly(Msg) def fold[A, S](z: S)(step: (S, A) => S): Agg[A, S] = new Agg()

  /** run `f` for its side effect on every (surviving) element; result is Unit. The single-aggregate base that the standalone `foreach`/`toList`/`toSet`/…
    * terminals desugar to.
    */
  @compileTimeOnly(Msg) def foreach[A](f: A => Unit): Agg[A, Unit] = new Agg()

  /** mean of the projected field (sum / count, fused into one pass), or 0.0 if empty. */
  @compileTimeOnly(Msg) def avg[A](f: A => Double): Agg[A, Double] = new Agg()

  /** the `n` elements with the largest / smallest `key(a)`, best-first — via a bounded size-`n` heap, in ONE streaming pass: O(N log n) time, O(n) memory (no
    * full sort, no O(N) buffer). Returns `FArray[A]`.
    */
  @compileTimeOnly(Msg) def topNBy[A, B](n: Int)(key: A => B)(using Ordering[B]): Agg[A, FArray[A]] = new Agg()
  @compileTimeOnly(Msg) def bottomNBy[A, B](n: Int)(key: A => B)(using Ordering[B]): Agg[A, FArray[A]] = new Agg()

  /** the `n` largest / smallest elements by their natural ordering, best-first. */
  @compileTimeOnly(Msg) def largest[A](n: Int)(using Ordering[A]): Agg[A, FArray[A]] = new Agg()
  @compileTimeOnly(Msg) def smallest[A](n: Int)(using Ordering[A]): Agg[A, FArray[A]] = new Agg()

  /** reduce the (surviving) elements with `op` (seeded by the first element, no zero), or None if empty. */
  @compileTimeOnly(Msg) def reduce[A, B >: A](op: (B, B) => B): Agg[A, Option[B]] = new Agg()

  /** like `reduce` but for a KNOWN-NON-EMPTY input → `B` (throws `NoSuchElementException` if empty). */
  @compileTimeOnly(Msg) def reduce1[A, B >: A](op: (B, B) => B): Agg[A, B] = new Agg()

  /** left-fold reduce seeded by the first element: `op(acc, a): B` with the element as the 2nd arg (the form the standalone
    * `reduceOption`/`reduceLeftOption`/`min`/`max`/`last` terminals desugar to). None if empty.
    */
  @compileTimeOnly(Msg) def reduceL[A, B >: A](op: (B, A) => B): Agg[A, Option[B]] = new Agg()

  /** the ELEMENT with the smallest / largest `f(a)` (not the key — the element), or None if empty. */
  @compileTimeOnly(Msg) def minBy[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[A]] = new Agg()
  @compileTimeOnly(Msg) def maxBy[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[A]] = new Agg()

  /** like `minBy`/`maxBy` but for a KNOWN-NON-EMPTY input → `A` (throws if empty). */
  @compileTimeOnly(Msg) def minBy1[A, B](f: A => B)(using Ordering[B]): Agg[A, A] = new Agg()
  @compileTimeOnly(Msg) def maxBy1[A, B](f: A => B)(using Ordering[B]): Agg[A, A] = new Agg()
