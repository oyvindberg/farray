package farray

/** A single aggregation for the `agg` multi-aggregate terminal: `xs.fuse.agg(Agg.sum(_.x), Agg.count, …)`
 *  runs all of them in ONE fused pass, carrying one accumulator per aggregate. Each aggregate's element
 *  lambda is decomposed against the shared element by the optimizer, so the columns the aggregates read are
 *  MERGED automatically — the union is computed (dead fields are skipped), a field read by several aggregates
 *  is computed once (CSE), and a field behind a filter is computed only for survivors (the sink).
 *
 *  `Agg` values are MARKERS: the macro reads the `Agg.xxx(...)` call off the AST and synthesizes the loop;
 *  the bodies here never run. `R` is the aggregate's result type. */
final class Agg[-A, +R] private ():
  // phantom: the macro never constructs/reads instances — it pattern-matches the `Agg.sum(...)` call site.
  ()

object Agg:
  /** Σ f(a) — sum the projected numeric field. */
  def sum[A, B](f: A => B)(using Numeric[B]): Agg[A, B] = new Agg()
  /** number of (surviving) elements. */
  def count[A]: Agg[A, Int] = new Agg()
  /** minimum / maximum of the projected field, or None if empty. (For a primitive field the comparison is
   *  unboxed; the `Option` wraps only the final result — so a case-class field receiving a min/max via `aggTo`
   *  must be typed `Option[B]`.) */
  def min[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[B]] = new Agg()
  def max[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[B]] = new Agg()
  /** like `min`/`max` but for a KNOWN-NON-EMPTY input: the result is `B`, not `Option[B]` (so a case-class
   *  field can be a plain primitive). Throws `NoSuchElementException` if the pipeline is empty. */
  def min1[A, B](f: A => B)(using Ordering[B]): Agg[A, B] = new Agg()
  def max1[A, B](f: A => B)(using Ordering[B]): Agg[A, B] = new Agg()
  /** the general aggregate: a seeded left fold over the (surviving) elements. */
  def fold[A, S](z: S)(step: (S, A) => S): Agg[A, S] = new Agg()
  /** run `f` for its side effect on every (surviving) element; result is Unit. The single-aggregate base that
   *  the standalone `foreach`/`toList`/`toSet`/… terminals desugar to. */
  def foreach[A](f: A => Unit): Agg[A, Unit] = new Agg()
  /** mean of the projected field (sum / count, fused into one pass), or 0.0 if empty. */
  def avg[A](f: A => Double): Agg[A, Double] = new Agg()

  /** the `n` elements with the largest / smallest `key(a)`, best-first — via a bounded size-`n` heap, in ONE
   *  streaming pass: O(N log n) time, O(n) memory (no full sort, no O(N) buffer). Returns `FArray[A]`. */
  def topNBy[A, B](n: Int)(key: A => B)(using Ordering[B]): Agg[A, FArray[A]] = new Agg()
  def bottomNBy[A, B](n: Int)(key: A => B)(using Ordering[B]): Agg[A, FArray[A]] = new Agg()
  /** the `n` largest / smallest elements by their natural ordering, best-first. */
  def largest[A](n: Int)(using Ordering[A]): Agg[A, FArray[A]] = new Agg()
  def smallest[A](n: Int)(using Ordering[A]): Agg[A, FArray[A]] = new Agg()

  /** reduce the (surviving) elements with `op` (seeded by the first element, no zero), or None if empty. */
  def reduce[A, B >: A](op: (B, B) => B): Agg[A, Option[B]] = new Agg()
  /** like `reduce` but for a KNOWN-NON-EMPTY input → `B` (throws `NoSuchElementException` if empty). */
  def reduce1[A, B >: A](op: (B, B) => B): Agg[A, B] = new Agg()
  /** left-fold reduce seeded by the first element: `op(acc, a): B` with the element as the 2nd arg (the form the
   *  standalone `reduceOption`/`reduceLeftOption`/`min`/`max`/`last` terminals desugar to). None if empty. */
  def reduceL[A, B >: A](op: (B, A) => B): Agg[A, Option[B]] = new Agg()
  /** the ELEMENT with the smallest / largest `f(a)` (not the key — the element), or None if empty. */
  def minBy[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[A]] = new Agg()
  def maxBy[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[A]] = new Agg()
  /** like `minBy`/`maxBy` but for a KNOWN-NON-EMPTY input → `A` (throws if empty). */
  def minBy1[A, B](f: A => B)(using Ordering[B]): Agg[A, A] = new Agg()
  def maxBy1[A, B](f: A => B)(using Ordering[B]): Agg[A, A] = new Agg()
