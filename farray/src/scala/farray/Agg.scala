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
  /** minimum / maximum of the projected field, or None if empty. */
  def min[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[B]] = new Agg()
  def max[A, B](f: A => B)(using Ordering[B]): Agg[A, Option[B]] = new Agg()
  /** the general aggregate: a seeded left fold over the (surviving) elements. */
  def fold[A, S](z: S)(step: (S, A) => S): Agg[A, S] = new Agg()
  /** mean of the projected field (sum / count, fused into one pass), or 0.0 if empty. */
  def avg[A](f: A => Double): Agg[A, Double] = new Agg()
