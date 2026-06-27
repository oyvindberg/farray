package farray

import scala.reflect.ClassTag

/** Fused-pipeline builder. `xs.fuse.map(f).filter(p).take(k).run` compiles to ONE unboxed traversal of `xs` producing ONE output collection — no intermediate
  * `FArray` per stage, no per-element boxing or virtual calls. See `docs/fused-pipeline-design.md`.
  *
  * The combinator methods below are MARKERS: their bodies never run. The terminal methods (`run`, …) are `inline` macros that read the whole `xs.fuse.…` chain
  * off the AST, peel the stage list + the (inlined) lambdas, and emit the fused loop. The `Fuse` wrapper itself is elided by the macro — only `xs` and the
  * lambda bodies survive into the generated code.
  *
  * Semantics — assume PURE stage functions. A fused pipeline is lazy/short-circuiting: `take`, `find`, `head`, `exists`/`forall` stop the traversal as soon as
  * the answer is known, so a stage function (incl. a `flatMap`'s) may run FEWER times than in the equivalent strict `List` pipeline (e.g.
  * `xs.fuse.flatMap(f).take(3)` invokes `f` only until 3 elements are produced). Likewise, when a `map` produces a tuple of independent columns and a later
  * `filter` uses only some of them, the OTHER columns are computed only for elements that pass the filter — "compute-for-survivors" (e.g.
  * `xs.fuse.map(x => (cheap(x), expensive(x))).filter(_._1 > 0).map(_._2)` runs `expensive` only on survivors, and never allocates the tuple). For a pure `f`
  * the result is identical; with a side-effecting or throwing `f` the observable behavior (call count, whether it throws) can differ. The element type must be
  * Int/Long/Double or a reference type (`<: AnyRef`) — a primitive-backed FArray widened to `Any`/`AnyVal` is a compile error, not a silent miscompile.
  */
// `base` is `AnyRef` (not `FBase`) so a non-array source (e.g. a byte-backed JSON NDJSON source) can flow
// through the same `Fuse` surface + terminals; the macro detects the source's static type at the base case
// and lowers accordingly (an `FBase` → the indexed array loop; a JSON source → a per-record scanner).
final class Fuse[+A](private[farray] val base: AnyRef):
  // ---- stage markers (bodies irrelevant; the macro reads these calls off the AST) ----
  def map[B](f: A => B): Fuse[B] = this.asInstanceOf[Fuse[B]]
  def flatMap[B](f: A => FArray[B]): Fuse[B] = this.asInstanceOf[Fuse[B]]
  def filter(p: A => Boolean): Fuse[A] = this

  /** like `filter`; also lets a for-comprehension guard (`for (x <- xs.fuse if p(x)) yield …`) fuse. */
  def withFilter(p: A => Boolean): Fuse[A] = this
  def filterNot(p: A => Boolean): Fuse[A] = this

  /** keep elements that match `pf`, mapping each through it — filter + map + pattern-match fused in one pass. */
  def collect[B](pf: PartialFunction[A, B]): Fuse[B] = this.asInstanceOf[Fuse[B]]
  def take(n: Int): Fuse[A] = this
  def drop(n: Int): Fuse[A] = this

  /** the LAST `n` elements, in order — a ring buffer (O(n) memory), one pass, no `reverse`. Nothing is emitted until the stream ends, so unlike `take` this
    * doesn't short-circuit the source; a downstream `take` does.
    */
  def takeRight(n: Int): Fuse[A] = this

  /** emit elements until `p` first fails, then stop the whole traversal (short-circuits like `take`). */
  def takeWhile(p: A => Boolean): Fuse[A] = this

  /** skip the leading run of elements matching `p`, then emit all the rest. */
  def dropWhile(p: A => Boolean): Fuse[A] = this

  /** keep only the first occurrence of each element (by `==`/hashCode). */
  def distinct: Fuse[A] = this

  /** keep only the first element for each distinct key `f(a)`. */
  def distinctBy[K](f: A => K): Fuse[A] = this

  /** running fold: emit `z`, then each successive `op(acc, a)` — yields one more element than the input. */
  def scanLeft[B](z: B)(op: (B, A) => B): Fuse[B] = this.asInstanceOf[Fuse[B]]

  /** Streaming group-aggregate for input ALREADY CLUSTERED by `key`: emit `(k, acc)` once per maximal run of equal keys, where `acc` starts at
    * `combine(seed, firstOfRun)` and folds the run with `combine`. O(1) memory (state = curKey, acc, started), no buffer, no hashmap, short-circuits under
    * `take` — the ordered-input counterpart of `groupMapReduce`. The "already clustered by key" precondition is the USER'S declaration; on unordered input the
    * result is per-run, not per-key (documented, not detected).
    */
  def foldAdjacentBy[K, B](key: A => K)(seed: B)(combine: (B, A) => B): Fuse[(K, B)] = this.asInstanceOf[Fuse[(K, B)]]

  /** Streaming GROUP stage for input ALREADY CLUSTERED by `key`: emit each maximal run of equal keys as its own `FArray[A]` (the rows, in order). O(largest
    * run) memory — buffers one run at a time, not all N — no hashmap, short-circuits under `take`. The materializing counterpart of `foldAdjacentBy` (use that
    * when you only need a per-run aggregate and never the rows). Same "clustered by key" user precondition.
    */
  def groupAdjacentBy[K](key: A => K): Fuse[FArray[A]] = this.asInstanceOf[Fuse[FArray[A]]]

  /** NESTED FUSION — the spec's headline: for input ALREADY CLUSTERED by `key`, reduce each run with a FUSED sub-pipeline and emit `(k, result)` per run.
    * `prep` is the per-group stages (map/filter/collect/take/…) over the run's rows; `agg` is the per-group aggregate (`Agg.sum`/`count`/`min`/`fold`/…). When
    * the aggregate is a fold, the run's rows are NEVER materialized — the inner fold runs inline as rows stream past: O(1) memory per group, zero per-group
    * allocation. Same "clustered by key" precondition. Example: {{{src.fuse.groupAdjacentReduceBy(_.day)(_.map(_.amount))(Agg.sum(identity))}}}
    *
    * (Spelled `(prep)(agg)` rather than `(reduce: Fuse[A] => B)` because an inner inline terminal like `.sum` expands before this macro reads the lambda;
    * `prep` is plain stage markers and `agg` is a macro-read value.)
    */
  inline def groupAdjacentReduceBy[K, B, R](inline key: A => K)(inline prep: Fuse[A] => Fuse[B])(inline agg: Agg[B, R]): Fuse[(K, R)] =
    ${ FuseMacro.groupReduceStageImpl[A, K, B, R]('this, 'key, 'prep, 'agg) }

  /** Internal non-inline marker the `.run`/agg macro reads off the AST for nested fusion. `agg` here is an `AggRaw.*` (the non-compileTimeOnly twin) —
    * `groupReduceStageImpl` already consumed the user's `Agg.*`. Not for direct use.
    */
  def groupAdjacentReduceByMarker[K, B, R](key: A => K)(prep: Fuse[A] => Fuse[B])(agg: Agg[B, R]): Fuse[(K, R)] =
    this.asInstanceOf[Fuse[(K, R)]]

  /** run `f` for its side effect on each surviving element, passing the element through unchanged. */
  def tapEach(f: A => Unit): Fuse[A] = this

  /** pair each element with its position in the stream at this point (post-upstream-filtering). */
  def zipWithIndex: Fuse[(A, Int)] = this.asInstanceOf[Fuse[(A, Int)]]

  /** lock-step with another source: pair element k of this pipeline with `that(k)`; stops at the shorter. */
  def zip[B](that: FArray[B]): Fuse[(A, B)] = this.asInstanceOf[Fuse[(A, B)]]

  /** lock-step combine with another source via `f` (like `zip(that).map(f)` but never builds the pair). */
  def map2[B, C](that: FArray[B])(f: (A, B) => C): Fuse[C] = this.asInstanceOf[Fuse[C]]

  // ---- derived stages (inline sugar over the markers above; still one fused pass) ----
  /** elements in index range `[from, until)` — `drop(from).take(until - from)`. */
  inline def slice(from: Int, until: Int): Fuse[A] = drop(from).take(until - from)

  /** concatenate a pipeline of `FArray`s — `flatMap(identity)`. */
  inline def flatten[B](using asArray: A <:< FArray[B]): Fuse[B] = flatMap(a => asArray(a))

  // ---- terminals (macros: rewrite the whole chain into one fused loop) ----
  inline def run: FArray[A] = ${ FuseMacro.runImpl[A]('this) }
  // foreach/foldLeft/count desugar to a single-aggregate `agg(...)`, sharing the Agg/AState terminal machinery
  // (one accumulator carried above the loop, one step per element). Same generated loop as the old dedicated
  // terminals; one lowering path instead of three.
  inline def foreach(inline f: A => Unit): Unit = agg(Agg.foreach[A](f))
  inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z = agg(Agg.fold[A, Z](z)(op))

  /** number of elements surviving the whole pipeline. */
  inline def count: Int = agg(Agg.count[A])

  /** number of elements matching `p` (one fused pass). */
  inline def count(inline p: A => Boolean): Int = filter(p).count
  // ---- short-circuit terminals: stop as soon as the answer is known (across flatMap nesting) ----
  inline def find(inline p: A => Boolean): Option[A] = ${ FuseMacro.findImpl[A]('this, 'p) }
  inline def exists(inline p: A => Boolean): Boolean = ${ FuseMacro.existsImpl[A]('this, 'p) }
  inline def forall(inline p: A => Boolean): Boolean = ${ FuseMacro.forallImpl[A]('this, 'p) }
  inline def headOption: Option[A] = ${ FuseMacro.headOptionImpl[A]('this) }
  inline def head: A = ${ FuseMacro.headImpl[A]('this) }

  /** A machine-checkable DESCRIPTION of the plan the macro built for this pipeline (not a real terminal — the pipeline is analyzed, never executed). For a JSON
    * source: which fields are scanned, which are decoded vs kept as lazy string slices, the predicate/early-out set, whether the record is rebuilt, and the
    * would-be terminal. Tests assert on this structure rather than on brittle generated code or coincidentally-correct output values. To include a
    * field-reading TERMINAL's contribution, pass it: `…plan(_.foldLeft…)` is not needed — instead append the terminal's projection as a `map` (e.g. test
    * `…map(_.amount).plan`).
    */
  inline def plan: String = ${ FuseMacro.planImpl[A]('this) }

  /** plan of the pipeline AS IF it ended in a fold whose `op` reads the element — captures a terminal that reads record fields directly (e.g.
    * `Json.ndjson[Rec](b).stream.planFold((acc, r) => acc + r.amount)`).
    */
  inline def planFold[Z](inline op: (Z, A) => Z): String = ${ FuseMacro.planFoldImpl[A, Z]('this, 'op) }

  /** plan of the pipeline AS IF it ended in these aggregates — so the agg live-set (the union of the aggregates' read fields) can be asserted.
    * `Json.ndjson[Rec](b).stream.planAgg(Agg.sum(_.amount), Agg.count)`.
    */
  inline def planAgg(inline aggs: Agg[A, Any]*): String = ${ FuseMacro.planAggImpl[A]('this, 'aggs) }

  // ---- multi-aggregate: run several aggregations in ONE fused pass, carrying one accumulator each. The
  //      aggregates' read fields are merged automatically (union computed, shared field once, survivor-gated). ----
  inline def agg[R1, R2](inline a1: Agg[A, R1], inline a2: Agg[A, R2]): (R1, R2) =
    ${ FuseMacro.aggImpl[A, (R1, R2)]('this, '{ List(a1, a2) }) }
  inline def agg[R1, R2, R3](inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3]): (R1, R2, R3) =
    ${ FuseMacro.aggImpl[A, (R1, R2, R3)]('this, '{ List(a1, a2, a3) }) }
  inline def agg[R1, R2, R3, R4](inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3], inline a4: Agg[A, R4]): (R1, R2, R3, R4) =
    ${ FuseMacro.aggImpl[A, (R1, R2, R3, R4)]('this, '{ List(a1, a2, a3, a4) }) }

  // ---- aggregate into a CASE CLASS (or any product): the aggregate results are passed to `make` (typically a
  //      case-class constructor) instead of a tuple — same single fused pass, same column merge. E.g.
  //      `xs.fuse.aggTo(Summary.apply)(Agg.sum(_.x), Agg.count, Agg.max(_.y))`. ----
  inline def aggTo[R1, R2, R](inline make: (R1, R2) => R)(inline a1: Agg[A, R1], inline a2: Agg[A, R2]): R =
    ${ FuseMacro.aggToImpl[A, R]('this, '{ List(a1, a2) }, 'make) }
  inline def aggTo[R1, R2, R3, R](inline make: (R1, R2, R3) => R)(inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3]): R =
    ${ FuseMacro.aggToImpl[A, R]('this, '{ List(a1, a2, a3) }, 'make) }
  inline def aggTo[R1, R2, R3, R4, R](
      inline make: (R1, R2, R3, R4) => R
  )(inline a1: Agg[A, R1], inline a2: Agg[A, R2], inline a3: Agg[A, R3], inline a4: Agg[A, R4]): R =
    ${ FuseMacro.aggToImpl[A, R]('this, '{ List(a1, a2, a3, a4) }, 'make) }

  /** run ONE aggregate, returning its result bare (no Tuple1). The single-aggregate base for the standalone sugar below; the whole pipeline still fuses.
    */
  inline def agg[R1](inline a1: Agg[A, R1]): R1 =
    ${ FuseMacro.aggImpl[A, R1]('this, '{ List(a1) }) }

  // ---- standalone top-N: the n best elements by a key, in ONE fused pass via a bounded size-n heap
  //      (O(N log n) time, O(n) memory — no full sort, no O(N) buffer). Returns FArray[A], best-first. ----
  /** the `n` elements with the largest `key(a)`, best-first. */
  inline def topNBy[B](n: Int)(inline key: A => B)(using Ordering[B]): FArray[A] = agg(Agg.topNBy(n)(key))

  /** the `n` elements with the smallest `key(a)`, best-first. */
  inline def bottomNBy[B](n: Int)(inline key: A => B)(using Ordering[B]): FArray[A] = agg(Agg.bottomNBy(n)(key))

  /** the `n` largest elements by natural ordering, best-first. */
  inline def topN[A1 >: A](n: Int)(using Ordering[A1]): FArray[A1] = agg(Agg.largest[A1](n))

  /** the `n` smallest elements by natural ordering, best-first. */
  inline def bottomN[A1 >: A](n: Int)(using Ordering[A1]): FArray[A1] = agg(Agg.smallest[A1](n))

  // ===== derived terminals — pure sugar over the base terminals above; the whole pipeline still fuses =====

  // ---- conversions (one fused pass into a builder via foreach) ----
  inline def toList: List[A] = { val b = List.newBuilder[A]; foreach(b += _); b.result() }
  inline def toVector: Vector[A] = { val b = Vector.newBuilder[A]; foreach(b += _); b.result() }
  inline def toSeq: Seq[A] = toVector
  inline def toSet[B >: A]: Set[B] = { val b = Set.newBuilder[B]; foreach(b += _); b.result() }
  inline def toArray[B >: A](using ClassTag[B]): Array[B] = { val b = Array.newBuilder[B]; foreach(b += _); b.result() }
  inline def toMap[K, V](using ev: A <:< (K, V)): Map[K, V] =
    val b = Map.newBuilder[K, V]; foreach(a => b += ev(a)); b.result()
  inline def mkString(start: String, sep: String, end: String): String =
    val sb = new java.lang.StringBuilder(start); var first = true
    foreach { a => if first then first = false else sb.append(sep); sb.append(String.valueOf(a.asInstanceOf[Object])) }
    sb.append(end).toString
  inline def mkString(sep: String): String = mkString("", sep, "")
  inline def mkString: String = mkString("", "", "")

  // ---- reductions (one fused pass via foldLeft) ----
  inline def fold[B >: A](z: B)(inline op: (B, B) => B): B = foldLeft[B](z)((acc, a) => op(acc, a))
  inline def sum[B >: A](using num: Numeric[B]): B = foldLeft[B](num.zero)((acc, a) => num.plus(acc, a))
  inline def product[B >: A](using num: Numeric[B]): B = foldLeft[B](num.one)((acc, a) => num.times(acc, a))
  // reduce/min/max/last desugar to `reduceOption` (a single-aggregate `agg(Agg.reduceL(...))`): seeded
  // accumulator, no per-element Option, one Some at the end. minBy/maxBy desugar to `agg(Agg.minBy/maxBy(...))`:
  // best element + its key in two vars. All share the Agg/AState terminal machinery.
  inline def reduceLeft[B >: A](inline op: (B, A) => B): B =
    reduceOption[B](op).getOrElse(throw new UnsupportedOperationException("reduceLeft on an empty fused pipeline"))
  inline def reduce[B >: A](inline op: (B, B) => B): B =
    reduceOption[B]((acc, a) => op(acc, a)).getOrElse(throw new UnsupportedOperationException("reduce on an empty fused pipeline"))
  inline def min[B >: A](using ord: Ordering[B]): A =
    reduceOption[B]((acc, a) => if ord.lteq(acc, a) then acc else a)
      .getOrElse(throw new UnsupportedOperationException("min of an empty fused pipeline"))
      .asInstanceOf[A]
  inline def max[B >: A](using ord: Ordering[B]): A =
    reduceOption[B]((acc, a) => if ord.gteq(acc, a) then acc else a)
      .getOrElse(throw new UnsupportedOperationException("max of an empty fused pipeline"))
      .asInstanceOf[A]
  inline def minBy[B](inline f: A => B)(using ord: Ordering[B]): A =
    minByOption[B](f).getOrElse(throw new UnsupportedOperationException("minBy on an empty fused pipeline"))
  inline def maxBy[B](inline f: A => B)(using ord: Ordering[B]): A =
    maxByOption[B](f).getOrElse(throw new UnsupportedOperationException("maxBy on an empty fused pipeline"))

  // ---- last (one fused pass — keeps the most recent survivor) ----
  inline def lastOption: Option[A] = reduceOption[A]((_, a) => a)
  inline def last: A = reduceOption[A]((_, a) => a).getOrElse(throw new NoSuchElementException("last of an empty fused pipeline"))

  // ---- predicates / counts ----
  inline def contains[B >: A](elem: B): Boolean = exists(_ == elem)
  inline def isEmpty: Boolean = !exists(_ => true)
  inline def nonEmpty: Boolean = exists(_ => true)
  inline def size: Int = count
  inline def length: Int = count

  // ---- positional / partial (short-circuit) ----
  // dedicated IndexWhere terminal: a bare `var idx` + position counter, no (value, index) pair allocated.
  inline def indexWhere(inline p: A => Boolean): Int = ${ FuseMacro.indexWhereImpl[A]('this, 'p) }
  inline def indexOf[B >: A](elem: B): Int = indexWhere(_ == elem)
  inline def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = find(pf.isDefinedAt).map(pf)

  // ---- generic conversion: build any collection from a std Factory in one fused pass ----
  inline def to[C1](factory: scala.collection.Factory[A, C1]): C1 =
    val b = factory.newBuilder; foreach(b += _); b.result()

  // ---- Option-returning reductions (empty → None instead of throwing). These now desugar to a single-aggregate
  //      `agg(...)`, sharing the Agg/AState terminal machinery (the dedicated ReduceOpt/ExtremumBy terminals were
  //      folded into `Agg.reduceL`/`Agg.minBy`/`Agg.maxBy`). ----
  inline def reduceOption[B >: A](inline op: (B, A) => B): Option[B] = agg(Agg.reduceL[A, B](op))
  inline def reduceLeftOption[B >: A](inline op: (B, A) => B): Option[B] = reduceOption[B](op)
  inline def minOption[B >: A](using ord: Ordering[B]): Option[A] =
    reduceOption[B]((acc, a) => if ord.lteq(acc, a) then acc else a).asInstanceOf[Option[A]]
  inline def maxOption[B >: A](using ord: Ordering[B]): Option[A] =
    reduceOption[B]((acc, a) => if ord.gteq(acc, a) then acc else a).asInstanceOf[Option[A]]
  inline def minByOption[B](inline f: A => B)(using ord: Ordering[B]): Option[A] = agg(Agg.minBy[A, B](f))
  inline def maxByOption[B](inline f: A => B)(using ord: Ordering[B]): Option[A] = agg(Agg.maxBy[A, B](f))

  // ---- groupBy: one fused pass into per-key builders ----
  inline def groupBy[K](inline f: A => K): Map[K, List[A]] =
    val m = scala.collection.mutable.LinkedHashMap.empty[K, scala.collection.mutable.Builder[A, List[A]]]
    foreach(a => m.getOrElseUpdate(f(a), List.newBuilder[A]) += a)
    m.view.mapValues(_.result()).toMap
  inline def groupMapReduce[K, B](inline key: A => K)(inline f: A => B)(inline reduce: (B, B) => B): Map[K, B] =
    val m = scala.collection.mutable.LinkedHashMap.empty[K, B]
    foreach { a =>
      val k = key(a); val b = f(a); m.updateWith(k) { case Some(o) => Some(reduce(o, b)); case None => Some(b) }
    }
    m.toMap

  /** group by `key`, combine each element's `value` per key with `reduce`, in ONE fused pass — into a primitive- keyed open-addressing map (an Int key stays
    * UNBOXED in the hot loop, an Int/Long/Double value too), boxing only at the final O(#keys) materialization to `Map[K,B]`. The fast analogue of
    * `groupMapReduce`.
    */
  inline def groupReduceBy[K, B](inline key: A => K)(inline value: A => B)(inline reduce: (B, B) => B): Map[K, B] =
    ${ FuseMacro.groupReduceByImpl[A, K, B]('this, 'key, 'value, 'reduce) }

  /** group-reduce with `value = identity` — combine the elements themselves per key. (`A1 >: A` so the covariant element type may appear in `reduce`'s
    * contravariant position, like `reduce`/`+:`.)
    */
  inline def groupReduce[K, A1 >: A](inline key: A => K)(inline reduce: (A1, A1) => A1): Map[K, A1] =
    groupReduceBy(key)(a => (a: A1))(reduce)

  /** count elements per key (unboxed Int accumulator). */
  inline def groupCount[K](inline key: A => K): Map[K, Int] =
    groupReduceBy(key)(_ => 1)((x, y) => x + y)

  /** sum the projected `value` per key (unboxed for Int/Long/Double values). */
  inline def groupSum[K, B](inline key: A => K)(inline value: A => B)(using num: Numeric[B]): Map[K, B] =
    groupReduceBy(key)(value)((x, y) => num.plus(x, y))

  // ---- multi-output terminals: one fused pass into two builders ----
  inline def partition(inline p: A => Boolean): (FArray[A], FArray[A]) =
    val y = List.newBuilder[A]; val n = List.newBuilder[A]
    foreach(a => if p(a) then y += a else n += a)
    (FArray.from(y.result()), FArray.from(n.result()))
  inline def span(inline p: A => Boolean): (FArray[A], FArray[A]) =
    val pre = List.newBuilder[A]; val post = List.newBuilder[A]; var go = true
    foreach(a => if go && p(a) then pre += a else { go = false; post += a })
    (FArray.from(pre.result()), FArray.from(post.result()))
  inline def unzip[A1, A2](using ev: A <:< (A1, A2)): (FArray[A1], FArray[A2]) =
    val l = List.newBuilder[A1]; val r = List.newBuilder[A2]
    foreach { a =>
      val t = ev(a); l += t._1; r += t._2
    }
    (FArray.from(l.result()), FArray.from(r.result()))

  // ---- windowing ----
  /** Fixed-size chunks (the last may be shorter) — a composable STAGE: `grouped(n)` emits an `FArray[A]` every `n` elements, so it fuses into the one pass AND
    * short-circuits under a downstream `take`. `xs.fuse.grouped(n).run` gives the same `FArray[FArray[A]]` the old terminal did, but you can now keep
    * transforming the chunks. O(n) memory.
    */
  def grouped(n: Int): Fuse[FArray[A]] = this.asInstanceOf[Fuse[FArray[A]]]

  /** overlapping windows of size `n` stepping by 1; a shorter-than-`n` stream yields one partial window. */
  inline def sliding(n: Int): FArray[FArray[A]] =
    require(n > 0, "sliding size must be > 0")
    val out = List.newBuilder[FArray[A]]; val window = scala.collection.mutable.ArrayDeque.empty[A]; var emitted = false
    foreach { a =>
      window += a; if window.length > n then window.removeHead()
      if window.length == n then { out += FArray.from(window.toList); emitted = true }
    }
    if !emitted && window.nonEmpty then out += FArray.from(window.toList)
    FArray.from(out.result())
