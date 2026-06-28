package farray

import scala.quoted.*

/** The boundary between the fusion engine and a decomposed-source decoder (a byte/columnar source that produces a
  * record's fields on demand), expressed only in `q.reflect` terms and functions over them — so the decoder never
  * names the engine's `Shape`/`Tup`/`Ctx`/`Stage`, and the engine never names the decoder's byte mechanics. One
  * `Quotes` is threaded through (`q.type`) so every Expr/Term/callback lives at the same staging level.
  *
  * Direction of flow:
  *   - engine → decoder: everything in [[DecomposedInput]] (the record type, the runtime [[ByteRecordSource]], the
  *     pipeline's field-reading lambdas as plain Terms, and a `continue` callback the engine owns).
  *   - decoder → engine: per record, a [[RecordColumns]] handed back THROUGH `continue` — the decomposed column
  *     reads, which the engine assembles into its own `Shape` and rejoins to the shared optimizer.
  */
final class DecomposedInput[Q <: Quotes & Singleton](using val q: Q)(
    /** the record case-class type whose fields are scanned. */
    val srcElem: q.reflect.TypeRepr,
    /** the runtime byte source (already `self.base.asInstanceOf[ByteRecordSource]`). */
    val src: Expr[ByteRecordSource],
    /** short-circuit gate: stop pulling records/chunks once a downstream `take`/`find` is satisfied. */
    val notDone: Expr[Boolean],
    /** the pipeline's `map`/`filter` stage lambdas, in order — for live-field analysis + the leading-filter early-out. */
    val stageLambdas: List[StageLambda[Q]],
    /** terminal readers whose parameter IS the record type (so they contribute live fields), pre-decomposed to
      * `(param, body)` by the engine (e.g. a fold op's element param, an `Agg.sum(_.x)` lambda) — the decoder just walks them.
      */
    val terminalRecordReaders: List[(q.reflect.Symbol, q.reflect.Term)],
    /** topN/reduce/extremumByElem retain the whole element → every field is live (the record is materialized). */
    val forcesWholeRecord: Boolean,
    /** the terminal's display name for the plan string (`"Fold"`/`"Agg"`/the TTag name). */
    val terminalName: String,
    /** the engine-owned rejoin: given the decomposed per-record columns, build the `Shape` + run the downstream. */
    val continue: RecordColumns[Q] => Expr[Unit]
)

/** one `map`/`filter` stage flattened to a plain lambda + its role. `isPositiveFilter` marks a `filter` (not
  * `filterNot`) — the only kind eligible for the inline predicate-fail early-out.
  */
final class StageLambda[Q <: Quotes & Singleton](using val q: Q)(
    val lambda: q.reflect.Term,
    val isFilter: Boolean,
    val isPositiveFilter: Boolean
)

/** what the decoder hands the engine per record: one column read per record field (live or dead), in declaration
  * order. The engine wraps each into its `Shape` (memoizing String columns) and rebuilds the record only if needed.
  */
final class RecordColumns[Q <: Quotes & Singleton](using val q: Q)(
    val columns: List[Column[Q]]
)

/** a single field's read, as a `Shape`-free CPS thunk: `read(k)` yields the field-value `Term` to `k` (so a lazy
  * String slice binds on first read). `isString` tells the engine to MEMOIZE (decode-once); a numeric is an already-
  * bound var and a dead field is a constant default, neither of which needs memoizing.
  */
final class Column[Q <: Quotes & Singleton](using val q: Q)(
    val read: (q.reflect.Term => q.reflect.Term) => q.reflect.Term,
    val isString: Boolean
)
