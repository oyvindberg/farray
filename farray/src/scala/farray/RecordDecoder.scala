package farray

import scala.quoted.*

/** The engine-facing seam for lowering a decomposed [[ByteRecordSource]] (a source that produces a record's fields on demand). The fusion engine detects such a
  * source and calls `lower`/`planString` here, handing a [[DecomposedInput]]; it never names a concrete decoder. This indirection is the one place that binds
  * the decomposed-source contract to its implementation — today the NDJSON decoder (`farray.json.JsonDecode`); a future columnar/protobuf decoder would
  * dispatch here too (by the runtime source's type).
  */
private[farray] object RecordDecoder:
  /** lower the source to a per-record projection scanner that rejoins the shared optimizer via `in.continue`. */
  def lower(using q: Quotes)(in: DecomposedInput[q.type]): Expr[Unit] =
    farray.json.JsonDecode.lower(in)

  /** a machine-checkable description of the plan the decoder built (for the `.plan` terminal / tests). */
  def planString(using q: Quotes)(in: DecomposedInput[q.type]): String =
    farray.json.JsonDecode.planString(in)
