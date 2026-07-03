package farray

import scala.quoted.*

/** The engine-facing seam for lowering a decomposed [[ByteRecordSource]] (a source that produces a record's fields on demand). The fusion engine detects such a
  * source and calls `lower`/`planString` here, handing a [[DecomposedInput]]; it never names a concrete decoder.
  *
  * Decoders live DOWNSTREAM of the engine — the NDJSON one in the `example-json-decoder` project — and are discovered reflectively at macro-expansion time.
  * That works because the terminal macro expands while the USER's code is being compiled, and this classloader sees the user's compile classpath, which is what
  * carries the decoder. A future columnar/protobuf decoder is one more entry in `knownDecoders` (or, eventually, dispatch by the runtime source's type).
  */
private[farray] object RecordDecoder:
  private val knownDecoders = List("farray.json.JsonDecode")

  private lazy val plugin: Option[RecordDecoderSpi] =
    knownDecoders.iterator
      .flatMap { fqn =>
        try
          val cls = Class.forName(fqn + "$", true, getClass.getClassLoader)
          Some(cls.getField("MODULE$").get(null).asInstanceOf[RecordDecoderSpi])
        catch case _: ClassNotFoundException => None
      }
      .nextOption()

  private def required(using q: Quotes): RecordDecoderSpi =
    plugin.getOrElse(
      q.reflect.report.errorAndAbort(
        "fuse: this pipeline reads a record-framed byte source, but no record decoder is on the compile classpath. " +
          "Add a decoder module — e.g. `example-json-decoder`, which provides farray.json.JsonDecode."
      )
    )

  /** lower the source to a per-record projection scanner that rejoins the shared optimizer via `in.continue`. */
  def lower(using q: Quotes)(in: DecomposedInput[q.type]): Expr[Unit] = required.lower(in)

  /** a machine-checkable description of the plan the decoder built (for the `.plan` terminal / tests). */
  def planString(using q: Quotes)(in: DecomposedInput[q.type]): String = required.planString(in)
