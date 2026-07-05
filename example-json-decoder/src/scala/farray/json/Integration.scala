package farray.json

import farray.{Fuse, FuseMacro, RecordDecoder, StandardTerminals, Terminal}
import scala.quoted.*

// start:json-integration
// HOW A LIBRARY PLUGS INTO FUSION — the complete integration, one file.
//
// A fused pipeline is `Fuse[A, S]`: the STAGES (map/filter/…) are shape-generic markers from
// farray; the TERMINALS come from the shape's FuseLowering given. To fuse over your own byte
// format you provide:
//
//   1. a SHAPE     — the phantom type that names your format               (sealed trait Ndjson)
//   2. a LOWERING  — which terminal families your shape supports, plus ONE hook whose macro
//                    hands the engine your decoder                          (JsonLowering)
//   3. a SOURCE    — how complete records are framed out of bytes           (NdjsonSource, in Json.scala)
//   4. a DECODER   — macro-time: how one framed record's fields are read    (JsonDecode.scala)
//
// The engine keeps everything column-aware — live-set analysis, dead-field skipping,
// compute-for-survivors, the loop itself. The decoder only knows how to read ITS bytes.

/** the shape: `Json.ndjson[T](bytes).stream` stamps its pipelines as `Fuse[T, Ndjson]`. */
sealed trait Ndjson

object Ndjson:
  /** rides the implicit scope of every `Fuse[A, Ndjson]` — no imports at the use site. Declared at the singleton type so the lowering's terminals resolve as
    * members.
    */
  given lowering: JsonLowering.type = JsonLowering

/** the lowering: NDJSON supports the standard terminal families (aggregation, search, group-reduce, plan — everything except element materialization). Every
  * terminal funnels into `lower`, whose macro forwards pipeline + reified terminal to the engine together with OUR decoder — an ordinary method argument, at
  * compile time. This is the entire hook; there is no registry and no reflection.
  */
object JsonLowering extends StandardTerminals[Ndjson]:
  inline def lower[A, R](inline self: Fuse[A, Ndjson], inline t: Terminal[A, R]): R =
    ${ JsonLowering.impl[A, R]('self, 't) }

  def impl[A: Type, R: Type](self: Expr[Fuse[A, Ndjson]], t: Expr[Terminal[A, R]])(using Quotes): Expr[R] =
    FuseMacro.lower(self, t, JsonDecode)
// stop:json-integration
