# Fusion integration design — one file, four nouns

> **Status: IMPLEMENTED** (branch restructure-docs). The one-file integration is
> `example-json-decoder/src/scala/farray/json/Integration.scala`; goldens confirmed identical modulo
> one inline-hop paren/ascription.

**Goal.** A developer reading the JSON module's integration file understands the whole model and
could write their own format integration (CSV, log lines, protobuf) by imitation. DX and performance
are both 100% goals: zero terminal boilerplate per module, zero runtime cost anywhere, generated
code byte-identical to today's.

**Validated.** The load-bearing mechanism was prototyped across all three modules
(farray → example-json-decoder → tests) and passes: terminal syntax defined ONCE in farray traits,
inherited by a module's lowering object; the module implements a single abstract `inline def lower`
hook whose macro receives the reified terminal (lambdas intact) and forwards to the engine with the
module's decoder as a plain argument. Static dispatch through the shape given works; sugar over
sibling terminals works; no reflection anywhere.

## The mental model — four nouns (plus two for byte formats)

A fused pipeline is `Fuse[A, S]`:

| noun | what it is | who provides it |
|---|---|---|
| **stages** | shape-generic markers on `Fuse` (`map`, `filter`, `collect`, …) — free, read off the AST | farray, once |
| **shape `S`** | a phantom type naming the source format (`Chunks`, `Ndjson`, your `Csv`) | the format module (1 line) |
| **lowering** | `S`'s `FuseLowering[S]` given: the terminals the shape supports (inherited syntax) + one `lower` hook | the format module (~5 lines) |
| **terminal** | the pipeline-ending call, reified as `Terminal[A, R]` and handed to the hook | farray, once |

For byte formats, two more:

| noun | what it is |
|---|---|
| **source** (`ByteRecordSource`) | frame records out of bytes — 6 members: "hand me the next complete record's byte range, in constant memory" |
| **decoder** (`RecordDecoder`) | macro-time: emit the per-field byte reads for one record; the ENGINE does live-set analysis, DCE, compute-for-survivors, and the loop |

The engine's split of labor is the part that makes a decoder small: the decoder only knows how to
read *its* bytes; everything column-aware is the engine's.

## Naming (renames from today)

| today | proposed | rationale |
|---|---|---|
| `RecordDecoderSpi` | `RecordDecoder` | it IS the decoder interface; the old reflective object is gone, freeing the name |
| `DecomposedInput` | `DecodeRequest` | what the engine asks of a decoder; answered through `.continue` |
| `RecordColumns` / `Column` | unchanged | already right |
| `JsonTerminalMacro` (9 wrappers) | gone | folded into one `impl` next to the hook |
| `FuseMacro.*ImplWith` (13 entries) | one `FuseMacro.lower(self, terminal, decoder)` | the single module-facing engine entry |
| — | `Terminal[A, R]` (new) | the reified terminal request — typed, marker-only, consumed at expansion |
| — | capability traits (new, below) | the shared syntax |
| `Chunks`, `Ndjson` | unchanged | shape names; fine |

## Capability traits — the shared syntax, written once in farray

```
trait FuseLowering[S]                      // the typeclass (search key; @implicitNotFound as today)
trait ShapeLowering[S] extends FuseLowering[S]:
  inline def lower[A, R](inline self: Fuse[A, S], inline t: Terminal[A, R]): R   // THE hook (abstract inline)

trait AggTerminals[S]        extends ShapeLowering[S]  // agg×4, aggTo×3 + ALL agg/foreach sugar:
                                                       // foreach foldLeft count count(p) sum product fold
                                                       // toList toVector toSeq toSet toArray to toMap mkString×3
                                                       // reduce* min* max* minBy* maxBy* last* topN* partition
                                                       // span unzip groupBy groupMapReduce sliding
trait SearchTerminals[S]     extends ShapeLowering[S]  // find exists forall indexWhere + contains isEmpty
                                                       // nonEmpty indexOf collectFirst
trait GroupTerminals[S]      extends ShapeLowering[S]  // groupReduceBy groupReduce groupCount groupSum
trait PlanTerminals[S]       extends ShapeLowering[S]  // plan planFold planAgg
trait MaterializeTerminals[S] extends ShapeLowering[S] // run head headOption

trait StandardTerminals[S] extends AggTerminals[S], SearchTerminals[S], GroupTerminals[S], PlanTerminals[S]
```

Scope stays in the types: a shape supports exactly what its lowering mixes in — an unsupported
terminal is *not a member*, at compile time. The JSON shape takes `StandardTerminals`; the native
shape takes `StandardTerminals + MaterializeTerminals`. A read-only telemetry format could take just
`AggTerminals + SearchTerminals`.

Every trait body is one-liners of the form
`inline def find(inline p: A => Boolean): Option[A] = lower(self, Terminal.find(p))`.
`Terminal`'s constructors are typed markers (`Terminal.find[A](p): Terminal[A, Option[A]]`,
`Terminal.agg2(a1, a2): Terminal[A, (R1, R2)]`, …), `@compileTimeOnly`-guarded, never evaluated —
the hook's macro consumes the tree.

## The engine entry — one method, module-facing

```scala
object FuseMacro:
  def lower[A: Type, S: Type, R: Type](
      self: Expr[Fuse[A, S]], t: Expr[Terminal[A, R]], decoder: RecordDecoder | Null
  )(using Quotes): Expr[R]
```

Parses the `Terminal` constructor off the tree (engine-owned, exhaustive — a hand-built `Terminal`
value is an `errorAndAbort`) and routes to the existing `core(tag, extras)`. The 13 per-terminal
entries collapse into this.

## The one-file integration (the deliverable)

`example-json-decoder`'s `Integration.scala` becomes the teachable artifact — the ENTIRE plug-in,
top to bottom, extracted verbatim onto the JSON docs page:

```scala
package farray.json

// HOW A LIBRARY PLUGS INTO FUSION — the complete integration, one file.
// You provide: a SHAPE (names your format), a LOWERING (which terminals you support + one hook
// that hands the engine your decoder), and SOURCES (how records are framed out of bytes).
// The decoder itself — the format's real work — is JsonDecode.scala.

/** the shape: the phantom type `.stream` stamps on its pipelines — `Fuse[T, Ndjson]`. */
sealed trait Ndjson
object Ndjson:
  given lowering: JsonLowering.type = JsonLowering   // rides the implicit scope of Fuse[A, Ndjson]

/** the lowering: NDJSON supports the standard terminal set; every one funnels into `lower`,
  * whose macro hands the engine OUR decoder — an ordinary argument, at compile time. */
object JsonLowering extends StandardTerminals[Ndjson]:
  inline def lower[A, R](inline self: Fuse[A, Ndjson], inline t: Terminal[A, R]): R =
    ${ impl[A, R]('self, 't) }
  private def impl[A: Type, R: Type](self: Expr[Fuse[A, Ndjson]], t: Expr[Terminal[A, R]])(using Quotes): Expr[R] =
    FuseMacro.lower(self, t, JsonDecode)

/** the source: NDJSON records framed out of an in-memory buffer (ByteRecordSource, 6 members). */
final class NdjsonSource[T](bytes: Array[Byte], from: Int, until: Int) extends ByteRecordSource:
  inline def stream: Fuse[T, Ndjson] = new Fuse(this)
  … 15 lines of framing (nextChunk/nextRecord/buf/recordStart/recordEnd) …
```

Per-module cost: **~10 lines of integration** + the decoder (the format's genuine work) + the
framing. Compare today's ~150 lines of duplicated terminal defs.

## DX details

- Missing lowering → `implicitNotFound` naming the module to add (exists today).
- Unsupported terminal → not a member (exists today; now *derived from the mixins*, not hand-curated).
- Hand-built `Terminal` value → compile error pointing at the shape's terminals.
- Docs: the JSON page's setup section shows `Integration.scala` whole (extraction markers around the
  file body), replacing the current piecemeal presentation. The sources page and guide reference the
  four nouns by these names.

## Performance

- Shapes and `Terminal` are fully erased: markers are consumed at expansion, `@compileTimeOnly`
  guarantees none survive; one extra inline hop (`terminal → lower`) folds away.
- **Acceptance test: the regenerated goldens must be identical to today's** (modulo inline-proxy
  binding names) — the refactor moves syntax, not lowering.

## Migration steps

1. farray: `Terminal` ADT + `FuseMacro.lower` dispatch (subsumes the 13 `…ImplWith` entries).
2. farray: capability traits; `ChunksLowering` becomes `StandardTerminals + MaterializeTerminals`
   with a 3-line hook; delete the hand-written terminal block.
3. Renames: `RecordDecoderSpi → RecordDecoder`, `DecomposedInput → DecodeRequest`.
4. json module: `Integration.scala` (one file); delete the boilerplate `JsonFuse.scala`.
5. Regenerate goldens (expect no semantic diff), full test suite, docs, site build.
