package farray.json

/** Realistic NDJSON record shapes the fused-JSON tests and benchmarks project against. These are pure data
  * fixtures — no scanners, no logic. They live in main `src` (not test scope) only because BOTH the `tests` and
  * `benchmarks` projects use them and neither depends on the other.
  *
  * The point of the demo is *projection*: a query reads one or two fields and the macro skips the rest at the byte
  * level. So the fixtures are deliberately WIDE — many fields, only a few ever read.
  */

/** A 20-field record (mixed Long/Int/Double/String, names of varied length so the length-then-first-byte-then-memcmp
  * key dispatch is exercised). The headline query `filter(_.amount > t).map(_.amount).sum` reads 1 of 20 fields.
  */
final case class Rec(
    id: Long,
    ts: Long,
    age: Int,
    count: Int,
    score: Double,
    amount: Double,
    rank: Int,
    lat: Double,
    lon: Double,
    flags: Int,
    name: String,
    category: String,
    region: String,
    status: String,
    code: String,
    note: String,
    tag: String,
    kind: String,
    source: String,
    label: String
)

/** A "many numeric fields, read few" record: 16 `Double` metrics + an id. A query reading `m0`/`m5` decodes only
  * those and skips the other 14 as raw bytes — the compute-for-survivors win widens the more numeric fields a record
  * has (vs a parser that must decode every declared field).
  */
final case class FatRec(
    id: Long,
    m0: Double,
    m1: Double,
    m2: Double,
    m3: Double,
    m4: Double,
    m5: Double,
    m6: Double,
    m7: Double,
    m8: Double,
    m9: Double,
    m10: Double,
    m11: Double,
    m12: Double,
    m13: Double,
    m14: Double,
    m15: Double
)
