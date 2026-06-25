# A fused, projection-pushdown, constant-memory JSON parser for the JVM

*Research report. Goal: assess the feasibility and design of a Scala 3 macro that fuses a JSON parser INTO
the `fuse` pipeline, so the existing column/DCE/sink optimizer drives parsing — extracting only the fields a
query touches, decoding lazily for survivors, allocating no AST and no per-record object, in O(chunk) memory
over an NDJSON stream.*

*This is a sibling to `docs/streaming-roadmap.md` (the `Source`/chunk-driver keystone the parser plugs into)
and `docs/inside-the-fusion-macro.md` (the column/DCE/sink/CSE model the parser reuses). It does not touch
`docs/fs2-research.md` or `docs/ox-research.md`.*

---

## 0. TL;DR — the thesis, and the honest verdict

**Thesis.** A JSON object is a *product whose fields are columns, sourced from byte ranges.* `fuse` already
does product decomposition + dead-column elimination (DCE) + compute-for-survivors (sink) + lazy
materialization + CSE on in-memory columns. If a macro generates the per-record *scanner* with the same
column model, then:

```scala
src.parseJson[Record].filter(_.age > 18).map(_.name)
```

would (a) **projection-pushdown**: skip `age`/`name`-irrelevant fields at the byte level (no decode, no
alloc); (b) **predicate-pushdown**: parse `age` first, and if `_.age > 18` fails, *skip the rest of the
record's bytes* and emit nothing; (c) **lazy decode**: keep `name` as a `(start,len)` slice and turn it into
a `String` only for survivors; (d) **zero per-record allocation** for reduce/project pipelines; (e)
**O(chunk) memory** over an NDJSON stream via the roadmap's `Source`/chunk-driver.

**Verdict (detailed in §4): GO, but as a *niche scanner*, not a general parser.** The defensible,
genuinely-novel claim is narrow and real:

> **The fastest pure-JVM, zero-dependency way to run a *selective projection/filter/aggregate query* over an
> NDJSON byte stream in constant memory — because it is the only JVM parser that skips unwanted fields'
> bytes *and* never decodes/allocates survivors' unread fields, with the projection set fixed at compile
> time so there is no per-field dispatch.**

It can plausibly **beat jsoniter-scala on a projection query** (jsoniter must still byte-walk every skipped
value *and* fully decode + hash every key; it also allocates the full case class). It will **lose to
jsoniter on full-object parsing** (jsoniter is a mature, SWAR-tuned, near-optimal scalar parser — expect
roughly parity-to-1.5×-behind, not ahead). It cannot reach **simdjson C++** (2.0–2.5 GB/s structural-index
throughput) and is **~2–5× behind** on raw bytes-scanned/sec; but on a *highly selective* projection it can
*close or invert* that gap by simply scanning far fewer bytes than simdjson-java, whose stage-1 indexes the
**entire** document unconditionally. On ARM/Apple-Silicon it categorically beats simdjson-java, which has no
NEON path and runs ~40× *slower* than Jackson there.

The one experiment that proves or kills it is in §5.6.

---

## 1. SOTA inventory & techniques (verified against source)

Repos cloned and read at `~/pr/jsonresearch/{jsoniter-scala, simdjson-java, dsl-json}`. Numbers are cited
with their benchmark caveats; where a number is a derived/secondary figure it is marked as such.

### 1.1 jsoniter-scala — the JVM scalar king

Reader: `jsoniter-scala-core/jvm-native/src/main/scala/.../JsonReader.scala` (~5200 lines). Macro (Scala 3):
`jsoniter-scala-macros/shared/src/main/scala-3/.../JsonCodecMaker.scala`. SWAR helper:
`jsoniter-scala-core/jvm/src/main/java/.../ByteArrayAccess.java`.

**Cursor model.** A single mutable `byte[] buf` with `head` (next byte) / `tail` (one-past-last). The hot
paths thread the position as an explicit `pos: Int` *parameter* (so C2 keeps it in a register) and write
`head` back only at method exit. `nextToken` folds whitespace-skipping into the read with a neat trick:
`b == ' ' || b == '\n' || (b | 0x4) == '\r'` (the `| 0x4` collapses `\t` (0x09) and `\r` (0x0D)). For an
in-memory `byte[]`, `loadMore` is a no-op — it parses straight off the caller's array, zero-copy.

**Streaming refill.** `loadMore` *compacts* (shifts unconsumed bytes — from `mark` if a rollback anchor is
set, else `pos` — to the front of the same buffer), then refills the tail from `InputStream.read`/
`ByteBuffer.get`. The buffer doubles only when full and nothing can be compacted. This is exactly the
NDJSON chunk-boundary strategy (§2.6): a partial record at the buffer tail is preserved across the refill by
`mark`.

**Field dispatch (generated).** `JsonCodecMaker` emits one of two strategies based on the field set:
- **≤8 fields AND total name length ≤64 bytes** → a flat linear chain of `isCharBufEqualsTo` (no hashing).
- **otherwise** → a `@switch` (JVM `tableswitch`/`lookupswitch`) on a **compile-time-computed hash** of each
  field name, each case linearly disambiguating hash collisions. The compile-time hash recurrence
  `h = (h << 5) + (cs(i) - h)` is *bit-identical* to the runtime `charBufToHashCode`, so the macro pre-bakes
  the switch labels. Unknown keys → `skip()` (default `skipUnexpectedFields = true`).

**The cost that a projection parser can undercut:** every key is first **fully decoded into a reused
`charBuf` and hashed** (`readKeyAsCharBuf`) *before* dispatch — even the 18 keys you will discard. There is
no "compare key bytes directly in the buffer against the wanted names and skip on first mismatch" path.

**Number parsing.** `readInt`/`readLong` accumulate digits with a SWAR fast path: read 4 ASCII digits as a
little-endian `int` via `ByteArrayAccess.getInt`, validate all-four-in-`0..9` with the classic
`((dec + 0x76767676 | dec) & 0x80808080) == 0` trick, then convert 4 digits to a number with two multiplies
(`(dec * 2561 >> 8 & 0xFF00FF) * 6553601 >> 16`). Accumulation uses *negated magnitude* (runs negative) so
`Int.MinValue` doesn't overflow. `readDouble` is **tiered**: tier-1 integral mantissa → `Long.toDouble`;
tier-2 (mantissa < 2^52, exponent in the exactly-representable window) → a single multiply/divide against a
`pow10Doubles` table (provably correctly rounded); tier-3 a custom 128-bit integer path (credited to
rust-lexical) using `unsignedMultiplyHigh` against a `pow10Mantissas` table; **only** when correct rounding
can't be proven does it fall back to `java.lang.Double.parseDouble(new String(...))` (rare, allocates).

**String parsing.** `parseString` is SWAR over 4 bytes (borrowed from *borer*): load 4 bytes, widen to 4
chars, one mask `((bs - 0x20202020 ^ 0x3C3C3C3C) - 0x1010101 | (bs ^ 0x5D5D5D5D) + 0x1010101) & 0x80808080`
detects a quote/backslash/control/non-ASCII byte anywhere in the word; the clean path writes 4 chars with no
per-byte branch. `numberOfTrailingZeros(m) >> 3` finds which byte tripped. Escapes/`\uXXXX`/surrogate
pairs/UTF-8 multibyte go to `parseEncodedString`. The `charBuf` (4096 chars) is **reused across all
strings/keys** in the document. Keys cost **zero** String allocations; each kept string value costs **one**
`new String(charBuf, 0, len)`.

**Value skipping — the projection primitive.** `skip()` dispatches on the first token:
- `skipString`: scan bytes to the unescaped closing quote; on `\`, jump 2 (so `\"` doesn't terminate). No
  decode, no charBuf write.
- `skipObject`/`skipArray`: **brace/bracket depth counting**, routing strings through `skipString` so braces
  *inside* string values don't miscount. This string-awareness is the correctness subtlety.
- `skipNumber`: consume the maximal `[0-9.eE+-]` run, no parsing.
- `skipFixedBytes(n)`: `null`/`true` → `pos+3`, `false` → `pos+4` (does *not* validate the literal bytes).

**Where it allocates (per record, projection query of 2-of-20):** one case-class instance (all 20 fields,
even skipped slots exist as `var`s); one `String` per kept string value; collection builders for
collection-typed fields. **Redundant work it cannot avoid:** it fully decodes + hashes **all 20 keys**; it
byte-walks **all 18 skipped values** to find their ends; it constructs the full declared case class. Pushdown
in jsoniter saves *decode/alloc of skipped values' contents*, not the *key decode* and not the *linear byte
walk*.

Published throughput: jsoniter-scala's own JMH suite reports it consistently fastest among Scala libraries
(borer, circe, jackson-module-scala, play-json, zio-json, uPickle, smithy4s) for both reads and writes, with
the lowest `gc.alloc.rate.norm`. Concrete sample (their suite, GeoJSON/Twitter-API models): on the order of
*1.5–4× faster than circe/jackson-module-scala and an order of magnitude less allocation*. In absolute terms
a well-tuned scalar JVM parser like jsoniter lands roughly in the **200–400 MB/s** range on mixed real-world
JSON (cf. Lemire's "how fast can you parse JSON" scalar baselines and DSL-JSON figures below); it is the
parser to beat on the JVM.

### 1.2 DSL-JSON — Java annotation-processor codegen

Generates encoders/decoders at compile time (annotation processor), reflection-free, works at the byte
level with near-zero allocation. Independent benchmarks (fabienrenaud/java-json-benchmark; the Medium/blog
write-ups) put it **~3–5× faster than Jackson** for deserialization in tight loops and **~7×** in some
serialization cases. Same family as jsoniter (codegen, byte-level, low alloc); the Scala analogue's
advantages over DSL-JSON would be the same projection/lazy techniques, not the basic codegen.

### 1.3 Jackson (+ Afterburner/Blackbird) — the streaming baseline & the "manual projection" yardstick

Data-binding (`ObjectMapper`) is reflection-based and slower/allocation-heavy. **Afterburner** (and its
Java-11+ successor **Blackbird**) replace reflection with bytecode-generated accessors and do *speculative
ordered field-name matching* in the parser; the published win is **20–40%** over vanilla Jackson, and "use
the streaming API instead of data binding can add 30–40%."

**The streaming `JsonParser` is the closest existing "manual projection" baseline for us.** A hand-written
loop using `nextFieldName(SerializableString)` (a pre-interned name matched against raw bytes without
building a String) + `skipChildren()` (skip an unwanted subtree) + per-field `getIntValue`/`getText` is
*exactly the manual version of what our macro generates.* It is the honest baseline to beat: if the macro
can't beat a careful Jackson-streaming projection loop, the whole thesis is in question. (Caveat: Jackson's
`nextFieldName(SerializableString)` is documented to be sub-optimal for byte-backed parsers — issue
fasterxml/jackson-core#220 — so a tight jsoniter-style hand loop is a tougher baseline.)

### 1.4 simdjson / simdjson-java — the two-stage SIMD ceiling

**C++ simdjson.** Two stages. Stage 1: a SIMD sweep classifies *every* byte (whitespace / structural /
in-string), validates UTF-8 (~13 GB/s), and emits a compressed **structural index** (positions of
`{}[]:,"` and value starts) at **2.0–2.5 GB/s** regardless of document content. Stage 2 walks that index.
The **On-Demand API** "feels like DOM but is entirely lazy — decoding only the parts of the document you
access," skipping the rest via the index. C++ simdjson is ~4× RapidJSON, ~25× nlohmann/json. **This is the
ceiling.** Note even C++ On-Demand still pays full stage-1 over the whole document.

**simdjson-java** (pure JVM, `~/pr/jsonresearch/simdjson-java`, v0.4.0). A faithful port using
`jdk.incubator.vector` (Panama Vector API). Key facts verified from source:
- **Stage 1 uses the Vector API** in `StructuralIndexer` (the `index256`/`index512` methods),
  `Utf8Validator`, `StringParser`. Quote/backslash/structural masks are computed with
  `ByteVector.eq(...).toLong()` → 64-bit bitmasks, plus the simdjson **nibble-lookup-via-shuffle**
  (`rearrange(VectorShuffle)` standing in for `pshufb`) for whitespace/operator classification.
- **No hardware carry-less multiply** (the Vector API exposes no `pclmulqdq`): the in-string mask is computed
  with a **software prefix-XOR** (6 shift-XOR steps over a `long`) — `prefixXor`. This is correct but is
  scalar work per 64-bit lane.
- **Hardware-locked to AVX2/AVX-512.** `VectorUtils` accepts only 256-/512-bit species and *throws*
  otherwise — **no scalar fallback, no 128-bit/NEON path.** On Apple Silicon / ARM (128-bit preferred), it
  either throws at class-init or (if forced) is catastrophic.
- **Build requires JDK 24+;** the Vector API is **still incubator** (JEP 529 = *eleventh* incubator round as
  of JDK 25) — must pass `--add-modules jdk.incubator.vector`, API unstable across releases by design.
- **On-Demand / schema path is genuinely lazy at stage 2** (`SchemaBasedJsonIterator.collectArguments`:
  matches keys on raw UTF-8 bytes via a hashed map, parses wanted values, `skipChild()`s unwanted subtrees by
  pointer-advancing over the structural index, and *early-exits the object once all wanted fields are
  collected*). **But stage 1 is unconditional:** it SIMD-classifies and writes the structural index for the
  *entire* document no matter how few fields you read. Default buffers are huge (34 MiB index `int[]`, 34 MB
  padded input, 34 MB string buffer), reused across parses.

**Verified numbers (simdjson-java README, twitter.json ≈631 KB, "parse + find default-profile users"):**
- AVX-512 (Xeon Platinum 8375C): schema **3164 ops/s** (≈ **2.0 GB/s**), DOM **1842 ops/s** (≈ 1.16 GB/s).
- AVX2 (Xeon E5-2686 v4): schema **1237 ops/s** (vs jsoniter-scala 614, jackson 340 — ~2× faster),
  DOM **784 ops/s** (vs jackson 260).
- **The crucial caveat:** the schema parser is only **~1.6×** the DOM parser, *not* orders of magnitude,
  *because stage 1 dominates and is projection-blind*. The lazy stage-2 win is small relative to the
  unconditional stage-1 cost.
- **ARM/M1 (independent JMH, "parse and select"):** simdjson-java **26 ops/s** vs Jackson **1100 ops/s** —
  a **~40× LOSS** (no NEON path → falls off a cliff). This is a decisive point for a pure-JVM,
  platform-portable target.

### 1.5 Inventory summary — where each parser does redundant work for a 2-of-20 projection

| Parser | Skips unwanted value bytes? | Decodes unwanted keys? | Allocs per record | Indexes whole doc? | Lazy survivor decode? | Platform-portable? |
|---|---|---|---|---|---|---|
| jsoniter-scala | walks bytes (skip) | **yes, all keys** | full case class + strings | n/a (one pass) | no (parses all kept fields) | yes (scalar) |
| Jackson streaming | `skipChildren` walks bytes | partly (`nextFieldName` matches bytes) | none if you hand-loop | n/a | manual | yes |
| simdjson-java | yes (index-advance) | matches bytes | result record + strings | **yes, stage 1, always** | yes (stage 2) | **no (x86 AVX only)** |
| **fused parser (proposed)** | **yes, byte-skip** | **no — byte-compare only** | **none** (reduce/project) | no | **yes** | **yes (scalar)** |

The two columns where the fused parser is uniquely "all-green" — *no key decode* and *no per-record alloc* —
are the thesis. The column where it can't win is raw structural throughput (no SIMD index).

---

## 2. The hard sub-problems & concrete recommendations

### 2.1 Number parsing

- **Int/Long:** lift jsoniter's negated-magnitude digit accumulation with the 4-byte SWAR fast path verbatim
  (`ByteArrayAccess`-style little-endian `getInt` via `MethodHandles.byteArrayViewVarHandle`, the
  all-digits mask, the two-multiply 4-digit conversion). This is ~optimal scalar and battle-tested. ~80 MB/s
  → with SWAR multiple-×.
- **Double — the genuinely hard one.** Correct `double` parsing is the correctness landmine (double-parse
  must be bit-exact with `Double.parseDouble`, or queries silently disagree). **Recommendation: tier exactly
  like jsoniter** — (1) integral fast path; (2) Eisel-Lemire / fast-path: parse mantissa (≤19 digits) as a
  `long`, normalize (CLZ), 128-bit-multiply against a precomputed power-of-ten table (`Math.multiplyHigh` /
  `unsignedMultiplyHigh`), extract 54 bits, round-to-even; (3) **fall back to `java.lang.Double.parseDouble`
  on a substring when Eisel-Lemire's half-way ambiguity test trips** (`XLo==0 && (XHi & 0x1FF)==0 &&
  (X54 & 3)==1`, plus >19 digits or out-of-range exponent). Eisel-Lemire is ~9× `strtod`; the JVM-proven
  reference is **Werner Randelshofer's FastDoubleParser** (Java port of Lemire's fast_float), which Jackson
  adopted: **byte[] path ≈ 694 MB/s vs `Double.parseDouble` ≈ 81 MB/s, a 7.9× speedup** (their JMH, Java 22).
  *Do not invent a double parser.* Reuse jsoniter's `toDouble` logic / link FastDoubleParser's algorithm.
  The fallback's substring allocation is acceptable because it is rare (only genuinely-ambiguous doubles).

### 2.2 String / escape / UTF-8 / lazy decode

- **Escape-free fast path:** the SWAR-4 quote/backslash/control scan from jsoniter/borer. The common JSON
  string is ASCII and escape-free; the fast path must write nothing extra.
- **The lazy-decode win is the differentiator.** For a string field, **do not decode at scan time** — record
  a slice `(start, len, hasEscapes: Boolean)` where `start`/`len` index into the chunk's `byte[]` and
  `hasEscapes` is set if the fast scan saw a `\` or non-ASCII byte. Materialize a `String` only when the
  pipeline forces it (output element, `==`, opaque call). If `!hasEscapes`, decode is the cheap
  `new String(buf, start, len, ISO_8859_1)`-style latin1 fast path *or* the ASCII fast path; only `hasEscapes`
  goes to the full `parseEncodedString` (escapes, `\uXXXX`, surrogate pairs, UTF-8 multibyte). This is the
  byte-source analogue of `fuse`'s "compute-for-survivors": a `name` behind a `filter(_.age > 18)` is sliced
  for everyone but `String`-ified only for survivors.
- **Subtlety:** a slice that points into a chunk `byte[]` is only valid until the chunk is recycled. For
  streaming, either (a) decode survivors' kept strings to `String` before the chunk is released (the safe
  default — survivors are rare under a selective filter, so this is cheap), or (b) copy the bytes into a
  small per-survivor arena. The slice-and-defer is *within* a record always safe (the whole record lives in
  one chunk — see §2.6).
- **`\uXXXX` + surrogates:** lift jsoniter's `readEscapedUnicode` (nibble table) + high/low surrogate
  validation verbatim. **UTF-8 validation:** for the bytes we actually decode, validate inline (jsoniter
  does branchless XOR-formula multibyte decode). For *skipped* fields we do **not** validate UTF-8 — this is
  a deliberate, documented correctness relaxation (a query that reads 2 of 20 fields will not reject a
  document for invalid UTF-8 in an unread field; simdjson *would*, because stage-1 validates everything).
  This is a feature for speed and a footnote for strict-validation users.

### 2.3 Structural scanning

The minimal scalar scanner needs: skip whitespace; recognize `{ } [ ] : , "` and value-start bytes; track
object/array nesting *only when skipping*. We do **not** need a full structural index — the macro knows the
record shape statically, so the generated scanner walks fields directly (key-scan → dispatch → value-scan or
value-skip) without materializing positions. This is the central asymmetry vs simdjson: **simdjson must build
an index because it doesn't know the query; we don't, because we do.**

**Vector API for structural classification — feasibility, honestly.** Could `jdk.incubator.vector` do
simdjson-style quote/backslash/structural masks in pure JVM? simdjson-java *proves it's possible* and gives
us the exact recipe (`ByteVector.eq().toLong()`, `rearrange` for `pshufb`, software `prefixXor` for the
in-string mask since there's no `pclmulqdq`). **But the risks are severe for this project:**
1. **Incubator instability** — JEP 529 is the 11th incubator (JDK 25); API churns every release; requires
   `--add-modules` (hostile to library users). The `fuse` roadmap explicitly avoided shipping preview APIs
   (it rejected `StructuredTaskScope` for being preview).
2. **No NEON/portable path** — simdjson-java throws on ARM and runs ~40× slower than Jackson on M1. A
   pure-JVM library that *loses 40× on Apple Silicon* contradicts priority #3 (portable, zero-dep).
3. **It indexes the whole document** — the SIMD win is structural-throughput, which is exactly the work a
   *projection* query wants to *skip*. SIMD and projection-pushdown pull in opposite directions: SIMD is
   "classify everything fast," pushdown is "touch as little as possible."
4. **Autovectorization won't save us** — JSON's data-dependent control flow (quotes, escapes, variable field
   lengths) defeats C2's auto-vectorizer, which needs counted loops with regular access. The only SIMD route
   is *explicit* Vector API, which reintroduces (1)–(3).

**Recommendation: ship the scalar scanner; treat the Vector API as a strictly-optional, x86-only,
opt-in stage-1 accelerator for the *full-parse* (non-projective) path, behind a flag, much later — if ever.**
For the projection thesis it is the wrong tool. The honest framing: "we beat simdjson-java on selectivity
*and* portability precisely by **not** doing SIMD."

### 2.4 Fast value skipping (the core of projection pushdown)

Lift jsoniter's `skip*` family verbatim, but generate them inline and specialize on the *known* value type
where possible (the macro knows the wanted field is `Int`, so a *skipped* sibling that the schema declares as
`String` can use the tighter `skipString`). The string-aware brace counting (`skipObject`/`skipArray` route
through `skipString`) is the correctness-critical part — get it from jsoniter, with the same fuzz coverage.
For *flat* records (the first deliverable), skipping is just `skipString`/`skipNumber`/`skipFixedBytes` plus
`skipObject`/`skipArray` for any nested-object field we don't descend into — straightforward.

### 2.5 Key dispatch — the macro's edge over jsoniter

The macro knows the **N wanted field names** at compile time and (critically) can compare *raw key bytes in
the buffer* against them **without decoding the key into a char buffer at all** — jsoniter's structural
weakness. Recommended generated dispatch:
1. On entering a key, the scanner has `keyStart` and (after the quote scan) `keyLen` in the buffer.
2. **First branch on `keyLen`** (a single `int` compare; most wanted names have distinct lengths). For each
   candidate length, **branch on the first byte** (`buf(keyStart)`), then do a **length-bounded `memcmp`**
   (`Arrays.equals(buf, keyStart, keyStart+len, NAME_BYTES, 0, len)` — JDK-intrinsified) against the
   compile-time-baked `byte[]` of the wanted name. Generate this as a static `match`/`if`-tree from the
   field-name set; for larger sets, a **compile-time perfect-hash or trie over the name bytes** (the macro
   can compute a minimal perfect hash at compile time, like jsoniter bakes its switch labels).
3. **Any key not in the wanted set short-circuits to `skip()` after the very first mismatching byte** — no
   decode, no charBuf, no hash of the full key. This is strictly less work than jsoniter, which decodes *and*
   hashes every key before it can dispatch.
4. **Early-out:** once all wanted fields are collected (a small popcount/bitset of "seen wanted fields"), the
   generated code can `skipObject` the rest of the record immediately (like simdjson-java's schema
   early-exit) — relevant when wanted fields appear early in the record.

### 2.6 NDJSON chunk-boundary handling

NDJSON = one JSON value per line, `\n`-delimited. The streaming driver (roadmap §S1) pulls fixed-size
`byte[]` chunks. A record can straddle two chunks. Strategy (jsoniter's `loadMore` compaction is the
reference):
- The chunk driver scans for `\n` boundaries. The fused per-record scanner runs over `[lineStart, lineEnd)`
  entirely within one buffer — so **a whole record is always contiguous in memory**, which is what makes
  intra-record slices (§2.2) safe.
- When the last line in a chunk is **partial** (no terminating `\n` before `tail`), **compact**: copy the
  partial-line bytes `[lineStart, tail)` to the front of the buffer (or a carry buffer), then refill the
  tail from the source. Cost: one `arraycopy` of *at most one record's worth* of bytes per chunk boundary —
  amortized negligible (one partial line per ~chunk-many lines). Grow the buffer only if a single record
  exceeds the buffer size (pathological).
- This integrates cleanly with the roadmap's `Source[A] { pullChunk(): FArray[A] | End }` keystone: the JSON
  source is a `Source[Record]`-shaped thing whose `pullChunk` yields a chunk of *parsed/projected records*
  (or, for the zero-alloc reduce case, drives the fold directly). The "above the chunk loop" state machinery
  (roadmap §3a/§S3) holds the carry buffer and the partial-line offset across chunks.

---

## 3. The fused-parser design (the novel part)

### 3.1 The mapping: byte-sourced fields ARE columns

`fuse`'s in-memory model (from `docs/inside-the-fusion-macro.md`): a `map` body that builds a product is a
set of independent **lazy columns** (`Shape = Sc(scalar) | Tup(parts, rebuild)`); each column is a CPS thunk
that binds to a `val` on first read and memoizes. DCE = a never-read column emits nothing; sink = a column
read only inside a guard emits *inside* that guard (compute-for-survivors); CSE = shared sub-expression bound
once.

**The fused parser replaces the *source* of each column.** Instead of a column being "an arithmetic
expression over the array element `a(i)`," a column is **"decode field F from the current record's bytes."**
Everything downstream of the source is the *existing* `fuse` machinery — untouched. The parser is a new kind
of `Source` (roadmap §S1) whose elements are *records*, but with a twist: the record's fields are exposed to
the optimizer as a `Tup` of lazy byte-sourced columns, so DCE/sink/CSE apply to *parsing work* exactly as
they apply to arithmetic today.

Concretely, `src.parseJson[Record]` produces a `Fuse[Record]` whose underlying element shape, as seen by the
macro, is **not** an opaque `Record` value but a decomposed `Tup([col_field1, col_field2, ..., col_fieldN],
rebuild = Record.apply)`. Each `col_fieldK` is a lazy thunk "scan/decode field K from the record bytes."
Then:
- `.map(_.name)` reads only `col_name` → DCE drops every other field's *decode*.
- `.filter(_.age > 18)` reads `col_age` → it's bound *before* the survivor region; the sink pushes
  `col_name`'s decode *inside* the `if`.
- A field neither projected nor filtered → its column is dead → **its bytes are skipped, not decoded.**
- `Record` is `rebuild`-materialized (allocated) **only** if the pipeline uses the whole record (passes it to
  an opaque function, emits it as output). A reduce/project pipeline never allocates `Record`.

The beautiful part: **this requires almost no new optimizer.** The column model, DCE, sink, and CSE already
exist and are validated. The new work is (a) a parser `Source` and (b) teaching the macro that *this
particular source* presents its element as a pre-decomposed `Tup` of byte-sourced columns whose "read" thunk
emits scan/decode code instead of array-index code.

### 3.2 What "read a byte-sourced column" emits, and the two-pass-within-a-record problem

There's a wrinkle the in-memory model doesn't have: **JSON fields are not random-access.** To read
`col_age` and `col_name`, the scanner must walk the record's bytes left-to-right; it can't jump to `name`.
And the *order of decode* should be: cheap predicate fields first (for early-out), survivors' projected
fields second. But the *byte order* in the record is whatever the producer wrote.

Two viable lowerings:

**(A) Single-pass scan into per-field slots (recommended for the first deliverable).** The generated
per-record scanner does **one left-to-right pass**: for each key, dispatch (§2.5); if the key is a *wanted*
field, record its value's slice/decoded-primitive into a local `var` slot; else `skip()`. After the pass,
the columns read from the slots. This means: predicate fields and projected fields are *all* captured in the
one pass, but **decode is still deferred** — a wanted string field's slot holds a `(start,len,hasEscapes)`
slice, not a `String`, so the sink still defers `String`-ification past the filter. Numbers needed by the
predicate are decoded in-pass (cheap, and the predicate needs them). The early-out is: as soon as all
*predicate* fields are filled, evaluate the predicate; if it fails, `skipObject` the remainder and emit
nothing (don't bother filling projected-only slots).

- *Pro:* one pass, simple, robust, handles fields in any order.
- *Con:* to early-out *before* scanning the whole record, the predicate fields must appear before the point
  we stop; if `age` is the last field, we scan the whole record anyway (but still skip-decode the others).

**(B) Targeted multi-probe (a later optimization).** Generate a scanner that scans for the predicate field
first (skipping others), evaluates the predicate, and only on survival rescans for projected fields. This
double-scans the bytes but decodes/allocates strictly less for the rejected majority. Worth it only when
records are large and selectivity is low and predicate fields are early. **Start with (A); measure; add (B)
behind a heuristic or a user hint only if the benchmark demands it.** (Note the macro's own rule from the
fusion docs: *never reorder for impure code* — but parsing is pure, so reordering scans is sound here; the
constraint is purely about producing identical *results*, which both orders do.)

### 3.3 The generated loop (sketch)

For `src.parseJson[Rec].filter(_.age > 18).map(_.name).toList`, where
`case class Rec(id: Long, age: Int, name: String, /* ...17 more... */)`, lowering (A):

```scala
// ---- above the chunk loop: streaming state (roadmap §S1/§S3) ----
val source   = src                       // a Source over NDJSON bytes
var buf      = source.firstChunk         // byte[]; refilled/compacted across chunks
var bufTail  = source.firstTail
val nameBytes = Array[Byte]('a','g','e')  // compile-time-baked wanted-key bytes (one per wanted field)
val out      = scala.collection.mutable.ListBuffer.empty[String]   // toList sink

// ---- chunk loop ----
var done = false
while (!done) {
  // find line bounds in buf; compact a straddling partial line, refill (see §2.6)
  var lineStart = ...; 
  while (lineStart < bufTail && !done) {
    // ===== one record scanner (generated from Rec's fields ∩ {age, name}) =====
    var p = expectObjectOpen(buf, lineStart)        // skip ws, expect '{'
    // wanted-field slots (ONLY for fields the pipeline touches; DCE dropped the other 18):
    var ageVal   = 0;        var ageSeen   = false   // predicate field → decoded primitive
    var nameStart = 0; var nameLen = 0; var nameEsc = false; var nameSeen = false  // lazy slice
    var keep = true
    while ({ p = skipWs(buf, p); buf(p) != '}' }) {
      val ks = p + 1                                  // key bytes start (after '"')
      val ke = scanStringEnd(buf, ks)                 // position of closing quote
      val klen = ke - ks
      p = expectColon(buf, ke + 1)
      // ---- compile-time-generated key dispatch: length → first byte → memcmp (§2.5) ----
      if (klen == 3 && buf(ks) == 'a' && Arrays.equals(buf, ks, ke, AGE_BYTES, 0, 3)) {
        ageVal = readInt(buf, p); ageSeen = true; p = afterInt
        if (ageVal <= 18) { keep = false; p = skipObjectRest(buf, p); /* break inner */ }
      } else if (klen == 4 && buf(ks) == 'n' && Arrays.equals(buf, ks, ke, NAME_BYTES, 0, 4)) {
        // LAZY: record the slice, do NOT decode to String yet (sink defers past the filter)
        val s = scanStringValue(buf, p)               // returns (start,len,hasEsc), no String
        nameStart = s.start; nameLen = s.len; nameEsc = s.esc; nameSeen = true; p = s.end
      } else {
        p = skipValue(buf, p)                          // §2.4 — unwanted field: byte-skip, no decode/alloc
      }
    }
    // ===== back in the pipeline: the fused downstream =====
    if (keep) {                                        // filter(_.age > 18) — the predicate already early-outed
      // sink: name decoded to String ONLY for survivors (compute-for-survivors)
      val name = if (!nameEsc) new String(buf, nameStart, nameLen, ISO_8859_1)  // ASCII/latin1 fast path
                 else decodeEscaped(buf, nameStart, nameLen)
      out += name                                      // map(_.name) → toList
    }
    lineStart = nextLine(buf, p)
  }
  if (source.exhausted) done = true else { /* compact + refill buf, bufTail; loop */ }
}
out.result()
```

Read what is and isn't there: **no `Rec` is allocated** (it's a `Tup` projected to `name`); the 17 unread
fields are **`skipValue`'d** with no decode and no slot; `age` is decoded in-pass and **early-outs the
record** on failure (`skipObjectRest` + `keep=false`); `name` is captured as a **slice** and turned into a
`String` **only inside `if (keep)`** — the sink. The downstream `.filter(...).map(...).toList` is the
*existing* fused machinery; only the *source read* changed from `a(i)` to the record scanner. The whole thing
runs inside the roadmap's chunk loop, in O(one chunk) memory.

For a pure aggregate — `src.parseJson[Rec].filter(_.age > 18).map(_.amount).sum` — there is **zero allocation
per record**: `amount` is a `double` slot, `sum` is a `var acc`, no `String`, no `Rec`, no builder. That is
the headline constant-memory, zero-alloc streaming query.

### 3.4 Where it connects to the existing macro

- **New surface:** `extension (src: JsonSource) def parseJson[T](using JsonScan[T]): Fuse[T]` (or
  `src.lines.parseJson[T]`), where `T`'s fields + the pipeline's projection/predicate set drive codegen. The
  terminal macro already receives the whole `xs.fuse.…` chain; it would additionally receive the parse
  source and `T`'s `Symbol.caseFields` (already used for product decomposition).
- **Reused machinery (no change):** `Shape`/`Tup`/`Sc`, `readShape`/`readAll`, the DCE/sink/CSE passes, the
  `Ctx.consume` threading, the `done` short-circuit flag (a `find`/`head`/`take` over the stream stops
  pulling chunks), `loopOver` (the per-element body becomes the per-record scanner; the outer chunk loop is
  the roadmap's chunk-driver).
- **New machinery:** (1) a `JsonScan[T]` derivation that, from `caseFields` + a projection/predicate set,
  emits the per-record scanner (key dispatch + per-field decode/slice/skip) as the "read" of each column;
  (2) the byte-level decode primitives (number/string/skip) as inline helpers; (3) the NDJSON
  `Source`/chunk-driver with compaction.
- **The element kind constraint generalizes cleanly:** `fuse` already requires Int/Long/Double/<:AnyRef
  columns (specialize-or-fail, anti-boxing). JSON field types map exactly: number → Int/Long/Double slot
  (unboxed), string → `String` (ref), nested object → either a sub-`Tup` (recursive decomposition) or an
  opaque ref. The "no boxing" invariant is preserved because primitive fields live in primitive `var` slots.

---

## 4. Feasibility verdict — brutal

### 4.1 Can it get within ~2–3× of simdjson and beat jsoniter on a projection query?

**Beating jsoniter on a projection query: plausible-to-likely.** On `select 2 of 20 fields, filter selecting
10%`, the fused parser does strictly less work than jsoniter on three axes simultaneously: (1) it
byte-compares keys instead of decoding+hashing all 20; (2) it never allocates the 20-field case class; (3)
it decodes the projected string only for the 10% survivors. jsoniter is near-optimal at the *byte walk* (and
the fused parser must do the same byte walk to find value ends), so the win is the *eliminated decode + alloc*,
not the scan. Expected: **1.3–2.5× jsoniter on the selective projection**, larger as field count grows and
selectivity drops, **shrinking toward parity** as the projection widens (read 18 of 20 → no pushdown win →
jsoniter's maturity may even edge ahead). Honest risk: jsoniter's SWAR string/number paths are extremely
tuned; if our scan isn't equally tuned, the projection win can be eaten by a slower byte walk. The macro must
lift jsoniter's SWAR primitives, not hand-roll naive byte loops.

**Within 2–3× of simdjson C++ (2.0–2.5 GB/s structural): no, and yes, depending on metric.**
- On *bytes-scanned-per-second*, a scalar parser is **~2–5× behind** simdjson's SIMD stage-1. We will not
  close that with scalar code.
- On *records-answered-per-second for a selective query*, the comparison **inverts in our favor vs
  simdjson-java**: simdjson-java's stage-1 indexes the *whole* document unconditionally (its own schema-vs-DOM
  numbers show only ~1.6× from laziness), while we skip unwanted fields' bytes and never index. On a record
  where we read 2 of 20 fields and reject 90%, we touch far fewer bytes than simdjson-java touches building
  its full index. So **"within 2–3× of simdjson-java on a selective projection" is achievable, and on ARM we
  win outright** (simdjson-java has no NEON path).
- Against C++ simdjson On-Demand specifically: it still pays full stage-1, so on a *very* selective query our
  byte-skipping can be competitive on bytes-touched; but C++'s constant factors (SIMD, no JVM) keep it ahead
  in absolute MB/s. Realistic target: **2–4× behind C++ simdjson on selective projection, not within 2×.**

### 4.2 Full-object parse vs jsoniter

When the pipeline reads *all* fields and emits the whole case class, there is **no pushdown win** — it's a
straight parser-vs-parser fight, and **jsoniter wins or ties.** jsoniter is a decade-tuned codebase with
SWAR everything, a tiered double parser, pooled buffers, and exhaustive fuzzing. A new macro parser should
expect to be **roughly parity to ~1.5× behind** on full-object parse. *We should not market full-object
parsing; we should explicitly cede it to jsoniter* and even consider delegating the full-parse path to
jsoniter under the hood (use it as the `JsonScan[T]` when the pipeline reads the whole record).

### 4.3 Where projection-pushdown materially wins / is marginal / loses

- **Materially wins:** NDJSON analytics — fat records (20–100 fields), read 1–4, filter/aggregate, low
  selectivity. The win is multiplicative in (fields skipped) × (records rejected) × (allocations avoided).
  Constant-memory streaming over a 100 GB file folds in O(chunk). This is the sweet spot and the whole pitch.
- **Marginal:** moderate field counts, moderate selectivity, or projections that read most fields. The
  byte-walk still dominates and we're back to parser-vs-parser.
- **Loses:** full-object parse (jsoniter); deeply-nested / dynamically-shaped JSON where the projected field
  is buried under variable structure (skip cost grows, dispatch codegen complexity explodes); documents
  needing strict whole-document UTF-8/structural validation (we deliberately skip validating unread fields);
  single small documents where there's no stream to amortize the chunk machinery.

### 4.4 Real risks

1. **Double-parse correctness (highest).** The decoded `double`/`long`/`String` must be bit-identical to
   jsoniter/`Double.parseDouble` or queries silently disagree. Mitigation: *reuse* jsoniter's tiered double
   logic / FastDoubleParser; fuzz every primitive against `Double.parseDouble` and jsoniter over millions of
   random and adversarial inputs (subnormals, 17-digit ties, `1e308`, leading zeros).
2. **UTF-8 / escape / surrogate edge cases.** Lift jsoniter's `parseEncodedString` and its `skipString`
   string-aware brace counting verbatim; fuzz against jsoniter. The "don't validate unread fields" relaxation
   must be a documented, opt-out behavior.
3. **Vector API immaturity** — only a risk if we pursue SIMD; the recommendation (§2.3) is *don't*, which
   neutralizes it. If a SIMD stage-1 is ever added it must be x86-only, flagged, optional.
4. **Macro complexity of skip/dispatch codegen.** Generating correct key-dispatch (length/first-byte/memcmp
   or perfect-hash), per-type skip, and the early-out logic — over arbitrary case-class field sets, nested
   products, optional fields, defaults, unknown-field handling — is substantially more code than the current
   arithmetic-column lowering. Nested objects and arrays multiply the cases. **Mitigation: ship flat records
   only in v1** (numbers + strings, no nested objects/arrays), prove the thesis, then expand. The existing
   fuzz/snapshot discipline (the fusion macro's golden snapshots + List-reference fuzz) extends directly:
   snapshot the generated scanner, fuzz parsed-and-projected results against jsoniter's full parse + a
   reference projection.
5. **Chunk-boundary correctness.** Partial-line compaction + slices-into-recycled-buffers is a lifetime
   hazard (§2.2/§2.6). Mitigation: keep records intra-chunk; decode survivor strings before releasing the
   chunk; fuzz with adversarial chunk splits (split mid-string, mid-escape, mid-number).
6. **The byte-walk floor.** Projection saves decode+alloc but *not* the linear scan to find value ends. If
   the win is "just" the eliminated decode and that's a small fraction of total time on the target data, the
   thesis is marginal. **This is what the §5.6 experiment must measure first.**

---

## 5. Benchmark plan

Reuse the existing JMH harness (`benchmarks/`, run via `bleep run benchmarks-runner`, `GenJmh` sourcegen).
Heed the box caveats from the benchmark-suite memory: this is a macOS dev box — strip
`-XX:+UseNUMA`/`-server` from `CommonParams`, `@Fork(3)`, **always `-prof gc`** (allocations are priority #1),
compiler blackholes for small-N, inputs as non-final `var` set in `@Setup`.

### 5.1 Datasets

- **D1 — fat NDJSON, selective:** 1M lines, each a 20-field record (mix: 8 ints/longs, 6 doubles, 6 strings),
  field-name lengths varied so length-dispatch is exercised. A predicate field (`age`/`amount`) chosen so a
  threshold selects **~10%**. ~200–500 MB on disk → forces real streaming. **This is the primary dataset.**
- **D2 — fat NDJSON, projection only (no filter):** same records, project 2 fields, no predicate — isolates
  the *projection* (skip+no-alloc) win from the *predicate early-out* win.
- **D3 — full-object NDJSON:** same records, parse all 20 into the case class — the "we lose to jsoniter"
  control, to quantify the full-parse gap honestly.
- **D4 — chunk-split adversary:** D1 fed through a `Source` with tiny (e.g. 4 KB) chunks and deliberately
  awkward splits (mid-string, mid-escape, mid-number) — correctness + the compaction cost.
- Real-world sanity: a public NDJSON corpus (e.g. GitHub events / a Twitter-API-shaped stream) for an
  honest non-synthetic data point.

### 5.2 Baselines (per query)

1. **jsoniter-scala full parse** then `.filter(...).map(...)` in Scala — the dominant JVM baseline (it must
   build the full case class).
2. **jsoniter-scala with a narrow case class** (only the read fields) — jsoniter's *own* best projection
   (it'll `skip()` the rest but still decode all keys + byte-walk skipped values). The fairest "jsoniter
   doing projection" baseline.
3. **Jackson streaming hand-loop** — `nextFieldName(SerializableString)` + `skipChildren()` + typed getters —
   the closest existing "manual projection" and the honest yardstick (§1.3).
4. **simdjson-java schema parser** — *only if Panama is available and x86* (note its 34 MB buffers and that on
   this macOS/ARM box it will likely throw or be ~40× slower — that result is itself a headline finding for
   the portability claim).
5. **The fused parser** (the contender).

### 5.3 Metrics

- **Throughput:** records/s and derived **MB/s** (bytes consumed / time). Report both — records/s rewards
  selectivity, MB/s is the honest cross-parser number.
- **Allocations:** `gc.alloc.rate.norm` (bytes/op) via `-prof gc`. The fused reduce/project pipeline should
  show **near-zero bytes/record**; jsoniter full-parse shows the case class + strings; this is expected to be
  the most lopsided, most defensible chart.
- **Peak working set:** RSS over a long stream (D1 at 1M+ lines) — must be flat (O(chunk)) for the fused +
  streaming path; flat for jsoniter-streaming too; the point is to *prove* constant memory, not just claim it.
- **Bytes-touched ratio** (instrumentation, not JMH): fraction of input bytes the parser actually reads vs
  the full document — quantifies the projection-pushdown advantage directly.

### 5.4 The specific query that should show the win

```scala
// D1: 20-field records, predicate selects ~10%, project 1 field
jsonSource.parseJson[Rec].filter(_.amount > threshold).map(_.category).toList
// and the zero-alloc variant:
jsonSource.parseJson[Rec].filter(_.amount > threshold).map(_.amount).sum
```

Expected shape of results: fused **beats jsoniter-narrow on throughput by 1.3–2.5×** and on allocation by
**~10×+** (near-zero vs case-class+string per record); fused **beats Jackson-streaming** modestly (no
per-field virtual dispatch, no decode of unwanted keys); fused **loses to simdjson-java on x86 full-throughput
but wins on selectivity/ARM**; fused **loses to jsoniter on D3 full-parse** (the honest control).

### 5.5 Threats to validity

JMH constant-folding (inputs must be `var`); the 10%-selectivity must come from data, not a constant
predicate (else the JIT hoists it); string decode must actually be forced for survivors (a `toList` of
strings, not a `count`, or the decode is DCE'd and the bench measures nothing). Warm the parser; report
`-prof gc` always; run D1 long enough that the streaming working-set is observable.

### 5.6 The single experiment that proves or kills the thesis

**Build the minimal vertical slice and run D1's selective-projection query (§5.4) against jsoniter-narrow,
with `-prof gc`.** Minimal slice = (a) the NDJSON `Source` + chunk-driver with partial-line compaction; (b) a
`JsonScan[T]` for a *flat* 20-field record that emits the single-pass scanner (length/first-byte/memcmp key
dispatch, lift jsoniter's SWAR int/double/string + `skip*`), exposing fields as lazy columns; (c) wire it to
the existing `Fuse` so `.filter().map().toList`/`.sum` fuses. Success criteria, all required:
1. **Throughput ≥ jsoniter-narrow** on D1's selective query (≥1.3× is the target; parity is a yellow flag).
2. **Allocations: near-zero bytes/record** for the `.sum` variant (the constant-memory claim).
3. **Correctness:** parsed/projected results bit-identical to jsoniter over the fuzz corpus (incl. D4 splits).

If (1) comes out *below* jsoniter-narrow, the byte-walk floor (§4.4.6) dominates the eliminated-decode win on
realistic data and the thesis is **dead** as a speed play (it might still live as a *memory* play if (2)
holds dramatically). If (1) and (2) both hold, the thesis is **proven** and the rest is engineering
(nested records, more types, the SIMD question deferred).

---

## 6. Recommendation

**GO — scoped tightly.** Build the §5.6 vertical slice and run the one experiment. The thesis is
architecturally sound (the column/DCE/sink model maps onto byte-sourced fields with almost no new optimizer,
and the streaming roadmap's `Source`/chunk-driver is the exact substrate), the SOTA leaves a real gap on
*projection* queries (jsoniter decodes every key + allocates the full object; simdjson-java indexes the whole
doc and dies on ARM), and the win is multiplicative on the target workload. The risk is concentrated and
testable: the §5.6 benchmark settles it in one shot.

**Minimal first deliverable:** NDJSON `Source` + chunk-driver (partial-line compaction) + a macro-generated
single-pass projection scanner for a **flat** record (numbers + strings, no nesting), lifting jsoniter's SWAR
int/double/string and `skip*` primitives, wired into the existing `Fuse` surface — plus the one D1
selective-projection benchmark with `-prof gc`. Defer: nested objects/arrays, the Vector API entirely,
full-object parsing (cede to / delegate to jsoniter).

**The genuinely-novel, defensible claim if it works:**

> The **fastest pure-JVM, zero-dependency way to run a selective projection/filter/aggregate query over an
> NDJSON byte stream in constant memory.** It is the only JVM parser that combines all of: (1) skipping
> unwanted fields' bytes (no decode, no alloc) — like simdjson On-Demand but with *no SIMD index built over
> the whole document*; (2) matching keys by raw byte comparison with **zero key decode** — unlike jsoniter,
> which decodes and hashes every key; (3) **zero per-record object allocation** for reduce/project pipelines
> — the record is a `Tup` of columns, materialized only if used whole; (4) **lazy, compute-for-survivors
> string decode** driven by the *same* optimizer that drives in-memory `fuse`; (5) **O(chunk) working set**
> over unbounded input; (6) **full platform portability** (scalar — wins outright on ARM, where simdjson-java
> loses ~40× to Jackson). No existing JVM parser is all-green on those six axes simultaneously. We
> explicitly **do not** claim to beat jsoniter on full-object parsing or simdjson on raw structural
> throughput — the claim is precisely the *selective-query-over-a-stream* niche, where the existing fusion
> optimizer turns "parse less" into a first-class, compile-time-driven property.

---

## Appendix: source map (cloned repos)

- jsoniter-scala: `~/pr/jsonresearch/jsoniter-scala` — reader
  `jsoniter-scala-core/jvm-native/.../JsonReader.scala`; macro
  `jsoniter-scala-macros/shared/src/main/scala-3/.../JsonCodecMaker.scala`; SWAR
  `jsoniter-scala-core/jvm/.../ByteArrayAccess.java`.
- simdjson-java: `~/pr/jsonresearch/simdjson-java` — `StructuralIndexer.java` (Vector API stage-1,
  `index256`/`index512`, `prefixXor`), `SchemaBasedJsonIterator.java` + `OnDemandJsonIterator.java` (lazy
  projection, `skipChild`), `VectorUtils.java` (AVX2/512-only species gate).
- dsl-json: `~/pr/jsonresearch/dsl-json` — annotation-processor codegen reference.
- Verified external numbers: simdjson-java README (twitter.json ops/s, AVX2/512); FastDoubleParser README
  (byte[] ≈694 MB/s, 7.9× over `Double.parseDouble`); C++ simdjson (2.0–2.5 GB/s stage-1); Eisel-Lemire
  (~9× strtod, half-way fallback condition).
</content>
</invoke>
