# FSet — "build an algebra and query it a lot"

The signature FSet workload: take a 3-way union `a ∪ b ∪ c` and probe `contains` **n = size**
times. Measured two ways — **lazy** (never materialize; `contains` distributes over the lazy
tree) and **materialized** (`.materialize` once, then probe the resulting leaf) — for Int and
String, against the best competitor in each row.

> Numbers are JMH throughput (ops/s), 0-fork / 2-warmup / 3-measure on a macOS dev box. They are
> **noisy** (wide error bars); the *gaps* are wide enough to be directional, but use `-f2` with
> fewer shards before quoting absolute figures. Inputs: three overlapping windows of `size`
> elements each (so the union has ~2·size distinct), `contains` probed `size` times.

## Results

| workload | @1000 | @100000 | vs best competitor |
|---|---:|---:|---|
| **Int — lazy** | 1.83M | 18,344 | ~0.95× BitSet · **18–75× fastutil** |
| **Int — materialized** | 1.66M | 18,704 | ~0.95× BitSet |
| **String — lazy** (no materialize) | **110,660** | **515** | **fastest — 2–8.6× over all** |
| **String — materialized** | — | 274 | fastest — 2.1× |

Per-row competitors at @100000:
- Int: immutable BitSet 18,766 · java BitSet 20,820 · fastutil 240.
- String lazy: fastutil 127 · scala.mutable 124 · Guava 90 · scala.immutable (CHAMP) 60.
- String materialized: fastutil 128 · scala.mutable 123 · HPPC 76 · CHAMP 70 · Guava 64.

## The two stories are opposite

**Int → BitSet parity, lazy ≈ materialized.** The merge is *cheap* (word-parallel OR), so the
smart `++` constructor eager-merges the chain into a single `IntDense` bitmap — there is no
lazy-vs-eager distinction. Both land at ~0.9–0.98× of the bitsets and **18–75× over fastutil**
(whose hash-set merge + probes can't keep up). Int has no "lazy advantage" because there is
nothing expensive to defer.

**String → the lazy moat is enormous (2–8.6×), and it grows with scale.** Reference merging is
*expensive* (rehash every string), so staying lazy — skip the merge entirely, distribute
`contains` over the F14 leaves — beats every competitor by 2× at 1000 and **8.6× over CHAMP at
100k**. Each competitor must pay the O(n) string-rehash to build the union; FSet does not.

## The non-obvious kicker

For strings at 100k, **lazy (515) beats FSet's *own* materialized path (274)** by ~1.9×. Because
ref merging is so costly, deferring it wins even at `n = size` queries — you would only
materialize a reference union if you intend to query it *far* more than n times. For Int it is
the reverse: the merge is so cheap you simply eager-merge always.

## Takeaway

The headline number — **merge-but-don't-materialize, then query many** — is where FSet is the
clear champion on references (2–8.6×) and ties the bitsets on Int. It is the moat no other JVM
set has: a lazy set algebra whose `contains` distributes, so you pay for the queries you make,
not for materializing a result you never needed.
