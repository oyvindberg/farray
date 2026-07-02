package farray

/** Hash mixers shared by the generated cores — the single source of truth.
  *
  * `FArrayOps` (distinct's scratch tables) and `FSetOps` (the frozen hash leaves; the fset project depends on farray) both call these statics instead of
  * emitting private copies. Bodies are the exact functions FSet's scorecard was measured with — do not tweak one consumer without re-running the other's
  * benchmarks.
  */
private[farray] object Mixers {

  /** murmur3 fmix32 — full avalanche, ~9 cycles. The default integer scramble. */
  def mixInt(h0: Int): Int = { var h = h0; h ^= h >>> 16; h *= 0x85ebca6b; h ^= h >>> 13; h *= 0xc2b2ae35; h ^= h >>> 16; h }

  /** murmur3 fmix64 folded to 32 bits — the Long-keyed table scramble. */
  def mixLong(h0: Long): Int = {
    var h = h0; h ^= h >>> 33; h *= 0xff51afd7ed558ccdL.toLong; h ^= h >>> 33; h *= 0xc4ceb9fe1a85ec53L.toLong; h ^= h >>> 33; (h ^ (h >>> 32)).toInt
  }

  /** Fibonacci multiply + high-bit fold (~3 cycles): enough on top of a cached/object hashCode, where the murmur avalanche is redundant. Tuned for ~16-bit
    * masks — degrades outside [2^10, 2^15] caps (measured); FSetOps applies it only in that band.
    */
  def mixRef(h0: Int): Int = { val x = h0 * 0x9e3779b1; x ^ (x >>> 16) }
}
