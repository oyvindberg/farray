package farray

import org.junit.Test
import org.junit.Assert.*

/** `groupReduceBy` / `groupReduce` / `groupCount` / `groupSum`: a primitive-keyed group-reduce in ONE fused pass — Int keys stay unboxed in the hot loop (via
  * IntKeyMap), boxing only at the O(#keys) materialization.
  */
class GroupReduceTest:

  private val xs = FArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  private val ref = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  @Test def groupReduceBy_int_matches_groupMapReduce(): Unit =
    val got = xs.fuse.groupReduceBy(_ % 3)(x => x)(_ + _)
    assertEquals(ref.groupMapReduce(_ % 3)(x => x)(_ + _), got)

  @Test def groupReduceBy_mapsValue(): Unit =
    val got = xs.fuse.groupReduceBy(_ % 2)(x => x * 10)(_ + _)
    assertEquals(ref.groupMapReduce(_ % 2)(_ * 10)(_ + _), got)

  @Test def groupReduce_identityValue(): Unit =
    val got = xs.fuse.groupReduce(_ % 2)(_ max _) // max element per parity class
    assertEquals(ref.groupMapReduce(_ % 2)(identity)(_ max _), got)

  @Test def groupCount(): Unit =
    val got = xs.fuse.groupCount(_ % 3)
    assertEquals(ref.groupBy(_ % 3).view.mapValues(_.size).toMap, got)

  @Test def groupSum_int(): Unit =
    val got = xs.fuse.groupSum(_ % 3)(x => x)
    assertEquals(ref.groupMapReduce(_ % 3)(identity)(_ + _), got)

  @Test def groupSum_double(): Unit =
    val got = xs.fuse.groupSum(_ % 2)(_.toDouble)
    val r = ref.groupMapReduce(_ % 2)(_.toDouble)(_ + _)
    assertEquals(r.keySet, got.keySet)
    r.foreach { case (k, v) => assertEquals(v, got(k), 1e-9) }

  @Test def groupSum_long(): Unit =
    val got = xs.fuse.groupSum(_ % 3)(_.toLong * 1000000000L) // exercise the long value slot
    assertEquals(ref.groupMapReduce(_ % 3)(_.toLong * 1000000000L)(_ + _), got)

  /** Long key → boxed-HashMap fallback path (still correct). */
  @Test def groupReduceBy_longKey(): Unit =
    val got = xs.fuse.groupReduceBy(x => (x % 4).toLong)(x => x)(_ + _)
    assertEquals(ref.groupMapReduce(x => (x % 4).toLong)(identity)(_ + _), got)

  /** reference (String) key + reference (String) value. */
  @Test def groupReduceBy_refKey_refValue(): Unit =
    val words = FArray("apple", "banana", "avocado", "cherry", "blueberry")
    val got = words.fuse.groupReduceBy(_.head)(w => w)((a, b) => if a < b then a else b) // min word per first letter
    val r = List("apple", "banana", "avocado", "cherry", "blueberry").groupMapReduce(_.head)(identity)((a, b) => if a < b then a else b)
    assertEquals(r, got)

  /** Int key + reference value. */
  @Test def groupReduceBy_intKey_refValue(): Unit =
    val got = xs.fuse.groupReduceBy(_ % 2)(x => x.toString)((a, b) => a + "," + b)
    val r = ref.groupMapReduce(_ % 2)(_.toString)((a, b) => a + "," + b)
    assertEquals(r, got)

  @Test def empty(): Unit =
    assertEquals(Map.empty[Int, Int], FArray.empty[Int].fuse.groupReduceBy(_ % 3)(x => x)(_ + _))

  @Test def withFilter(): Unit =
    val got = xs.fuse.filter(_ > 3).groupSum(_ % 2)(x => x)
    assertEquals(ref.filter(_ > 3).groupMapReduce(_ % 2)(identity)(_ + _), got)

  /** growth: enough distinct keys to force several rehashes of IntKeyMap. */
  @Test def manyKeys_growth(): Unit =
    val big = FArray.tabulate(10000)(i => i)
    val got = big.fuse.groupSum(_ % 1000)(x => x) // 1000 distinct keys → grows past initial cap
    val r = (0 until 10000).toList.groupMapReduce(_ % 1000)(identity)(_ + _)
    assertEquals(r, got)

  /** the unboxed-key headline: an Int-key Int-value group-sum over a large input must not box per element. 50 passes over 100k with a few keys would allocate
    * millions of Integers if boxing; IntKeyMap → ~0.
    */
  @Test def intKeyIntValue_doesNotBox(): Unit =
    val big = FArray.tabulate(100000)(i => i)
    val rt = Runtime.getRuntime
    System.gc(); Thread.sleep(20)
    val before = rt.totalMemory - rt.freeMemory
    var sink = 0; var it = 0
    while it < 50 do { val m = big.fuse.groupSum(_ % 8)(x => x); sink += m.size; it += 1 }
    val after = rt.totalMemory - rt.freeMemory
    assertTrue(s"sink=$sink", sink == 50 * 8)
    // boxed groupBy here would allocate ~5M Integers (>100 MB). Unboxed loop + tiny maps ≈ a few MB. Slack ample.
    assertTrue(s"alloc grew ${(after - before) / 1000000} MB — Int key/value likely boxing per element", (after - before) < 80_000_000L)

end GroupReduceTest
