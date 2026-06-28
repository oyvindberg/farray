package farray

import org.junit.Assert.*
import org.junit.Test

/** Parity of the FSet M1 green slice vs `scala.collection.immutable.Set`. Covers all four element kinds
  * (Int / Long / Double prim + String Ref), proving the SBase core + the GenSets codegen + the kind
  * specialization end-to-end (no boxing of primitive elements — see the javap check in the report).
  *
  * Ops under test: empty / apply / fromArray / fromFArray / from, contains / apply, size / isEmpty / nonEmpty,
  * incl / excl (add / remove). (The lazy algebra ++/&/diff/xor, the traversal/materialize ops, and the
  * Hash/Dense leaves are NEXT-STEPS, not in this slice; both the `incl`/`excl` named ops and the `+`/`-` infix aliases are exercised.)
  */
class FSetTest:

  // ---- helpers: drive an FSet and the reference Set through the SAME op sequence, assert agreement. ----

  /** assert membership agreement on a probe domain + the structural size/isEmpty agreement. */
  private def checkInt(fs0: FSet[Int], ref: Set[Int], probes: Iterable[Int]): Unit =
    val fs = fs0.materialize // enumeration ops are FSetMaterialized-only; contains stays on the (FSet) original
    assertEquals("size", ref.size, fs.size)
    assertEquals("isEmpty", ref.isEmpty, fs.isEmpty)
    assertEquals("nonEmpty", ref.nonEmpty, fs.nonEmpty)
    probes.foreach(p => assertEquals(s"contains($p)", ref.contains(p), fs0.contains(p)))
    probes.foreach(p => assertEquals(s"apply($p)", ref(p), fs0(p)))

  @Test def empty_int(): Unit =
    checkInt(FSet.empty[Int], Set.empty[Int], -3 to 3)

  @Test def apply_small_arities_int(): Unit =
    checkInt(FSet(7), Set(7), 0 to 10)
    checkInt(FSet(7, 7), Set(7, 7), 0 to 10) // dup collapses
    checkInt(FSet(3, 1, 2), Set(3, 1, 2), 0 to 10)
    checkInt(FSet(3, 1, 2, 2, 3, 4), Set(3, 1, 2, 2, 3, 4), 0 to 10) // varargs path + dups

  @Test def from_array_int(): Unit =
    val data = Array(5, 3, 8, 3, 1, 8, 5, 9, 0)
    checkInt(FSet.fromArray(data), data.toSet, -1 to 12)
    // confirm the source array was NOT mutated by the in-place sort/dedup build
    assertArrayEquals(Array(5, 3, 8, 3, 1, 8, 5, 9, 0), data)

  @Test def from_iterable_int(): Unit =
    val xs = List(4, 4, 2, 7, 2, 0, -1, 7)
    checkInt(FSet.from(xs), xs.toSet, -3 to 10)
    checkInt(FSet.fromIterable(xs), xs.toSet, -3 to 10)

  @Test def from_farray_int(): Unit =
    val fa = FArray(9, 1, 4, 1, 9, 2)
    checkInt(FSet.fromFArray(fa), Set(9, 1, 4, 2), 0 to 12)

  @Test def incl_excl_int(): Unit =
    var fs: FSet[Int] = FSet.empty[Int]
    var ref = Set.empty[Int]
    val script = List(1, 5, 5, 3, 9, 1, 7, 2)
    for x <- script do
      fs = fs + x        // infix operator
      ref = ref + x
      checkInt(fs, ref, -2 to 12)
    // now remove some
    for x <- List(5, 100, 3, 1, 1) do // 100 absent (no-op alias), 1 removed twice
      fs = fs - x        // infix operator
      ref = ref - x
      checkInt(fs, ref, -2 to 12)

  @Test def incl_absent_returns_alias_int(): Unit =
    val fs = FSet(1, 2, 3)
    // + of a present element is a no-op alias (sound by immutability); membership unchanged.
    assertTrue((fs + 2).contains(2))
    assertFalse((fs - 9).contains(9)) // - of absent: alias, still absent
    checkInt(fs + 2, Set(1, 2, 3), 0 to 5)
    checkInt(fs - 9, Set(1, 2, 3), 0 to 5)

  // ---- Long / Double primitive kinds (no boxing — the same prim-array path) ----

  @Test def long_kind(): Unit =
    val xs = Array(10L, 5L, 10L, 7L, 5L, 3L)
    val fs = FSet.fromArray(xs)
    val ref = xs.toSet
    assertEquals(ref.size, fs.size)
    for p <- 0L to 12L do assertEquals(s"contains($p)", ref.contains(p), fs.contains(p))
    val fs2 = fs + 99L - 5L
    val ref2 = ref + 99L - 5L
    assertEquals(ref2.size, fs2.materialize.size)
    for p <- List(99L, 5L, 10L, 7L) do assertEquals(s"contains($p)", ref2.contains(p), fs2.contains(p))

  @Test def double_kind(): Unit =
    val xs = Array(1.5, 2.5, 1.5, 3.5, 2.5)
    val fs = FSet.fromArray(xs)
    val ref = xs.toSet
    assertEquals(ref.size, fs.size)
    for p <- List(1.5, 2.5, 3.5, 4.5, 0.0) do assertEquals(s"contains($p)", ref.contains(p), fs.contains(p))

  @Test def double_nan_and_zero(): Unit =
    // NaN and ±0.0 are pathological for Scala's own collections (immutable.Set COUNTS NaN yet
    // contains(NaN)==false; List keeps duplicate NaNs). FSet chooses SANE, consistent semantics via
    // java.lang.Double.compare: NaN dedups to ONE findable element; -0.0 and +0.0 are DISTINCT.
    val fs = FSet.fromArray(Array(0.0, -0.0, Double.NaN, 1.5, Double.NaN, -0.0, 1.5))
    assertEquals("distinct {-0.0, +0.0, 1.5, NaN}", 4, fs.size)
    assertTrue("NaN is a findable member", fs.contains(Double.NaN))
    assertTrue("+0.0 present", fs.contains(0.0))
    assertTrue("-0.0 present", fs.contains(-0.0))
    assertTrue(fs.contains(1.5))
    assertFalse(fs.contains(2.5))

  @Test def algebra_lazy_int(): Unit =
    // ++ / & / &~ / ^ are O(1) lazy nodes; contains distributes, size walks (until the merge core).
    val a = FSet(1, 2, 3, 4); val b = FSet(3, 4, 5, 6)
    val ra = Set(1, 2, 3, 4); val rb = Set(3, 4, 5, 6)
    def chk(fs: FSet[Int], ref: Set[Int]): Unit =
      assertEquals("size", ref.size, fs.materialize.size)
      for p <- -1 to 8 do assertEquals(s"contains($p)", ref.contains(p), fs.contains(p))
    chk(a ++ b, ra union rb)
    chk(a & b, ra intersect rb)
    chk(a &~ b, ra diff rb)
    chk(a ^ b, (ra union rb) diff (ra intersect rb))
    // nested algebra still distributes
    chk((a ++ b) & (a ^ b), (ra union rb) intersect ((ra union rb) diff (ra intersect rb)))

  @Test def merge_materialize_int(): Unit =
    // the §3.2 unboxed merge: materialize folds the lazy tree to ONE sorted leaf; iteration is ordered+distinct.
    val a = FSet(5, 1, 3, 1); val b = FSet(3, 4, 2)
    val ra = Set(5, 1, 3); val rb = Set(3, 4, 2)
    assertEquals("union",     (ra union rb).toList.sorted,                              (a ++ b).materialize.toList)
    assertEquals("intersect", (ra intersect rb).toList.sorted,                          (a & b).materialize.toList)
    assertEquals("diff",      (ra diff rb).toList.sorted,                               (a &~ b).materialize.toList)
    assertEquals("xor",       ((ra union rb) diff (ra intersect rb)).toList.sorted,     (a ^ b).materialize.toList)
    // size now flows through the unboxed merge (not the boxed walk) for a lazy node
    assertEquals((ra union rb).size, (a ++ b).materialize.size)
    // nested + memoized: materialize twice is stable
    val u = (a ++ b).materialize
    assertEquals(u.toList, u.toList)
    assertEquals((ra union rb).toList.sorted, u.toList)

  @Test def merge_materialize_str(): Unit =
    // the Ref merge core (cached-hash sort-merge). Ref iterates in hash order, so compare as SETS.
    val a = FSet("apple", "banana", "cherry", "fig"); val b = FSet("cherry", "date", "fig", "kiwi")
    val ra = Set("apple", "banana", "cherry", "fig"); val rb = Set("cherry", "date", "fig", "kiwi")
    assertEquals("union",     ra union rb,                              (a ++ b).materialize.toList.toSet)
    assertEquals("intersect", ra intersect rb,                         (a & b).materialize.toList.toSet)
    assertEquals("diff",      ra diff rb,                               (a &~ b).materialize.toList.toSet)
    assertEquals("xor",       (ra union rb) diff (ra intersect rb),     (a ^ b).materialize.toList.toSet)
    // HASH-COLLISION tie-group: "Aa" and "BB" share hashCode 2112 — drives the .equals-within-tie-group path.
    assertEquals(2112, "Aa".hashCode); assertEquals(2112, "BB".hashCode)
    val c = FSet("Aa", "BB"); val d = FSet("BB", "Cc")
    assertEquals("collide ∪", Set("Aa", "BB", "Cc"), (c ++ d).materialize.toList.toSet)
    assertEquals("collide ∩", Set("BB"),             (c & d).materialize.toList.toSet)
    assertEquals("collide ∖", Set("Aa"),             (c &~ d).materialize.toList.toSet)
    assertEquals("collide ^", Set("Aa", "Cc"),       (c ^ d).materialize.toList.toSet)
    // large (Hash-leaf) Ref merge, and chained algebra
    val big1 = FSet.fromArray((0 until 100).map("k" + _).toArray)
    val big2 = FSet.fromArray((50 until 150).map("k" + _).toArray)
    assertEquals((0 until 150).map("k" + _).toSet, (big1 ++ big2).materialize.toList.toSet)
    assertEquals((50 until 100).map("k" + _).toSet, (big1 & big2).materialize.toList.toSet)
    assertEquals((0 until 50).map("k" + _).toSet,  (big1 &~ big2).materialize.toList.toSet)
    // materialized === / setHashCode over a Ref merge result, shape-independent (merged union vs fresh build)
    val merged = (big1 ++ big2).materialize
    val direct = FSet.fromArray((0 until 150).map("k" + _).toArray)
    assertTrue(merged === direct)
    assertEquals(merged.setHashCode, direct.setHashCode)

  @Test def dense_bitmap_int(): Unit =
    // dense + large → bitmap leaf: O(1) bit-test contains, incl. NEGATIVE spans (java.util.BitSet can't).
    val a = FSet.fromArray((-50 to 50).toArray) // 101 dense, span 101 → bitmap
    assertEquals(101, a.size)
    assertTrue(a.contains(-50)); assertTrue(a.contains(0)); assertTrue(a.contains(50))
    assertFalse(a.contains(-51)); assertFalse(a.contains(51))
    assertEquals((-50 to 50).toList, a.toList) // bitmap iterates ascending
    // bitmap algebra (via extraction) is correct
    val b = FSet.fromArray((25 to 75).toArray)
    assertEquals((-50 to 75).toSet, (a ++ b).materialize.toList.toSet)
    assertEquals((25 to 50).toSet, (a & b).materialize.toList.toSet)
    assertEquals((-50 to 24).toSet, (a &~ b).materialize.toList.toSet)
    // sparse-wide stays NON-bitmap (router fallback) but still correct
    val sparse = FSet.fromArray(Array(0, 1000000, 2000000))
    assertTrue(sparse.contains(1000000)); assertFalse(sparse.contains(5)); assertEquals(3, sparse.size)

  @Test def deep_union_spine_materialize(): Unit =
    // a deep ++/incl-built left-spine would StackOverflow + be O(n²) in the recursive materialize — Step 5
    // collapses the spine iteratively + balanced-merges. n=20000 overflows the old recursive path.
    val n = 20000
    var s: FSet[Int] = FSet.empty[Int]
    var i = 0
    while (i < n) { s = s ++ FSet(i); i += 1 }
    assertEquals(n, s.materialize.size) // no StackOverflow
    assertTrue(s.contains(0)); assertTrue(s.contains(n - 1)); assertFalse(s.contains(n))
    // Ref spine too (hash-merge path)
    var rs: FSet[String] = FSet.empty[String]
    i = 0
    while (i < n) { rs = rs ++ FSet("k" + i); i += 1 }
    assertEquals(n, rs.materialize.size)
    assertTrue(rs.contains("k0")); assertFalse(rs.contains("nope"))

  @Test def merge_materialize_long(): Unit =
    val a = FSet(10L, 5L, 7L); val b = FSet(7L, 1L)
    assertEquals(List(1L, 5L, 7L, 10L), (a ++ b).materialize.toList)
    assertEquals(List(7L), (a & b).materialize.toList)

  @Test def value_equals_hash_int(): Unit =
    // shape- and order-independent value equality + commutative hash, on MATERIALIZED sets.
    val a = FSet(3, 1, 2); val b = FSet(2, 3, 1)
    assertTrue(a === b)
    assertEquals(a.setHashCode, b.setHashCode)
    assertFalse(a === FSet(1, 2))
    // an algebra result must be materialized before comparing
    val viaAlgebra = (FSet(1, 2) ++ FSet(2, 3)).materialize
    assertTrue(a === viaAlgebra)
    assertEquals(a.setHashCode, viaAlgebra.setHashCode)
    // a Hash-built leaf equals the same set assembled via union of two halves (different shapes)
    val big = FSet.fromArray((0 until 50).toArray)
    val bigAlgebra = (FSet.fromArray((0 until 25).toArray) ++ FSet.fromArray((25 until 50).toArray)).materialize
    assertTrue(big === bigAlgebra)
    assertEquals(big.setHashCode, bigAlgebra.setHashCode)

  // (the old `value_equals_throws_on_lazy` runtime test is gone — under Option C, `lazyFSet.setHashCode` /
  //  `=== lazyFSet` simply DON'T COMPILE: equals/hashCode live on FSetMaterialized, not on a lazy FSet.)

  @Test def value_equals_ref(): Unit =
    val a = FSet.from(List("b", "a", "c")); val b = FSet.from(List("c", "b", "a"))
    assertTrue(a === b)
    assertEquals(a.setHashCode, b.setHashCode)
    assertFalse(a === FSet.from(List("a", "b")))

  @Test def traversal_int(): Unit =
    val fs = FSet(3, 1, 4, 1, 5, 9, 2, 6)
    val ref = Set(3, 1, 4, 5, 9, 2, 6)
    var sum = 0
    fs.foreach(x => sum += x)
    assertEquals(ref.sum, sum)
    assertTrue(fs.forall(_ > 0))
    assertFalse(fs.forall(_ > 2))
    assertTrue(fs.exists(_ == 9))
    assertFalse(fs.exists(_ == 100))
    // over a materialized algebra result, still each element once
    val u = (FSet(1, 2, 3) ++ FSet(3, 4, 5)).materialize
    var c = 0; u.foreach(_ => c += 1)
    assertEquals(5, c)
    assertTrue(u.forall(_ < 6))

  @Test def filter_count_int(): Unit =
    val fs = FSet(1, 2, 3, 4, 5, 6)
    assertEquals(List(2, 4, 6), fs.filter(_ % 2 == 0).toList) // result stays sorted
    assertEquals(List(1, 3, 5), fs.filterNot(_ % 2 == 0).toList)
    assertEquals(3, fs.count(_ % 2 == 0))
    assertTrue(fs.filter(_ > 10).isEmpty)
    // filter a large (hash) set, and over a lazy result
    val big = FSet.fromArray((0 until 100).toArray)
    assertEquals(50, big.filter(_ % 2 == 0).size)
    val f = big.filter(_ >= 50)
    assertTrue(f.contains(75)); assertFalse(f.contains(25))
    assertEquals(40, (FSet(1, 2, 3) ++ big).materialize.count(_ >= 60))

  @Test def range_and_predicate_membership(): Unit =
    // the headline: union two HUGE ranges, query membership — O(1), nothing materialized.
    val u = FSet.range(1, 1000000) ++ FSet.range(2000000, 3000000)
    assertTrue(u.contains(500000))
    assertTrue(u.contains(2500000))
    assertFalse(u.contains(1500000))
    assertFalse(u.contains(0))
    // above / below / universal via the [lo,hi] bounds
    assertTrue(FSet.above(5).contains(6)); assertFalse(FSet.above(5).contains(5))
    assertTrue(FSet.below(5).contains(4)); assertFalse(FSet.below(5).contains(5))
    assertTrue(FSet.universalInt.contains(Int.MinValue))
    assertTrue(FSet.universalInt.contains(0))
    // complement distributes
    val evens = FSet(2, 4, 6, 8)
    val notEvens = evens.complement
    assertTrue(notEvens.contains(3)); assertFalse(notEvens.contains(4))
    // predicate ∩ finite is queryable; finite stays the materializable side
    val evensInRange = FSet(1, 2, 3, 4, 5, 6) & FSet.above(3)
    assertTrue(evensInRange.contains(4)); assertFalse(evensInRange.contains(2))
    // NOTE: `u.size` does NOT COMPILE — an infinite (range) set is an FSetInfinite/FSet with no enumeration
    // ops. The "can't enumerate an infinite set" guarantee is now a TYPE error, not a runtime throw.

  @Test def map_nxn(): Unit =
    // read kind × write kind, with dedup-on-build (map can collapse).
    assertEquals(List(2, 4, 6), FSet(1, 2, 3).map(_ * 2).toList)        // Int → Int
    assertEquals(List(0, 1), FSet(1, 2, 3, 4).map(_ % 2).toList)         // Int → Int, collapses to {0,1}
    assertEquals(List(10L, 20L, 30L), FSet(1, 2, 3).map(_.toLong * 10).toList) // Int → Long
    assertEquals(Set("1", "2", "3"), FSet(1, 2, 3).map(_.toString).toList.toSet) // Int → Ref
    assertEquals(List(1, 5, 6), FSet.from(List("a", "apple", "banana")).map(_.length).toList) // Ref → Int
    assertEquals(Set("A", "B"), FSet.from(List("apple", "avocado", "banana")).map(s => s.substring(0, 1).toUpperCase).toList.toSet) // Ref → Ref, collapses

  @Test def filter_ref(): Unit =
    val fs = FSet.from(List("apple", "banana", "avocado", "cherry"))
    assertEquals(Set("apple", "avocado"), fs.filter(_.startsWith("a")).toList.toSet)
    assertEquals(2, fs.count(_.startsWith("a")))
    assertEquals(Set("banana", "cherry"), fs.filterNot(_.startsWith("a")).toList.toSet)

  @Test def traversal_ref(): Unit =
    val fs = FSet.from(List("a", "bb", "ccc", "a"))
    var lens = 0
    fs.foreach(s => lens += s.length)
    assertEquals(1 + 2 + 3, lens)
    assertTrue(fs.exists(_.length == 3))
    assertFalse(fs.forall(_.length == 1))

  @Test def min_max_int(): Unit =
    assertEquals(1, FSet(5, 1, 3).min)
    assertEquals(5, FSet(5, 1, 3).max)
    val big = FSet.fromArray((10 until 90).toArray) // Hash leaf, arr sorted
    assertEquals(10, big.min)
    assertEquals(89, big.max)
    var threw = false
    try FSet.empty[Int].min catch { case _: NoSuchElementException => threw = true }
    assertTrue("min of empty throws", threw)

  @Test def ref_build_dedup_and_subset(): Unit =
    // many duplicates: the build dedups during the hash insert (O(n)), result is correct.
    val data = (0 until 2000).map(i => s"k${i % 100}").toArray // 100 distinct, 2000 with dups
    val fs = FSet.fromArray(data)
    assertEquals(100, fs.size)
    for i <- 0 until 100 do assertTrue(fs.contains(s"k$i"))
    assertFalse(fs.contains("k100"))

  @Test def subset_of_int(): Unit =
    val a = FSet(1, 2, 3); val b = FSet(0, 1, 2, 3, 4)
    assertTrue(a.subsetOf(b))
    assertFalse(b.subsetOf(a))
    assertTrue(FSet.empty[Int].subsetOf(a))
    assertTrue(a.subsetOf(a))
    // subsetOf works against a LAZY rhs (contains distributes)
    assertTrue(a.subsetOf(FSet(1, 2) ++ FSet(3, 4)))
    assertFalse(FSet(1, 5).subsetOf(FSet(1, 2) ++ FSet(3, 4)))

  @Test def hash_leaf_int(): Unit =
    // > 16 distinct ⇒ frozen open-addressing Hash leaf (O(1) probe). Parity with Set; merges over a Hash
    // operand still fold (asArr extracts the leaf's sorted arr).
    val data = (0 until 200 by 2).toArray // 100 evens ⇒ Hash
    val fs = FSet.fromArray(data)
    val ref = data.toSet
    assertEquals("size", ref.size, fs.size)
    for p <- -2 to 205 do assertEquals(s"contains($p)", ref.contains(p), fs.contains(p))
    val odds = FSet.fromArray((1 until 200 by 2).toArray)
    val all = (fs ++ odds).materialize
    assertEquals("union of two hash leaves", (0 until 200).toList, all.toList)
    assertEquals(200, all.size)
    assertTrue((fs & odds).materialize.toList.isEmpty) // evens ∩ odds = ∅

  @Test def hash_leaf_ref(): Unit =
    // large reference set ⇒ frozen RefHash (O(1) contains via the cached-hash probe), not the O(n) linear scan.
    val data = (0 until 100).map(i => s"k$i").toArray
    val fs = FSet.fromArray(data)
    val ref = data.toSet
    assertEquals("size", ref.size, fs.size)
    for p <- List("k0", "k50", "k99", "k100", "nope", "k7") do
      assertEquals(s"contains($p)", ref.contains(p), fs.contains(p))
    val fs2 = fs + "zz" - "k0"
    assertTrue(fs2.contains("zz"))
    assertFalse(fs2.contains("k0"))

  @Test def deep_incl_chain_no_stackoverflow(): Unit =
    // a long `s = s + x` loop builds a depth-n left-deep SUnion spine; the iterative containsLeaf
    // trampoline must answer membership without StackOverflowing (recursion would blow at ~5–25k).
    var fs: FSet[Int] = FSet.empty[Int]
    val n = 30000
    var i = 0
    while i < n do { fs = fs + i; i += 1 }
    assertTrue(fs.contains(0))
    assertTrue(fs.contains(n - 1))
    assertTrue(fs.contains(n / 2))
    assertFalse(fs.contains(-1))
    assertFalse(fs.contains(n))

  // ---- Ref kind (String) — typed Object[], equals-based membership ----

  @Test def ref_kind_string(): Unit =
    val xs = List("b", "a", "c", "a", "b", "d")
    val fs = FSet.from(xs)
    val ref = xs.toSet
    assertEquals(ref.size, fs.size)
    for p <- List("a", "b", "c", "d", "e", "z") do assertEquals(s"contains($p)", ref.contains(p), fs.contains(p))
    val fs2 = fs + "z" - "a"
    val ref2 = ref + "z" - "a"
    assertEquals(ref2.size, fs2.materialize.size)
    for p <- List("a", "z", "b", "x") do assertEquals(s"contains($p)", ref2.contains(p), fs2.contains(p))

  @Test def ref_empty_and_singleton(): Unit =
    assertTrue(FSet.empty[String].isEmpty)
    assertEquals(0, FSet.empty[String].size)
    val one = FSet("solo")
    assertEquals(1, one.size)
    assertTrue(one.contains("solo"))
    assertFalse(one.contains("other"))

  // ---- a small randomized fuzz vs immutable.Set over the slice's ops ----

  @Test def fuzz_int_vs_set(): Unit =
    val rng = new java.util.Random(0xf5e7L)
    for _ <- 0 until 200 do
      var fs: FSet[Int] = FSet.empty[Int]
      var ref = Set.empty[Int]
      val nOps = rng.nextInt(40)
      for _ <- 0 until nOps do
        val x = rng.nextInt(20)
        if rng.nextBoolean() then { fs = fs.incl(x); ref = ref + x }
        else { fs = fs.excl(x); ref = ref - x }
      val m = fs.materialize
      assertEquals(ref.size, m.size)
      assertEquals(ref.isEmpty, m.isEmpty)
      for p <- 0 until 20 do assertEquals(ref.contains(p), fs.contains(p))
