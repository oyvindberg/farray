package farray

import org.junit.Test
import org.junit.Assert.*

/** Parity + pattern-match tests for the dotty-migration gap-fill surface (Phase 0):
  * `unapplySeq`, the `+:` / `:+` name-based extractors, `newBuilder`, `toFArray`, `lazyZip`,
  * `withFilter`, and the small ops. */
class DottyGapsTest:
  @Test def arity0_int: Unit =
    assertTrue(FArray.empty[Int] match { case FArray() => true; case _ => false })
    assertFalse(FArray(1) match { case FArray() => true; case _ => false })

  @Test def arity2_int_unboxed: Unit =
    val r = FArray(1, 2) match
      case FArray(a, b) => a + b // a, b must be Int (unboxed) for + to work numerically
      case _            => -1
    assertEquals(3, r)

  @Test def arity1_int: Unit =
    val r = FArray(42) match
      case FArray(a) => a
      case _         => -1
    assertEquals(42, r)

  @Test def vararg_suffix_int: Unit =
    val r: Seq[Int] = FArray(1, 2, 3, 4) match
      case FArray(h, rest*) => rest
      case _                => Seq.empty
    assertEquals(Seq(2, 3, 4), r)
    // first-element unboxed
    val h = FArray(9, 8, 7) match { case FArray(x, _*) => x; case _ => -1 }
    assertEquals(9, h)

  @Test def arity2_ref: Unit =
    val r = FArray("a", "b") match
      case FArray(a, b) => a + b
      case _            => "?"
    assertEquals("ab", r)

  @Test def vararg_suffix_ref: Unit =
    val r = FArray("x", "y", "z") match
      case FArray(h, rest*) => (h, rest.toList)
      case _                => ("?", Nil)
    assertEquals(("x", List("y", "z")), r)

  @Test def no_match_wrong_arity: Unit =
    assertFalse(FArray(1, 2, 3) match { case FArray(_, _) => true; case _ => false })

  // ---- Task 1: +: / :+ extractors ----
  @Test def prepend_head_tail_int: Unit =
    import farray.`+:`
    val r = FArray(1, 2, 3) match
      case h +: t => (h, t.toList)
      case _      => (-1, Nil)
    assertEquals((1, List(2, 3)), r)

  @Test def prepend_empty_no_match: Unit =
    import farray.`+:`
    assertFalse(FArray.empty[Int] match { case _ +: _ => true; case _ => false })

  @Test def prepend_deep_int: Unit =
    import farray.`+:`
    val r = FArray(1, 2, 3, 4) match
      case a +: b +: rest => (a, b, rest.toList)
      case _              => (-1, -1, Nil)
    assertEquals((1, 2, List(3, 4)), r)

  @Test def prepend_on_prepend_node: Unit =
    import farray.`+:`
    val fa = 0 +: FArray(1, 2, 3) // Prepend node fast path
    val r = fa match { case h +: t => (h, t.toList); case _ => (-1, Nil) }
    assertEquals((0, List(1, 2, 3)), r)

  @Test def prepend_ref: Unit =
    import farray.`+:`
    val r = FArray("a", "b", "c") match
      case h +: t => (h, t.toList) // inline toList on the ref tail — must not crash LambdaLift
      case _      => ("?", Nil)
    assertEquals(("a", List("b", "c")), r)

  @Test def snoc_init_last_int: Unit =
    import farray.`:+`
    val r = FArray(1, 2, 3) match
      case init :+ last => (init.toList, last)
      case _            => (Nil, -1)
    assertEquals((List(1, 2), 3), r)

  @Test def snoc_empty_no_match: Unit =
    import farray.`:+`
    assertFalse(FArray.empty[Int] match { case _ :+ _ => true; case _ => false })

  @Test def snoc_on_append_node: Unit =
    import farray.`:+`
    val fa = FArray(1, 2, 3) :+ 4 // Append node fast path
    val r = fa match { case init :+ last => (init.toList, last); case _ => (Nil, -1) }
    assertEquals((List(1, 2, 3), 4), r)

  @Test def snoc_deep_int: Unit =
    import farray.`:+`
    val r = FArray(1, 2, 3, 4) match
      case init :+ y :+ z => (init.toList, y, z)
      case _              => (Nil, -1, -1)
    assertEquals((List(1, 2), 3, 4), r)

  @Test def snoc_ref: Unit =
    import farray.`:+`
    val r = FArray("a", "b", "c") match
      case init :+ last => (init.toList, last) // inline toList on the ref init — must not crash
      case _            => (Nil, "?")
    assertEquals((List("a", "b"), "c"), r)

  // ---- Task 2: newBuilder ----
  @Test def builder_int: Unit =
    val b = FArray.newBuilder[Int]
    b += 1; b += 2; b += 3
    assertEquals(List(1, 2, 3), b.result().toList)

  @Test def builder_empty: Unit =
    assertEquals(Nil, FArray.newBuilder[Int].result().toList)
    assertTrue(FArray.newBuilder[String].result().isEmpty)

  @Test def builder_single: Unit =
    val b = FArray.newBuilder[String]; b += "x"
    assertEquals(List("x"), b.result().toList)

  @Test def builder_sizeHint_and_grow: Unit =
    val b = FArray.newBuilder[Int]
    b.sizeHint(1000)
    var i = 0; while i < 1000 do { b += i; i += 1 }
    val r = b.result()
    assertEquals(1000, r.length)
    assertEquals((0 until 1000).toList, r.toList)

  @Test def builder_addAll_farray_and_iterable: Unit =
    val b = FArray.newBuilder[Int]
    b ++= FArray(1, 2, 3)
    b ++= List(4, 5)
    b ++= Iterator(6, 7)
    assertEquals((1 to 7).toList, b.result().toList)

  @Test def builder_ref_addAll: Unit =
    val b = FArray.newBuilder[String]
    b ++= FArray("a", "b")
    b ++= List("c")
    b += "d"
    assertEquals(List("a", "b", "c", "d"), b.result().toList)

  @Test def builder_clear: Unit =
    val b = FArray.newBuilder[Int]
    b += 1; b += 2; b.clear(); b += 9
    assertEquals(List(9), b.result().toList)

  @Test def builder_parity_vs_vector: Unit =
    // build the same sequence with Vector.newBuilder and FArray.newBuilder, compare
    def build[B](add: (Int => Unit)): Unit = { var i = 0; while i < 257 do { add(i * 3); i += 1 } }
    val vb = Vector.newBuilder[Int]; build(vb += _)
    val fb = FArray.newBuilder[Int]; build(fb += _)
    assertEquals(vb.result().toList, fb.result().toList)

  // ---- Task 3: toFArray ----
  @Test def toFArray_list_int: Unit =
    val r = List(1, 2, 3).toFArray
    assertEquals(List(1, 2, 3), r.toList)
    // must be a real Int leaf so Int-specialized ops work (not a boxed RefArr mistyped as FArray[Int])
    assertEquals(List(2, 3, 4), r.map(_ + 1).toList)
    assertEquals(6, r.sum)

  @Test def toFArray_list_ref: Unit =
    assertEquals(List("a", "b"), List("a", "b").toFArray.toList)

  @Test def toFArray_empty: Unit =
    assertTrue(List.empty[Int].toFArray.isEmpty)
    assertTrue(Iterator.empty[String].toFArray.isEmpty)

  @Test def toFArray_arraybuffer_and_arrayseq: Unit =
    assertEquals(List(1, 2, 3), scala.collection.mutable.ArrayBuffer(1, 2, 3).toFArray.toList)
    assertEquals(List(4, 5), scala.collection.immutable.ArraySeq(4, 5).toFArray.toList)

  @Test def toFArray_iterator_and_vector: Unit =
    assertEquals((0 until 100).toList, (0 until 100).iterator.toFArray.toList)
    assertEquals(Vector(7, 8, 9).toList, Vector(7, 8, 9).toFArray.toList)

  @Test def toFArray_farrayseq_roundtrip_o1: Unit =
    val orig = FArray(1, 2, 3, 4)
    val back = orig.toSeq.toFArray // FArraySeq unwrap — same backing core
    assertEquals(orig.toList, back.toList)
    assertSame(orig.asInstanceOf[FBase], back.asInstanceOf[FBase])

  @Test def toFArray_listbuffer: Unit =
    val lb = scala.collection.mutable.ListBuffer.empty[String]
    lb += "x"; lb += "y"
    assertEquals(List("x", "y"), lb.toFArray.toList)

  // ---- Task 4: lazyZip (2-ary + 3-ary) ----
  @Test def lazyZip2_map_parity: Unit =
    val a = FArray(1, 2, 3, 4); val b = FArray("a", "b", "c")
    val la = List(1, 2, 3, 4); val lb = List("a", "b", "c")
    assertEquals(la.lazyZip(lb).map((i, s) => s"$i$s"), a.lazyZip(b).map((i, s) => s"$i$s").toList)

  @Test def lazyZip2_foreach_forall_exists: Unit =
    val a = FArray(1, 2, 3); val b = FArray(10, 20, 30)
    val sb = scala.collection.mutable.ArrayBuffer.empty[Int]
    a.lazyZip(b).foreach((x, y) => sb += (x + y))
    assertEquals(List(11, 22, 33), sb.toList)
    assertTrue(a.lazyZip(b).forall((x, y) => y > x))
    assertFalse(a.lazyZip(b).forall((x, y) => x > 1))
    assertTrue(a.lazyZip(b).exists((x, y) => x == 2 && y == 20))
    assertFalse(a.lazyZip(b).exists((x, y) => x == 9))

  @Test def lazyZip2_foldLeft_flatMap_collect: Unit =
    val a = FArray(1, 2, 3); val b = FArray(4, 5, 6)
    assertEquals(1 * 4 + 2 * 5 + 3 * 6, a.lazyZip(b).foldLeft(0)((acc, x, y) => acc + x * y))
    assertEquals(List(1, 4, 2, 5, 3, 6), a.lazyZip(b).flatMap((x, y) => FArray(x, y)).toList)
    val c = a.lazyZip(b).collect { case (x, y) if x % 2 == 1 => x + y }
    assertEquals(List(5, 9), c.toList)

  @Test def lazyZip2_min_length_and_index_and_toFArray: Unit =
    val a = FArray(1, 2, 3, 4, 5); val b = FArray("a", "b")
    assertEquals(List((1, "a"), (2, "b")), a.lazyZip(b).toFArray.toList)
    assertEquals(List((1, "a", 0), (2, "b", 1)), a.lazyZip(b).zipWithIndex.toList)

  @Test def lazyZip3_parity: Unit =
    val a = FArray(1, 2, 3, 4); val b = FArray("a", "b", "c"); val c = FArray(true, false, true, false, true)
    val la = List(1, 2, 3, 4); val lb = List("a", "b", "c"); val lc = List(true, false, true, false, true)
    assertEquals(
      la.lazyZip(lb).lazyZip(lc).map((i, s, z) => s"$i$s$z"),
      a.lazyZip(b).lazyZip(c).map((i, s, z) => s"$i$s$z").toList
    )
    assertEquals(6, a.lazyZip(b).lazyZip(c).foldLeft(0)((acc, i, _, _) => acc + i)) // min len 3: 1+2+3
    assertTrue(a.lazyZip(b).lazyZip(c).exists((i, s, z) => i == 2 && s == "b" && !z))

  @Test def lazyZip2_empty: Unit =
    assertEquals(Nil, FArray.empty[Int].lazyZip(FArray(1, 2)).map((x, y) => x + y).toList)

  // ---- Task 5: withFilter ----
  @Test def withFilter_forcomp_map_parity: Unit =
    val fa = FArray(1, 2, 3, 4, 5, 6)
    val la = List(1, 2, 3, 4, 5, 6)
    val f = for x <- fa if x % 2 == 0 yield x * 10
    val l = for x <- la if x % 2 == 0 yield x * 10
    assertEquals(l, f.toList)

  @Test def withFilter_double_guard: Unit =
    val fa = FArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val la = (1 to 10).toList
    val f = for x <- fa if x % 2 == 0 if x > 4 yield x
    val l = for x <- la if x % 2 == 0 if x > 4 yield x
    assertEquals(l, f.toList)

  @Test def withFilter_foreach: Unit =
    val fa = FArray(1, 2, 3, 4, 5)
    val sb = scala.collection.mutable.ArrayBuffer.empty[Int]
    for x <- fa if x > 2 do sb += x
    assertEquals(List(3, 4, 5), sb.toList)

  @Test def withFilter_flatMap_forcomp: Unit =
    val xs = FArray(1, 2, 3); val ys = FArray(10, 20)
    val f = for x <- xs if x != 2; y <- ys yield x + y
    val lf = for x <- List(1, 2, 3) if x != 2; y <- List(10, 20) yield x + y
    assertEquals(lf, f.toList)

  @Test def withFilter_ref_and_empty: Unit =
    val fa = FArray("apple", "banana", "cherry")
    assertEquals(List("APPLE"), (for s <- fa if s.startsWith("a") yield s.toUpperCase).toList)
    assertEquals(Nil, (for x <- FArray.empty[Int] if x > 0 yield x).toList)

  // ---- Task 6: small ops ----
  @Test def option_min_max: Unit =
    val fa = FArray(3, 1, 2); val la = List(3, 1, 2)
    assertEquals(la.minOption, fa.minOption)
    assertEquals(la.maxOption, fa.maxOption)
    assertEquals(None, FArray.empty[Int].minOption)
    assertEquals(None, FArray.empty[Int].maxOption)
    assertEquals(la.minByOption(x => -x), fa.minByOption(x => -x))
    assertEquals(la.maxByOption(x => -x), fa.maxByOption(x => -x))
    assertEquals(None, FArray.empty[String].maxByOption(_.length))

  @Test def zipAll_parity: Unit =
    val a = FArray(1, 2, 3); val b = FArray("a", "b", "c", "d", "e")
    val la = List(1, 2, 3); val lb = List("a", "b", "c", "d", "e")
    assertEquals(la.zipAll(lb, 0, "z"), a.zipAll(b, 0, "z").toList)
    assertEquals(lb.zipAll(la, "z", 0), b.zipAll(a, "z", 0).toList)
    assertEquals(la.zipAll(la, 0, 0), a.zipAll(a, 0, 0).toList)

  @Test def sizeCompare_ops: Unit =
    val fa = FArray(1, 2, 3)
    assertTrue(fa.sizeCompare(5) < 0)
    assertEquals(0, fa.sizeCompare(3))
    assertTrue(fa.sizeCompare(1) > 0)
    assertTrue(fa.sizeCompare(FArray(1, 2, 3, 4)) < 0)
    assertEquals(0, fa.sizeCompare(FArray("a", "b", "c")))

  @Test def copyToArray_overloads: Unit =
    val fa = FArray(1, 2, 3, 4, 5)
    val d1 = new Array[Int](3); assertEquals(3, fa.copyToArray(d1)); assertEquals(List(1, 2, 3), d1.toList)
    val d2 = new Array[Int](10); val n = fa.copyToArray(d2, 2)
    assertEquals(5, n); assertEquals(List(0, 0, 1, 2, 3, 4, 5, 0, 0, 0), d2.toList)
    val d3 = new Array[Int](5); assertEquals(2, fa.copyToArray(d3, 3)); assertEquals(List(0, 0, 0, 1, 2), d3.toList) // only 2 slots left

  @Test def groupMapReduce_parity: Unit =
    val fa = FArray(1, 2, 3, 4, 5, 6, 7)
    val la = List(1, 2, 3, 4, 5, 6, 7)
    // sum of squares by parity
    assertEquals(
      la.groupMapReduce(_ % 2)(x => x * x)(_ + _),
      fa.groupMapReduce(_ % 2)(x => x * x)(_ + _)
    )
    val ws = FArray("apple", "banana", "avocado", "cherry", "blueberry")
    val lws = List("apple", "banana", "avocado", "cherry", "blueberry")
    assertEquals(
      lws.groupMapReduce(_.head)(_.length)(_ + _),
      ws.groupMapReduce(_.head)(_.length)(_ + _)
    )

  // ---- Task 7: ref-element sort stability (determinism for dotty) ----
  @Test def sort_stability: Unit =
    // (key, tag) pairs with DUPLICATE keys; a stable sort by key preserves tag order within a key.
    // Compare against List's sort (documented stable) as the oracle, for sorted/sortBy/sortWith.
    val data = List((3, "a"), (1, "b"), (2, "c"), (1, "d"), (3, "e"), (2, "f"), (1, "g"), (3, "h"))
    val fa: FArray[(Int, String)] = data.toFArray
    given Ordering[(Int, String)] = Ordering.by(_._1) // order ONLY by key, so ties are order-sensitive

    assertEquals(data.sorted, fa.sorted.toList)
    assertEquals(data.sortBy(_._1), fa.sortBy(_._1).toList)
    assertEquals(data.sortWith((x, y) => x._1 < y._1), fa.sortWith((x, y) => x._1 < y._1).toList)

    // explicit expectation: within each key, tags stay in first-seen order
    assertEquals(
      List((1, "b"), (1, "d"), (1, "g"), (2, "c"), (2, "f"), (3, "a"), (3, "e"), (3, "h")),
      fa.sortBy(_._1).toList
    )

    // larger, forces the merge path (> 32) — still stable
    val big = (0 until 500).map(i => (i % 7, i)).toList
    assertEquals(big.sortBy(_._1), big.toFArray.sortBy(_._1).toList)

  @Test def prepend_recursive_sum: Unit =
    import farray.`+:`
    def sum(xs: FArray[Int]): Int = xs match
      case h +: t => h + sum(t)
      case _      => 0
    assertEquals(15, sum(FArray(1, 2, 3, 4, 5)))
    assertEquals(15, sum(FArray(1, 2, 3, 4, 5).map(_ + 0))) // leaf, not Prepend
