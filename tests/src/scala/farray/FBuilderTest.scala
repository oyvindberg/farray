package farray

import org.junit.Test

// Parity of FBuilder against the stdlib List builder across every element kind, plus the empty / size-1
// boundaries, ++= (FArray fast path incl. trees), sizeHint, repeatable result() + append-after-result safety, and the boxing asScala adapter.
class FBuilderTest:

  // NB: `toList` is inline+kind-dispatched, so it can't be called on an abstract A; the boxed `toSeq`
  // view (a non-inline FArraySeq) is the right generic-context read.
  private def check[A](fa: FArray[A], expected: List[A]): Unit =
    val got = fa.toSeq.toList
    assert(got == expected, s"$got != $expected")

  // ---- += element-at-a-time, all kinds ----
  @Test def addOne_int: Unit =
    val b = FArray.newBuilder[Int]
    val lb = List.newBuilder[Int]
    var i = 0
    while i < 100 do { b += i; lb += i; i += 1 }
    check(b.result(), lb.result())

  @Test def addOne_long: Unit =
    val b = FArray.newBuilder[Long]
    val lb = List.newBuilder[Long]
    var i = 0L
    while i < 100 do { b += i * 3L; lb += i * 3L; i += 1L }
    check(b.result(), lb.result())

  @Test def addOne_double: Unit =
    val b = FArray.newBuilder[Double]
    val lb = List.newBuilder[Double]
    var i = 0
    while i < 100 do { b += i.toDouble / 2.0; lb += i.toDouble / 2.0; i += 1 }
    check(b.result(), lb.result())

  @Test def addOne_string: Unit =
    val b = FArray.newBuilder[String]
    val lb = List.newBuilder[String]
    var i = 0
    while i < 100 do { b += s"s$i"; lb += s"s$i"; i += 1 }
    check(b.result(), lb.result())

  // ---- boundaries ----
  @Test def empty_result: Unit =
    check(FArray.newBuilder[Int].result(), Nil)
    check(FArray.newBuilder[String].result(), Nil)

  @Test def single_result: Unit =
    check((FArray.newBuilder[Int] += 42).result(), List(42))
    check((FArray.newBuilder[String] += "x").result(), List("x"))

  // ---- ++= FArray (leaf, One, Empty, and trees) ----
  @Test def addAll_leaves_and_trees: Unit =
    val b = FArray.newBuilder[Int]
    val lb = List.newBuilder[Int]
    val leaf = FArray(1, 2, 3, 4, 5)
    val one = FArray(99)
    val empty = FArray.empty[Int]
    val tree = (FArray(10, 20) ++ FArray(30, 40)).drop(1).reverse // SliceNode/ReverseNode over Concat
    b ++= leaf; lb ++= leaf.toList
    b ++= one; lb ++= one.toList
    b ++= empty; lb ++= empty.toList
    b ++= tree; lb ++= tree.toList
    b += 7; lb += 7
    check(b.result(), lb.result())

  @Test def addAll_string_tree: Unit =
    val b = FArray.newBuilder[String]
    val lb = List.newBuilder[String]
    val t = (FArray("a", "b") ++ FArray("c", "d", "e")).reverse
    b ++= FArray("x", "y"); lb ++= List("x", "y")
    b ++= t; lb ++= t.toList
    check(b.result(), lb.result())

  // ---- sizeHint must not change contents ----
  @Test def sizeHint_preserves: Unit =
    val b = FArray.newBuilder[Int]
    b.sizeHint(1000)
    val lb = List.newBuilder[Int]
    var i = 0
    while i < 500 do { b += i; lb += i; i += 1 }
    assert(b.length == 500)
    check(b.result(), lb.result())

  @Test def sizeHint_apply_ctor: Unit =
    val b = FBuilder[Int](256)
    var i = 0
    while i < 300 do { b += i; i += 1 } // forces a regrow past the hint too
    check(b.result(), (0 until 300).toList)

  // ---- result() is repeatable and safe to keep appending after (no clear() to corrupt a shared prefix) ----
  @Test def result_repeatable_and_append_safe: Unit =
    val b = FArray.newBuilder[Int]
    b += 1; b += 2; b += 3
    val fa1 = b.result() // shares the buffer at length 3
    b += 4; b += 5 // monotonic appends past length 3
    val fa2 = b.result() // shares the buffer at length 5
    check(fa1, List(1, 2, 3)) // fa1 must be UNCHANGED by the later appends
    check(fa2, List(1, 2, 3, 4, 5))

  @Test def result_survives_regrow_after_snapshot: Unit =
    // snapshot small, then append enough to force a regrow (16 -> …): the snapshot keeps the old array
    val b = FArray.newBuilder[Int]
    for i <- 0 until 8 do b += i
    val snap = b.result()
    for i <- 8 until 5000 do b += i // regrows several times
    check(snap, (0 until 8).toList) // still intact
    assert(b.result().length == 5000)

  // ---- knownSize / nonEmpty ----
  @Test def size_accessors: Unit =
    val b = FArray.newBuilder[Double]
    assert(b.isEmpty && !b.nonEmpty && b.knownSize == 0)
    b += 1.0
    assert(b.nonEmpty && b.length == 1)

  // ---- asScala boxing interop == native path ----
  @Test def asScala_matches_native: Unit =
    val nb = FArray.newBuilder[Int]
    val sb = FArray.newBuilder[Int].asScala
    var i = 0
    while i < 50 do { nb += i; sb += i; i += 1 }
    sb ++= List(100, 200, 300)
    nb += 100; nb += 200; nb += 300
    check(sb.result(), nb.result().toList)

  @Test def asScala_factory_to: Unit =
    // drive it as a real mutable.Builder via a Factory-style loop
    val sb = FArray.newBuilder[String].asScala
    sb.sizeHint(3)
    List("p", "q", "r").foreach(sb.addOne)
    check(sb.result(), List("p", "q", "r"))

  // ---- result() does not corrupt on large N (regrow correctness) ----
  @Test def large_regrow_int: Unit =
    val b = FArray.newBuilder[Int]
    val n = 100000
    var i = 0
    while i < n do { b += i; i += 1 }
    val r = b.result()
    assert(r.length == n)
    assert(r(0) == 0 && r(n - 1) == n - 1)
    assert(r.foldLeft(0L)(_ + _) == (0L until n.toLong).sum) // Long fold: Int sum overflows at 1e5
