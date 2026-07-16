package farray

import org.junit.Test
import org.junit.Assert.*

/** Round-3 acceptance suite: the BOXED FALLBACK for kind dispatch.
  *
  * Every op below is invoked in a context where the element kind is NOT statically resolvable, so the `summonFrom` dispatch used to fail to compile ("no
  * element-kind specialization for type parameter A"). Three real categories from the Scala-3 migration are covered:
  *   1. `def generic[T](xs: FArray[T])` — an abstract type param with NO bound.
  *   2. `FArray[?]` wildcard contexts.
  *   3. A primitive-containing union `type IS = Int | String; FArray[IS]`.
  *
  * The bar is behaviour PARITY with `List` (which is always boxed), including the hard case where the abstract-`T` FArray is backed by genuinely PRIMITIVE
  * storage at runtime (a real `FArray[Int]` passed to `generic[Int]`) — the boxed read paths must box the primitive leaves via `applyBoxed`. Cross-kind
  * equality/hashCode (a boxed-built result vs a kind-specialized equivalent) is asserted too.
  */
class BoxedFallbackTest:

  // ===== generic[T] helpers (abstract T, no bound) — each forces the boxed dispatch at the generic def site =====
  private def gApply[T](xs: FArray[T]): (T, T, T) = (xs(0), xs.head, xs.last)
  private def gHeadLastOption[T](xs: FArray[T]): (Option[T], Option[T]) = (xs.headOption, xs.lastOption)
  private def gIterator[T](xs: FArray[T]): List[T] = xs.iterator.toList
  private def gReverseIterator[T](xs: FArray[T]): List[T] = xs.reverseIterator.toList
  private def gToList[T](xs: FArray[T]): List[T] = xs.toList
  private def gMap[T, U](xs: FArray[T])(f: T => U): FArray[U] = xs.map(f)
  private def gFilter[T](xs: FArray[T])(p: T => Boolean): FArray[T] = xs.filter(p)
  private def gFilterNot[T](xs: FArray[T])(p: T => Boolean): FArray[T] = xs.filterNot(p)
  private def gFlatMap[T, U](xs: FArray[T])(f: T => FArray[U]): FArray[U] = xs.flatMap(f)
  private def gMapNotNone[T, U](xs: FArray[T])(f: T => Option[U]): FArray[U] = xs.mapNotNone(f)
  private def gCollect[T, U](xs: FArray[T])(pf: PartialFunction[T, U]): FArray[U] = xs.collect(pf)
  private def gFold[T, Z](xs: FArray[T])(z: Z)(op: (Z, T) => Z): Z = xs.foldLeft(z)(op)
  private def gForeach[T](xs: FArray[T]): List[T] =
    val b = List.newBuilder[T]; xs.foreach(b += _); b.result()
  private def gExists[T](xs: FArray[T])(p: T => Boolean): Boolean = xs.exists(p)
  private def gForall[T](xs: FArray[T])(p: T => Boolean): Boolean = xs.forall(p)
  private def gFind[T](xs: FArray[T])(p: T => Boolean): Option[T] = xs.find(p)
  private def gCount[T](xs: FArray[T])(p: T => Boolean): Int = xs.count(p)
  private def gIndexWhere[T](xs: FArray[T])(p: T => Boolean): Int = xs.indexWhere(p)
  private def gBuilder[T](items: Seq[T]): FArray[T] =
    val b = FArray.newBuilder[T]; items.foreach(b += _); b.result()
  private def gToFArray[T](xs: List[T]): FArray[T] = xs.toFArray
  private def gIndices[T](xs: FArray[T]): FArray[Int] = xs.indices
  private def gZip[T, U](xs: FArray[T], ys: FArray[U]): FArray[(T, U)] = xs.zip(ys)
  private def gLazyZip[T, U, R](xs: FArray[T], ys: FArray[U])(f: (T, U) => R): FArray[R] = xs.lazyZip(ys).map(f)
  private def gMkString[T](xs: FArray[T]): String = xs.mkString("[", ",", "]")
  private def gUnapply0[T](xs: FArray[T]): Boolean = xs match { case FArray() => true; case _ => false }
  private def gUnapply2[T](xs: FArray[T]): Option[(T, T)] = xs match { case FArray(a, b) => Some((a, b)); case _ => None }
  private def gUnapplyVararg[T](xs: FArray[T]): Option[(T, Seq[T])] = xs match
    case FArray(h, rest*) => Some((h, rest))
    case _                => None
  private def gPrepend[T](xs: FArray[T]): Option[(T, List[T])] =
    import farray.`+:`
    xs match { case h +: t => Some((h, t.toList)); case _ => None }
  private def gSnoc[T](xs: FArray[T]): Option[(List[T], T)] =
    import farray.`:+`
    xs match { case init :+ last => Some((init.toList, last)); case _ => None }
  private def gEquals[T](a: FArray[T], b: FArray[T]): Boolean = a == b
  private def gHash[T](xs: FArray[T]): Int = xs.hashCode

  // ---- 1a. generic[T] over PRIMITIVE-backed storage (the hard case: IntArr read through the boxed path) ----
  @Test def generic_int_backed_reads: Unit =
    val fa = FArray(10, 20, 30, 40) // real IntArr at runtime
    val la = List(10, 20, 30, 40)
    assertEquals((10, 10, 40), gApply[Int](fa))
    assertEquals((Some(10), Some(40)), gHeadLastOption[Int](fa))
    assertEquals(la, gIterator[Int](fa)) // materializeRef must box the IntArr
    assertEquals(la.reverse, gReverseIterator[Int](fa))
    assertEquals(la, gToList[Int](fa))
    assertEquals(la, gForeach[Int](fa))
    assertEquals(la.sum, gFold[Int, Int](fa)(0)(_ + _))
    assertEquals("[10,20,30,40]", gMkString[Int](fa))

  @Test def generic_int_backed_transform_parity: Unit =
    val fa = FArray(1, 2, 3, 4, 5, 6)
    val la = List(1, 2, 3, 4, 5, 6)
    assertEquals(la.map(_ * 10), gMap[Int, Int](fa)(_ * 10).toList)
    assertEquals(la.map(x => s"v$x"), gMap[Int, String](fa)(x => s"v$x").toList)
    assertEquals(la.filter(_ % 2 == 0), gFilter[Int](fa)(_ % 2 == 0).toList)
    assertEquals(la.filterNot(_ % 2 == 0), gFilterNot[Int](fa)(_ % 2 == 0).toList)
    assertEquals(la.flatMap(x => List(x, -x)), gFlatMap[Int, Int](fa)(x => FArray(x, -x)).toList)
    assertEquals(la.flatMap(x => if x % 2 == 0 then Some(x) else None), gMapNotNone[Int, Int](fa)(x => if x % 2 == 0 then Some(x) else None).toList)
    assertEquals(la.collect { case x if x > 3 => x * 2 }, gCollect[Int, Int](fa) { case x if x > 3 => x * 2 }.toList)

  @Test def generic_int_backed_predicates_parity: Unit =
    val fa = FArray(3, 1, 4, 1, 5, 9, 2, 6)
    val la = List(3, 1, 4, 1, 5, 9, 2, 6)
    assertEquals(la.exists(_ > 8), gExists[Int](fa)(_ > 8))
    assertEquals(la.exists(_ > 100), gExists[Int](fa)(_ > 100))
    assertEquals(la.forall(_ < 10), gForall[Int](fa)(_ < 10))
    assertEquals(la.forall(_ < 5), gForall[Int](fa)(_ < 5))
    assertEquals(la.find(_ > 4), gFind[Int](fa)(_ > 4))
    assertEquals(la.count(_ % 2 == 1), gCount[Int](fa)(_ % 2 == 1))
    assertEquals(la.indexWhere(_ == 5), gIndexWhere[Int](fa)(_ == 5))

  @Test def generic_int_backed_patterns: Unit =
    assertTrue(gUnapply0[Int](FArray.empty[Int]))
    assertFalse(gUnapply0[Int](FArray(1)))
    assertEquals(Some((1, 2)), gUnapply2[Int](FArray(1, 2)))
    assertEquals(None, gUnapply2[Int](FArray(1, 2, 3)))
    assertEquals(Some((9, Seq(8, 7))), gUnapplyVararg[Int](FArray(9, 8, 7)).map((h, r) => (h, r.toSeq)))
    assertEquals(Some((1, List(2, 3))), gPrepend[Int](FArray(1, 2, 3)))
    assertEquals(None, gPrepend[Int](FArray.empty[Int]))
    assertEquals(Some((List(1, 2), 3)), gSnoc[Int](FArray(1, 2, 3)))

  @Test def generic_int_zip_indices_builder: Unit =
    val fa = FArray(1, 2, 3); val fb = FArray(4, 5, 6, 7)
    assertEquals(List((1, 4), (2, 5), (3, 6)), gZip[Int, Int](fa, fb).toList)
    assertEquals(List(5, 7, 9), gLazyZip[Int, Int, Int](fa, fb)(_ + _).toList)
    assertEquals(List(0, 1, 2), gIndices[Int](fa).toList)
    assertEquals(List(100, 200, 300), gBuilder[Int](Seq(100, 200, 300)).toList)
    assertEquals(List(1, 2, 3, 4), gToFArray[Int](List(1, 2, 3, 4)).toList)

  // withFilter (guarded for-comprehension) and flatMap-for-comprehension in a generic[T] context
  private def gForCompMap[T, U](xs: FArray[T], p: T => Boolean, f: T => U): List[U] =
    (for x <- xs if p(x) yield f(x)).toList
  private def gForCompFlat[T, U](xs: FArray[T], ys: FArray[U], p: T => Boolean): List[(T, U)] =
    (for x <- xs if p(x); y <- ys yield (x, y)).toList

  @Test def generic_withFilter_forcomp: Unit =
    val fa = FArray(1, 2, 3, 4, 5, 6); val la = List(1, 2, 3, 4, 5, 6)
    assertEquals(la.withFilter(_ % 2 == 0).map(_ * 10), gForCompMap[Int, Int](fa, _ % 2 == 0, _ * 10))
    val fs = FArray("apple", "banana", "cherry"); val ls = List("apple", "banana", "cherry")
    assertEquals(ls.withFilter(_.startsWith("b")).map(_.toUpperCase), gForCompMap[String, String](fs, _.startsWith("b"), _.toUpperCase))
    // guarded flatMap for-comprehension over abstract T, primitive-backed
    val xs = FArray(1, 2, 3); val ys = FArray(10, 20)
    val exp = for x <- List(1, 2, 3) if x != 2; y <- List(10, 20) yield (x, y)
    assertEquals(exp, gForCompFlat[Int, Int](xs, ys, _ != 2))

  // ---- 1b. generic[T] over reference storage ----
  @Test def generic_ref_parity: Unit =
    val fa = FArray("apple", "banana", "cherry")
    val la = List("apple", "banana", "cherry")
    assertEquals(("apple", "apple", "cherry"), gApply[String](fa))
    assertEquals(la, gIterator[String](fa))
    assertEquals(la.map(_.toUpperCase), gMap[String, String](fa)(_.toUpperCase).toList)
    assertEquals(la.map(_.length), gMap[String, Int](fa)(_.length).toList)
    assertEquals(la.filter(_.startsWith("b")), gFilter[String](fa)(_.startsWith("b")).toList)
    assertEquals(la.foldLeft("")(_ + _), gFold[String, String](fa)("")(_ + _))
    assertEquals(Some(("apple", "banana")), gUnapply2[String](FArray("apple", "banana")))
    assertEquals(Some(("apple", List("banana", "cherry"))), gPrepend[String](fa))
    assertEquals("[apple,banana,cherry]", gMkString[String](fa))

  // ---- 2. FArray[?] wildcard / top-type contexts, expressed as FArray[Any] ----
  // BEHAVIORAL CAVEAT the migration must know (orthogonal to the dispatch fallback, and NOT fixable here): a
  // bare `FArray[?]` cannot be used as a receiver OR a parameter type — even the non-dispatched `length`
  // fails ("value length is not a member of FArray[?]" / "unreducible application ... to wildcard arguments"),
  // a pre-existing Scala interaction between the covariant opaque type and wildcards. The migration must
  // WIDEN a wildcard to `FArray[Any]` (a runtime no-op — the opaque type erases to FBase either way). The
  // round-3 boxed fallback is exactly what then makes the `Any` element dispatch compile + run: `Any` is not
  // a statically known kind, so before it every op errored with "no element-kind specialization". These
  // helpers take `FArray[Any]` — the compilable, migration-faithful stand-in for the wildcard.
  private def wLength(xs: FArray[Any]): Int = xs.length
  private def wIterator(xs: FArray[Any]): List[Any] = xs.iterator.toList
  private def wApply(xs: FArray[Any], i: Int): Any = xs.apply(i)
  private def wHead(xs: FArray[Any]): Any = xs.head
  private def wLast(xs: FArray[Any]): Any = xs.last
  private def wToList(xs: FArray[Any]): List[Any] = xs.toList
  private def wMkString(xs: FArray[Any]): String = xs.mkString(",")
  private def wCount(xs: FArray[Any]): Int = xs.count(_ => true)
  private def wMap(xs: FArray[Any]): List[String] = xs.map(_.toString).toList
  private def wMatch3(xs: FArray[Any]): String = xs match { case FArray(a, b, c) => s"$a$b$c"; case _ => "?" }

  @Test def wildcard_over_primitive: Unit =
    val w: FArray[Any] = FArray(1, 2, 3, 4) // a real IntArr widened to FArray[Any]
    assertEquals(4, wLength(w))
    assertEquals(List(1, 2, 3, 4), wIterator(w)) // xs.iterator over the widened wildcard, primitive-backed
    assertEquals(1, wApply(w, 0))
    assertEquals(4, wApply(w, 3))
    assertEquals(List(1, 2, 3, 4), wToList(w))
    assertEquals("1,2,3,4", wMkString(w))
    assertEquals(4, wCount(w))

  @Test def wildcard_over_reference: Unit =
    val w: FArray[Any] = FArray("x", "y", "z")
    assertEquals(List("x", "y", "z"), wIterator(w))
    assertEquals("x", wHead(w))
    assertEquals("z", wLast(w))
    assertEquals("xyz", wMatch3(w))

  @Test def wildcard_map_and_filter: Unit =
    val w: FArray[Any] = FArray(5, 10, 15)
    // map over the widened wildcard: reads boxed, produces boxed
    assertEquals(List("5", "10", "15"), wMap(w))
    assertEquals(List(10, 15), w.filter(x => x.asInstanceOf[Int] >= 10).map(_.asInstanceOf[Int]).toList)

  // ---- 3. primitive-containing union ----
  @Test def union_construct_index_map_match: Unit =
    type IS = Int | String
    val u: FArray[IS] = FArray[IS](1, "a", 2, "b")
    val lu: List[IS] = List[IS](1, "a", 2, "b")
    assertEquals(4, u.length)
    assertEquals(1, u.apply(0))
    assertEquals("a", u.apply(1))
    assertEquals(lu, u.toList)
    assertEquals(lu, u.iterator.toList)
    assertEquals(lu.map(_.toString), u.map(_.toString).toList)
    assertEquals(lu.map(x => x.toString.length), u.map(x => x.toString.length).toList)
    assertEquals(lu.filter(_.isInstanceOf[String]), u.filter(_.isInstanceOf[String]).toList)
    val matched = u match { case FArray(a, b, rest*) => (a, b, rest.toList); case _ => (0, "", Nil) }
    assertEquals((1: IS, "a": IS, List[IS](2, "b")), matched)
    val (h, t) = { import farray.`+:`; u match { case x +: xs => (x, xs.toList); case _ => (0: IS, Nil) } }
    assertEquals((1: IS, List[IS]("a", 2, "b")), (h, t))

  @Test def union_fold_and_build: Unit =
    type IS = Int | String
    val u: FArray[IS] = FArray[IS](10, "hi", 20)
    // fold accumulating a string
    assertEquals("10hi20", u.foldLeft("")((acc, x) => acc + x.toString))
    // newBuilder over the union
    val b = FArray.newBuilder[IS]
    b += 1; b += "z"; b += 3
    assertEquals(List[IS](1, "z", 3), b.result().toList)
    // toFArray of a union list round-trips
    assertEquals(List[IS](1, "z", 3), List[IS](1, "z", 3).toFArray.toList)

  // ---- 4. cross-kind equality / hashCode: boxed-built result == kind-specialized equivalent ----
  @Test def crosskind_equality_int: Unit =
    val boxed: FArray[Int] = gMap[Int, Int](FArray(1, 2, 3))(_ + 10) // RefArr[Integer]-backed
    val spec: FArray[Int] = FArray(11, 12, 13) // IntArr-backed
    assertNotSame(boxed.asInstanceOf[AnyRef].getClass, spec.asInstanceOf[AnyRef].getClass) // different runtime leaf kinds
    assertEquals(spec, boxed)
    assertEquals(boxed, spec)
    assertEquals(spec.hashCode, boxed.hashCode)
    // also equals a List-derived boxed FArray and vice versa
    assertTrue(gEquals[Int](boxed, spec))
    assertTrue(gEquals[Int](spec, boxed))

  @Test def crosskind_equality_via_builder_and_tofarray: Unit =
    val fromBuilder: FArray[Int] = gBuilder[Int](Seq(1, 2, 3, 4)) // boxed RefFArrayBuilder
    val fromToFArray: FArray[Int] = gToFArray[Int](List(1, 2, 3, 4)) // Int-specialized (real IntArr)
    val spec: FArray[Int] = FArray(1, 2, 3, 4)
    assertEquals(spec, fromBuilder)
    assertEquals(fromBuilder, spec)
    assertEquals(spec, fromToFArray)
    assertEquals(spec.hashCode, fromBuilder.hashCode)
    assertEquals(fromBuilder.hashCode, fromToFArray.hashCode)
    // a boxed map result concatenated with a specialized IntArr traverses correctly across kinds
    assertEquals(List(11, 12, 13, 1, 2, 3, 4), (gMap[Int, Int](FArray(1, 2, 3))(_ + 10) ++ spec).toList)

  @Test def crosskind_equality_double: Unit =
    val boxed: FArray[Double] = gMap[Double, Double](FArray(1.0, 2.0, 3.0))(_ + 0.5)
    val spec: FArray[Double] = FArray(1.5, 2.5, 3.5)
    assertEquals(spec, boxed)
    assertEquals(spec.hashCode, boxed.hashCode)

  // ---- 5. parity vs List, plus the specialized path stays unboxed (must remain green) ----
  @Test def specialized_paths_still_unboxed: Unit =
    // concrete Int element type: arithmetic on the bound variables proves they stay primitive
    val r = FArray(1, 2, 3, 4) match
      case FArray(a, b, c, d) => a + b + c + d
      case _                  => -1
    assertEquals(10, r)
    // a specialized map/sum still works and is Int-backed
    val fa = FArray(1, 2, 3, 4, 5)
    assertEquals(15, fa.sum)
    assertEquals(List(2, 4, 6, 8, 10), fa.map(_ * 2).toList)
    // the map result of a SPECIALIZED map is a real IntArr (unboxed)
    assertTrue(fa.map(_ * 2).asInstanceOf[FBase].isInstanceOf[IntArr])

  @Test def generic_equality_and_hash_parity_vs_list: Unit =
    // build the "same" sequence two ways (boxed-generic vs specialized) and assert List-parity of ==/hashCode
    val a = gMap[Int, Int](FArray(1, 2, 3, 4))(_ + 1)
    val b = FArray(2, 3, 4, 5)
    assertEquals(List(2, 3, 4, 5) == List(2, 3, 4, 5), gEquals[Int](a, b)) // both true
    assertEquals(List(2, 3, 4, 5).hashCode == List(2, 3, 4, 5).hashCode, gHash[Int](a) == gHash[Int](b))
    // inequality parity
    assertFalse(gEquals[Int](a, FArray(2, 3, 4)))
    assertFalse(gEquals[Int](a, FArray(2, 3, 4, 6)))

  @Test def generic_empty_and_single: Unit =
    assertEquals(Nil, gIterator[Int](FArray.empty[Int]))
    assertEquals(Nil, gMap[Int, Int](FArray.empty[Int])(_ + 1).toList)
    assertTrue(gUnapply0[Int](FArray.empty[Int]))
    assertEquals(List(42), gMap[Int, Int](FArray(42))(identity).toList)
    assertEquals((7, 7, 7), gApply[Int](FArray(7)))

  // ---- structural storage (tree nodes) read through the boxed path ----
  @Test def generic_over_tree_nodes: Unit =
    // Concat / Append / Prepend / reverse of primitive leaves, all read through the boxed fallback
    val concat = FArray(1, 2) ++ FArray(3, 4) ++ FArray(5) // Concat tree over IntArrs
    assertEquals(List(1, 2, 3, 4, 5), gIterator[Int](concat))
    assertEquals(15, gFold[Int, Int](concat)(0)(_ + _))
    assertEquals(3, gApply[Int](concat)._1 + 2) // apply(0)=1
    val prep = 0 +: FArray(1, 2, 3) // Prepend node
    assertEquals(List(0, 1, 2, 3), gIterator[Int](prep))
    val app = FArray(1, 2, 3) :+ 4 // Append node
    assertEquals(List(1, 2, 3, 4), gIterator[Int](app))
    val rev = FArray(1, 2, 3, 4).reverse // ReverseNode
    assertEquals(List(4, 3, 2, 1), gIterator[Int](rev))
    assertEquals(List(40, 30, 20, 10), gMap[Int, Int](rev)(_ * 10).toList)
    // mixed-kind Concat: boxed-built (RefArr) ++ specialized (IntArr), read generically
    val mixed = gMap[Int, Int](FArray(1, 2))(_ + 100) ++ FArray(3, 4)
    assertEquals(List(101, 102, 3, 4), gIterator[Int](mixed))
