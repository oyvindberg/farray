package farray
import org.junit.Test
import org.junit.Assert.assertEquals

class NewKindsSmoke {
  @Test def floatKind(): Unit = {
    val xs = FArray(1.0f, 2.5f, 3.0f)
    val ls = List(1.0f, 2.5f, 3.0f)
    assertEquals(ls.map(_ * 2), xs.map(_ * 2).toList)
    assertEquals(ls.foldLeft(0.0f)(_ + _), xs.foldLeft(0.0f)(_ + _), 0.0f)
    assertEquals(ls.filter(_ > 1.5f), xs.filter(_ > 1.5f).toList)
    assertEquals(ls.sum, xs.sum, 0.0f)
    assertEquals(ls.reverse, xs.reverse.toList)
    assertEquals(ls.hashCode, xs.toList.hashCode)
  }
  @Test def shortKind(): Unit = {
    val xs = FArray(1.toShort, 2.toShort, 3.toShort)
    val ls = List(1.toShort, 2.toShort, 3.toShort)
    assertEquals(ls.map(s => (s + 1).toShort), xs.map(s => (s + 1).toShort).toList)
    assertEquals(ls.foldLeft(0)(_ + _), xs.foldLeft(0)(_ + _))
    assertEquals(ls.sum, xs.sum)
    assertEquals(ls.toList.hashCode, xs.toList.hashCode)
  }
  @Test def byteKind(): Unit = {
    val xs = FArray(1.toByte, 2.toByte, 3.toByte)
    val ls = List(1.toByte, 2.toByte, 3.toByte)
    assertEquals(ls.map(b => (b * 2).toByte), xs.map(b => (b * 2).toByte).toList)
    assertEquals(ls.sum, xs.sum)
    assertEquals(ls.foldLeft(0)(_ + _), xs.foldLeft(0)(_ + _))
    assertEquals(ls.toList.hashCode, xs.toList.hashCode)
  }
  @Test def charKind(): Unit = {
    val xs = FArray('a', 'b', 'c')
    val ls = List('a', 'b', 'c')
    assertEquals(ls.map(_.toUpper), xs.map(_.toUpper).toList)
    assertEquals(ls.foldLeft(0)(_ + _.toInt), xs.foldLeft(0)(_ + _.toInt))
    assertEquals(ls.filter(_ != 'b'), xs.filter(_ != 'b').toList)
    assertEquals(ls.toList.hashCode, xs.toList.hashCode)
  }
  @Test def booleanKind(): Unit = {
    val xs = FArray(true, false, true, true)
    val ls = List(true, false, true, true)
    assertEquals(ls.map(!_), xs.map(!_).toList)
    assertEquals(ls.count(identity), xs.count(identity))
    assertEquals(ls.filter(identity), xs.filter(identity).toList)
    assertEquals(ls.foldLeft(0)((a, b) => a + (if (b) 1 else 0)), xs.foldLeft(0)((a, b) => a + (if (b) 1 else 0)))
    assertEquals(ls.toList.hashCode, xs.toList.hashCode)
  }
  @Test def structuralAcrossKinds(): Unit = {
    // exercise tree nodes (concat, take, drop, reverse, updated) for a new kind
    val xs = FArray.tabulate(20)(i => i.toFloat)
    val ls = List.tabulate(20)(i => i.toFloat)
    assertEquals((ls ++ ls).map(_ + 1), (xs ++ xs).map(_ + 1).toList)
    assertEquals(ls.drop(3).take(5), xs.drop(3).take(5).toList)
    assertEquals(ls.reverse, xs.reverse.toList)
    assertEquals(ls.updated(2, 99.0f), xs.updated(2, 99.0f).toList)
    assertEquals((ls ++ ls).reverse.hashCode, (xs ++ xs).reverse.toList.hashCode)
  }

}
