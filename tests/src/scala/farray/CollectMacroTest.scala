package farray
import org.junit.Test
import org.junit.Assert.*

/** CollectMacro path coverage: fast path (guard once), dual-match general path (destructuring, multi-case), stored-PF runtime fallback, mid-chain typing, and
  * the exotic kinds the impl carries.
  */
class CollectMacroTest:
  @Test def guardOnceSimple(): Unit =
    var n = 0
    val out = FArray(1, 2, 3, 4, 5, 6).collect { case x if { n += 1; x % 2 == 0 } => x * 10 }
    assertEquals(List(20, 40, 60), out.toList)
    assertEquals(6, n) // fast path: guard once per element
  @Test def destructuring(): Unit =
    val xs = FArray((1, "a"), (2, "bb"), (3, "ccc"))
    assertEquals(List("bb", "ccc"), xs.collect { case (n, s) if n > 1 => s }.toList)
  @Test def multiCase(): Unit =
    val xs = FArray(1, 2, 3, 4, 5, 6)
    assertEquals(List(10, 2, 30, 4, 50, 6), xs.collect { case x if x % 2 == 1 => x * 10; case y => y }.toList)
  @Test def storedPf(): Unit =
    val pf: PartialFunction[Int, Int] = { case x if x % 2 == 0 => x * 2 }
    assertEquals(List(4, 8), FArray(1, 2, 3, 4).collect(pf).toList)
  @Test def midChain(): Unit =
    val xs = FArray(1, 2, 3, 4, 5)
    assertEquals(List(4, 9, 14), xs.map(_ + 1).collect { case x if x % 2 == 0 => x * 2 + x / 2 }.map(_ - 1).toList)
  @Test def exoticKinds(): Unit =
    val fs = FArray(1.5f, 2.5f, 3.5f)
    assertEquals(List(5.0f, 7.0f), fs.collect { case f if f > 2.0f => f * 2 }.toList)
    val bs = FArray[Byte](1, 2, 3)
    assertEquals(List[Byte](4, 6), bs.collect { case b if b > 1 => (b * 2).toByte }.toList)
