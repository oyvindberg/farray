package farray
import org.junit.Test
import org.junit.Assert.*

/** Regression guard: the ClassTag typed-backing trick (tabulate/fill -> String[] backing) must not leak a narrow runtime array out through toArray[Object] —
  * materializeRef forces a genuine Object[].
  */
class ToArrayBackingTest:
  @Test def toArrayObjectIsGenuineObjectArray(): Unit =
    val strs = FArray.tabulate(4)(_.toString) // String[] backing
    val objs: FArray[Object] = strs // covariant widen, no cast
    val arr: Array[Object] = objs.toArray // must be a genuine Object[], not a secret String[]
    assertEquals(classOf[Array[Object]], arr.getClass) // was Array[String] before the fix
    arr(0) = Integer.valueOf(5) // would throw ArrayStoreException on a String[]
    assertEquals(5, arr(0))
  @Test def toArrayStringStillTyped(): Unit =
    val strs = FArray.tabulate(4)(_.toString)
    val arr: Array[String] = strs.toArray // concrete type still works + fast path
    assertEquals(classOf[Array[String]], arr.getClass)
    assertEquals(List("0", "1", "2", "3"), arr.toList)
