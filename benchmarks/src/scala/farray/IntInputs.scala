package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Int inputs, to exercise FArray's specialized IntArr path against boxed competitors. */
abstract class IntInputs extends CommonParams {
  @Param(Array("1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = 1000

  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var arrayInput: Array[Int] = _
  var farrayInput: FArray[Int] = _

  @Setup
  def setup(): Unit = {
    listInput = List.tabulate(size)(i => i)
    vectorInput = Vector.tabulate(size)(i => i)
    iarrayInput = IArray.tabulate(size)(i => i)
    arrayInput = Array.tabulate(size)(i => i)
    farrayInput = FArray.tabulate(size)(i => i)
  }
}
