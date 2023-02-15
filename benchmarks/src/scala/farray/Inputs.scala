package farray

import org.openjdk.jmh.annotations.{Param, Setup}

abstract class Inputs extends CommonParams {
  var listInput: List[String] = _
  var farrayInput: FArray[String] = _
  var iarrayInput: IArray[String] = _

  @Param(Array("1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = 1000

  @Setup
  def setup(): Unit = {
    listInput = List.tabulate(size)(_.toString)
    farrayInput = FArray.tabulate(size)(_.toString)
    iarrayInput = IArray.tabulate(size)(_.toString)
  }
}
