package farray

import org.openjdk.jmh.annotations.{Param, Setup}

abstract class Inputs extends CommonParams {
  var strArrInput: Array[String] = _ // raw array for the java.util.stream competitor
  var listInput: List[String] = _
  var farrayInput: FArray[String] = _
  var iarrayInput: IArray[String] = _
  var vectorInput: Vector[String] = _
  var fs2ChunkInput: fs2.Chunk[String] = _
  var zioChunkInput: zio.Chunk[String] = _
  var kyoChunkInput: kyo.Chunk[String] = _

  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(_.toString)
    strArrInput = arr
    listInput = arr.toList
    farrayInput = FArray.tabulate(size)(_.toString)
    iarrayInput = IArray.tabulate(size)(_.toString)
    vectorInput = arr.toVector
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
    kyoChunkInput = kyo.Chunk.from(arr)
  }
}
