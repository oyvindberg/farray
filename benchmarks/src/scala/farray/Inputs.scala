package farray

import org.openjdk.jmh.annotations.{Param, Setup}

abstract class Inputs extends CommonParams {
  var listInput: List[String] = _
  var farrayInput: FArray[String] = _
  var iarrayInput: IArray[String] = _
  var vectorInput: Vector[String] = _
  var fs2ChunkInput: fs2.Chunk[String] = _
  var zioChunkInput: zio.Chunk[String] = _

  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(_.toString)
    listInput = arr.toList
    farrayInput = FArray.tabulate(size)(_.toString)
    iarrayInput = IArray.tabulate(size)(_.toString)
    vectorInput = arr.toVector
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
