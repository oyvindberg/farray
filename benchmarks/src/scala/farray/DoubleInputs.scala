package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Double inputs — exercises FArray's specialized DoubleArr path against the competitors. */
abstract class DoubleInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Double] = _
  var vectorInput: Vector[Double] = _
  var iarrayInput: IArray[Double] = _
  var farrayInput: FArray[Double] = _
  var fs2ChunkInput: fs2.Chunk[Double] = _
  var zioChunkInput: zio.Chunk[Double] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i.toDouble)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i.toDouble)
    farrayInput = FArray.tabulate(size)(i => i.toDouble)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
