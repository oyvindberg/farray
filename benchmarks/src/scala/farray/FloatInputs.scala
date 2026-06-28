package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Float inputs — exercises FArray's specialized FloatArr path against the competitors. */
abstract class FloatInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Float] = _
  var vectorInput: Vector[Float] = _
  var iarrayInput: IArray[Float] = _
  var farrayInput: FArray[Float] = _
  var fs2ChunkInput: fs2.Chunk[Float] = _
  var zioChunkInput: zio.Chunk[Float] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i.toFloat)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i.toFloat)
    farrayInput = FArray.tabulate(size)(i => i.toFloat)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
