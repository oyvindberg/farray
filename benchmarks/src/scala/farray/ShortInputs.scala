package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Short inputs — exercises FArray's specialized ShortArr path against the competitors. */
abstract class ShortInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Short] = _
  var vectorInput: Vector[Short] = _
  var iarrayInput: IArray[Short] = _
  var farrayInput: FArray[Short] = _
  var fs2ChunkInput: fs2.Chunk[Short] = _
  var zioChunkInput: zio.Chunk[Short] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i.toShort)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i.toShort)
    farrayInput = FArray.tabulate(size)(i => i.toShort)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
