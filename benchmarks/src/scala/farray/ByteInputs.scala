package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Byte inputs — exercises FArray's specialized ByteArr path against the competitors. */
abstract class ByteInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Byte] = _
  var vectorInput: Vector[Byte] = _
  var iarrayInput: IArray[Byte] = _
  var farrayInput: FArray[Byte] = _
  var fs2ChunkInput: fs2.Chunk[Byte] = _
  var zioChunkInput: zio.Chunk[Byte] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => (i % 127).toByte)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => (i % 127).toByte)
    farrayInput = FArray.tabulate(size)(i => (i % 127).toByte)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
