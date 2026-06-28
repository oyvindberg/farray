package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Boolean inputs — exercises FArray's specialized BooleanArr path against the competitors. */
abstract class BooleanInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Boolean] = _
  var vectorInput: Vector[Boolean] = _
  var iarrayInput: IArray[Boolean] = _
  var farrayInput: FArray[Boolean] = _
  var fs2ChunkInput: fs2.Chunk[Boolean] = _
  var zioChunkInput: zio.Chunk[Boolean] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i % 2 == 0)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i % 2 == 0)
    farrayInput = FArray.tabulate(size)(i => i % 2 == 0)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
