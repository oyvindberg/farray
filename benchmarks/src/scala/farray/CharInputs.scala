package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Char inputs — exercises FArray's specialized CharArr path against the competitors. */
abstract class CharInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Char] = _
  var vectorInput: Vector[Char] = _
  var iarrayInput: IArray[Char] = _
  var farrayInput: FArray[Char] = _
  var fs2ChunkInput: fs2.Chunk[Char] = _
  var zioChunkInput: zio.Chunk[Char] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => ('a' + (i % 26)).toChar)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => ('a' + (i % 26)).toChar)
    farrayInput = FArray.tabulate(size)(i => ('a' + (i % 26)).toChar)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }
}
