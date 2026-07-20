package farray

import org.openjdk.jmh.annotations.{Param, Setup}

/** Primitive-Int inputs, to exercise FArray's specialized IntArr path against boxed competitors. */
abstract class IntInputs extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var arrInput: Array[Int] = _ // raw array for the java.util.stream competitor
  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var farrayInput: FArray[Int] = _
  var fs2ChunkInput: fs2.Chunk[Int] = _
  var zioChunkInput: zio.Chunk[Int] = _
  var kyoChunkInput: kyo.Chunk[Int] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i)
    arrInput = arr
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i)
    farrayInput = FArray.tabulate(size)(i => i)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
    kyoChunkInput = kyo.Chunk.from(arr)
  }
}
