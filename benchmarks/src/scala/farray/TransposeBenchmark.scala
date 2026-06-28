package farray

import org.openjdk.jmh.annotations._

// transpose: a rows x cols matrix of Ints; transpose flips it.
// fs2.Chunk has none of these. zio.Chunk is an IndexedSeq -> full API.
class TransposeIntBenchmark extends IntInputs {
  var farrayMatrix: FArray[FArray[Int]] = _
  var listMatrix: List[List[Int]] = _
  var vectorMatrix: Vector[Vector[Int]] = _
  var zioMatrix: zio.Chunk[zio.Chunk[Int]] = _

  @Setup
  def setupMatrix(): Unit = {
    val rows = math.max(1, math.min(size, 64))
    val cols = math.max(1, size / rows)
    farrayMatrix = FArray.tabulate(rows)(r => FArray.tabulate(cols)(c => r * cols + c))
    listMatrix = List.tabulate(rows)(r => List.tabulate(cols)(c => r * cols + c))
    vectorMatrix = Vector.tabulate(rows)(r => Vector.tabulate(cols)(c => r * cols + c))
    zioMatrix = zio.Chunk.fromIterable((0 until rows).map(r => zio.Chunk.fromIterable((0 until cols).map(c => r * cols + c))))
  }

  @Benchmark def farray_transpose(): FArray[FArray[Int]] = farrayMatrix.transpose
  @Benchmark def list_transpose(): List[List[Int]] = listMatrix.transpose
  @Benchmark def vector_transpose(): Vector[Vector[Int]] = vectorMatrix.transpose
  @Benchmark def ziochunk_transpose(): zio.Chunk[zio.Chunk[Int]] = zioMatrix.transpose
}
