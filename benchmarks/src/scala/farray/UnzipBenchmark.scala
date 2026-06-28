package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}

// unzip reads each tuple ONCE and fills both output arrays in a single pass (vs two tabulate passes).
class UnzipIntBenchmark extends CommonParams {
  @Param(Array("1", "10", "100", "1000", "10000"))
  var size: Int = 1000
  var farrayInput: FArray[(Int, Int)] = _
  var listInput: List[(Int, Int)] = _
  var iarrayInput: IArray[(Int, Int)] = _
  var vectorInput: Vector[(Int, Int)] = _
  var zioChunkInput: zio.Chunk[(Int, Int)] = _
  @Setup def setup(): Unit =
    farrayInput = FArray.tabulate(size)(i => (i, i * 2))
    listInput = List.tabulate(size)(i => (i, i * 2))
    iarrayInput = IArray.tabulate(size)(i => (i, i * 2))
    vectorInput = Vector.tabulate(size)(i => (i, i * 2))
    zioChunkInput = zio.Chunk.fromIterable(List.tabulate(size)(i => (i, i * 2)))
  @Benchmark def farray(): (FArray[Int], FArray[Int]) = farrayInput.unzip
  @Benchmark def list(): (List[Int], List[Int]) = listInput.unzip
  @Benchmark def iarray(): (IArray[Int], IArray[Int]) = iarrayInput.unzip
  @Benchmark def vector(): (Vector[Int], Vector[Int]) = vectorInput.unzip
  @Benchmark def ziochunk(): (zio.Chunk[Int], zio.Chunk[Int]) = zioChunkInput.unzip
  // fs2.Chunk has no unzip
}

class UnzipStrBenchmark extends CommonParams {
  @Param(Array("1", "10", "100", "1000", "10000"))
  var size: Int = 1000
  var farrayInput: FArray[(String, Int)] = _
  var listInput: List[(String, Int)] = _
  var iarrayInput: IArray[(String, Int)] = _
  var vectorInput: Vector[(String, Int)] = _
  var zioChunkInput: zio.Chunk[(String, Int)] = _
  @Setup def setup(): Unit =
    farrayInput = FArray.tabulate(size)(i => (i.toString, i))
    listInput = List.tabulate(size)(i => (i.toString, i))
    iarrayInput = IArray.tabulate(size)(i => (i.toString, i))
    vectorInput = Vector.tabulate(size)(i => (i.toString, i))
    zioChunkInput = zio.Chunk.fromIterable(List.tabulate(size)(i => (i.toString, i)))
  @Benchmark def farray(): (FArray[String], FArray[Int]) = farrayInput.unzip
  @Benchmark def list(): (List[String], List[Int]) = listInput.unzip
  @Benchmark def iarray(): (IArray[String], IArray[Int]) = iarrayInput.unzip
  @Benchmark def vector(): (Vector[String], Vector[Int]) = vectorInput.unzip
  @Benchmark def ziochunk(): (zio.Chunk[String], zio.Chunk[Int]) = zioChunkInput.unzip
  // fs2.Chunk has no unzip
}
