package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}

class FilterStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.filter(_.length > 1)
  @Benchmark def farray(): FArray[String] = farrayInput.filter(_.length > 1)
  @Benchmark def iarray(): IArray[String] = iarrayInput.filter(_.length > 1)
  @Benchmark def vector(): Vector[String] = vectorInput.filter(_.length > 1)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.filter(_.length > 1)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.filter(_.length > 1)
}

// filter with an UNPREDICTABLE predicate on REFERENCES: random-content strings, keep by hash
// parity — a coin flip per element, like FilterIntBenchmark, but on the kind where FArray
// deliberately KEEPS the branchy keep-write (an unconditional ref store pays a GC write barrier
// even for rejected elements, so the branchless trick is primitive-only).
class FilterRandStrBenchmark extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[String] = _
  var vectorInput: Vector[String] = _
  var iarrayInput: IArray[String] = _
  var farrayInput: FArray[String] = _
  var fs2ChunkInput: fs2.Chunk[String] = _
  var zioChunkInput: zio.Chunk[String] = _

  @Setup
  def setup(): Unit = {
    val rnd = new java.util.Random(42)
    val arr = Array.fill(size)(rnd.nextInt().toString)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.unsafeFromArray(arr.clone())
    farrayInput = FArray.fromArray(arr.clone())
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }

  @Benchmark def farray(): FArray[String] = farrayInput.filter(s => (s.hashCode & 1) == 0)
  @Benchmark def list(): List[String] = listInput.filter(s => (s.hashCode & 1) == 0)
  @Benchmark def iarray(): IArray[String] = iarrayInput.filter(s => (s.hashCode & 1) == 0)
  @Benchmark def vector(): Vector[String] = vectorInput.filter(s => (s.hashCode & 1) == 0)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.filter(s => (s.hashCode & 1) == 0)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.filter(s => (s.hashCode & 1) == 0)
}

// filter with an UNPREDICTABLE predicate: RANDOM ints, keep the even half. On the sequential
// 0..n input a parity test is a pattern the branch predictor learns perfectly; on random input
// every element is a coin flip — the case that separates a branchy keep-write (one mispredict
// per surprise) from FArray's branchless prim form (no branch at all, vectorizes).
class FilterIntBenchmark extends CommonParams {
  @Param(Array("0", "1", "10", "100", "1000", "10000", "100000"))
  var size: Int = 1000

  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var farrayInput: FArray[Int] = _
  var fs2ChunkInput: fs2.Chunk[Int] = _
  var zioChunkInput: zio.Chunk[Int] = _

  @Setup
  def setup(): Unit = {
    val rnd = new java.util.Random(42)
    val arr = Array.fill(size)(rnd.nextInt())
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.unsafeFromArray(arr.clone())
    farrayInput = FArray.fromArray(arr.clone())
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }

  // start:filter-rand
  @Benchmark def farray(): FArray[Int] = farrayInput.filter(x => (x & 1) == 0)
  // stop:filter-rand
  @Benchmark def list(): List[Int] = listInput.filter(x => (x & 1) == 0)
  @Benchmark def iarray(): IArray[Int] = iarrayInput.filter(x => (x & 1) == 0)
  @Benchmark def vector(): Vector[Int] = vectorInput.filter(x => (x & 1) == 0)
  @Benchmark def fs2chunk(): fs2.Chunk[Int] = fs2ChunkInput.filter(x => (x & 1) == 0)
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.filter(x => (x & 1) == 0)
}
