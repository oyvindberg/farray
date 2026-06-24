package farray

import org.openjdk.jmh.annotations.Benchmark

// reverse then map. FArray.reverse is an O(1) ReverseNode; map reads the base back-to-front straight into
// the result — no reversed copy is ever built. List/Vector/Array allocate the full reversed sequence first,
// then map it.
class IntReverseMapBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] = farrayInput.reverse.map(_ + 1)
  @Benchmark def list(): List[Int] = listInput.reverse.map(_ + 1)
  @Benchmark def vector(): Vector[Int] = vectorInput.reverse.map(_ + 1)
  @Benchmark def array(): Array[Int] = arrayInput.reverse.map(_ + 1)
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.reverse.map(_ + 1)
  // fs2.Chunk has no reverse
}
