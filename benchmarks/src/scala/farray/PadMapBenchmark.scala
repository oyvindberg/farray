package farray

import org.openjdk.jmh.annotations.Benchmark

// padTo (to 2N) then map. FArray.padTo is an O(1) PadNode; map streams the base then the filler run straight
// into the result — the doubled-length sequence is never materialised first. List/Vector/Array build all 2N
// padded elements, then map them.
class PadMapBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] = farrayInput.padTo(size * 2, -1).map(_ + 1)
  @Benchmark def list(): List[Int]     = listInput.padTo(size * 2, -1).map(_ + 1)
  @Benchmark def vector(): Vector[Int] = vectorInput.padTo(size * 2, -1).map(_ + 1)
  @Benchmark def array(): Array[Int]   = arrayInput.padTo(size * 2, -1).map(_ + 1)
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.padTo(size * 2, -1).map(_ + 1)
  // fs2.Chunk has no padTo
}
