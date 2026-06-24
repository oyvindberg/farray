package farray

import org.openjdk.jmh.annotations.Benchmark

// range then map. FArray.range is a closed-form RangeNode, so map reads start+i*step straight into the
// result — the N ints are never materialised as an intermediate. List/Vector/Array build the range first,
// then map (two passes + an intermediate allocation). Scala's Range is lazy too, so it fuses similarly.
// (IArray.range shares the same flat path as Array.)
class IntRangeMapBenchmark extends IntInputs {
  @Benchmark def farray(): FArray[Int] = FArray.range(0, size).map(_ + 1)
  @Benchmark def scalaRange(): IndexedSeq[Int] = (0 until size).map(_ + 1)
  @Benchmark def list(): List[Int] = List.range(0, size).map(_ + 1)
  @Benchmark def vector(): Vector[Int] = Vector.range(0, size).map(_ + 1)
  @Benchmark def iarray(): IArray[Int] = IArray.range(0, size).map(_ + 1)
  // fs2/zio Chunk have no range constructor; build from a materialised range, then map
  @Benchmark def fs2chunk(): fs2.Chunk[Int] = fs2.Chunk.array(Array.range(0, size)).map(_ + 1)
  @Benchmark def ziochunk(): zio.Chunk[Int] = zio.Chunk.fromArray(Array.range(0, size)).map(_ + 1)
}
