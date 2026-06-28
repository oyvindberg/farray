package farray

import org.openjdk.jmh.annotations.Benchmark

// map(_ + 1) over Ints. IArray.map keeps int[] storage but boxes through the generic map function;
// FArray's specialized inline map stays unboxed int[] -> int[].
class MapIntBenchmark extends IntInputs {
  @Benchmark def list(): List[Int] = listInput.map(_ + 1)
  @Benchmark def vector(): Vector[Int] = vectorInput.map(_ + 1)
  @Benchmark def iarray(): IArray[Int] = iarrayInput.map(_ + 1)
  @Benchmark def farray(): FArray[Int] = farrayInput.map(_ + 1)
  @Benchmark def fs2chunk(): fs2.Chunk[Int] = fs2ChunkInput.map(_ + 1)
  @Benchmark def ziochunk(): zio.Chunk[Int] = zioChunkInput.map(_ + 1)
}

class MapStrBenchmark extends Inputs {
  @Benchmark def list(): List[String] = listInput.map(x => x + x)
  @Benchmark def farray(): FArray[String] = farrayInput.map(x => x + x)
  @Benchmark def iarray(): IArray[String] = iarrayInput.map(x => x + x)
  @Benchmark def vector(): Vector[String] = vectorInput.map(x => x + x)
  @Benchmark def fs2chunk(): fs2.Chunk[String] = fs2ChunkInput.map(x => x + x)
  @Benchmark def ziochunk(): zio.Chunk[String] = zioChunkInput.map(x => x + x)
}
