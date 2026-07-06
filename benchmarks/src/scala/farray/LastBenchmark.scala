package farray

import org.openjdk.jmh.annotations.Benchmark

class LastStrBenchmark extends Inputs {
  @Benchmark def list(): String = listInput.last
  @Benchmark def farray(): String = farrayInput.last
  @Benchmark def iarray(): String = iarrayInput.last
  @Benchmark def vector(): String = vectorInput.last
  @Benchmark def fs2chunk(): String = fs2ChunkInput(size - 1) // fs2.Chunk.last returns Option; apply(size-1) is the element
  @Benchmark def ziochunk(): String = zioChunkInput.last
}

class LastIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.last
  @Benchmark def farray(): Int = farrayInput.last
  @Benchmark def iarray(): Int = iarrayInput.last
  @Benchmark def vector(): Int = vectorInput.last
  @Benchmark def fs2chunk(): Int = fs2ChunkInput(size - 1) // fs2.Chunk.last returns Option; apply(size-1) is the element
  @Benchmark def ziochunk(): Int = zioChunkInput.last
}
