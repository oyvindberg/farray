package farray

import org.openjdk.jmh.annotations.Benchmark

class HeadStrBenchmark extends Inputs {
  @Benchmark def list(): String = listInput.head
  @Benchmark def farray(): String = farrayInput.head
  @Benchmark def iarray(): String = iarrayInput.head
  @Benchmark def vector(): String = vectorInput.head
  @Benchmark def fs2chunk(): String = fs2ChunkInput(0) // fs2.Chunk.head returns Option; apply(0) is the element
  @Benchmark def ziochunk(): String = zioChunkInput.head
  @Benchmark def kyochunk(): String = kyoChunkInput.head
}

class HeadIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.head
  @Benchmark def farray(): Int = farrayInput.head
  @Benchmark def iarray(): Int = iarrayInput.head
  @Benchmark def vector(): Int = vectorInput.head
  @Benchmark def fs2chunk(): Int = fs2ChunkInput(0) // fs2.Chunk.head returns Option; apply(0) is the element
  @Benchmark def ziochunk(): Int = zioChunkInput.head
  @Benchmark def kyochunk(): Int = kyoChunkInput.head
}
