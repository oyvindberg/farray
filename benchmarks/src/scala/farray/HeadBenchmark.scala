package farray

import org.openjdk.jmh.annotations.Benchmark

class HeadStrBenchmark extends Inputs {
  @Benchmark def list(): String = listInput.head
  @Benchmark def farray(): String = farrayInput.head
  @Benchmark def iarray(): String = iarrayInput.head
  @Benchmark def vector(): String = vectorInput.head
  @Benchmark def fs2chunk(): String = fs2ChunkInput(0) // fs2.Chunk.head returns Option; apply(0) is the element
  @Benchmark def ziochunk(): String = zioChunkInput.head
}
