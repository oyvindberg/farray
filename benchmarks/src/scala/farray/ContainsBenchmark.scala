package farray

import org.openjdk.jmh.annotations.Benchmark

// linear search for an element that is not present (full traversal)
class ContainsBenchmark extends Inputs {
  @Benchmark def list(): Boolean   = listInput.contains("nope")
  @Benchmark def farray(): Boolean = farrayInput.contains("nope")
  @Benchmark def iarray(): Boolean = iarrayInput.contains("nope")
  @Benchmark def vector(): Boolean = vectorInput.contains("nope")
  @Benchmark def fs2chunk(): Boolean = fs2ChunkInput.contains("nope")
  @Benchmark def ziochunk(): Boolean = zioChunkInput.contains("nope")
}
