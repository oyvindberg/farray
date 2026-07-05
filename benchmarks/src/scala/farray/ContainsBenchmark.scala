package farray

import org.openjdk.jmh.annotations.Benchmark

// linear search for an element that is not present (full traversal)
class ContainsStrBenchmark extends Inputs {
  @Benchmark def list(): Boolean = listInput.contains("nope")
  @Benchmark def farray(): Boolean = farrayInput.contains("nope")
  @Benchmark def iarray(): Boolean = iarrayInput.contains("nope")
  @Benchmark def vector(): Boolean = vectorInput.contains("nope")
  @Benchmark def fs2chunk(): Boolean = fs2ChunkInput.contains("nope")
  @Benchmark def ziochunk(): Boolean = zioChunkInput.contains("nope")
}

// the Int twin: unboxed value compares — the boxing collections unbox per element to compare.
class ContainsIntBenchmark extends IntInputs {
  @Benchmark def farray(): Boolean = farrayInput.contains(-1)
  @Benchmark def list(): Boolean = listInput.contains(-1)
  @Benchmark def vector(): Boolean = vectorInput.contains(-1)
  @Benchmark def iarray(): Boolean = iarrayInput.contains(-1)
  @Benchmark def fs2chunk(): Boolean = fs2ChunkInput.contains(-1)
  @Benchmark def ziochunk(): Boolean = zioChunkInput.contains(-1)
}
