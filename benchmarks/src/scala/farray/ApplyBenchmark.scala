package farray

import org.openjdk.jmh.annotations.Benchmark

// indexed random-access lookup at a non-constant index (avoids constant folding)
class ApplyStrBenchmark extends Inputs {
  @Benchmark def list(): String = listInput(size / 2)
  @Benchmark def farray(): String = farrayInput(size / 2)
  @Benchmark def iarray(): String = iarrayInput(size / 2)
  @Benchmark def vector(): String = vectorInput(size / 2)
  @Benchmark def fs2chunk(): String = fs2ChunkInput(size / 2)
  @Benchmark def ziochunk(): String = zioChunkInput(size / 2)
}
