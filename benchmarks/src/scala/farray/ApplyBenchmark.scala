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

// the Int twin: indexed lookup at a non-constant index, unboxed.
class ApplyIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput(size / 2)
  @Benchmark def farray(): Int = farrayInput(size / 2)
  @Benchmark def iarray(): Int = iarrayInput(size / 2)
  @Benchmark def vector(): Int = vectorInput(size / 2)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput(size / 2)
  @Benchmark def ziochunk(): Int = zioChunkInput(size / 2)
}
