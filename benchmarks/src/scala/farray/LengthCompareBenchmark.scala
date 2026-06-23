package farray

import org.openjdk.jmh.annotations.Benchmark

class LengthCompareBenchmark extends Inputs {
  @Benchmark def list(): Int = listInput.lengthCompare(size / 2)
  @Benchmark def farray(): Int = farrayInput.lengthCompare(size / 2)
  @Benchmark def iarray(): Int = iarrayInput.lengthCompare(size / 2)
  @Benchmark def vector(): Int = vectorInput.lengthCompare(size / 2)
  @Benchmark def ziochunk(): Int = zioChunkInput.lengthCompare(size / 2)
  // fs2.Chunk has no lengthCompare
}
