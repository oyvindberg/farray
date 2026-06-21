package farray

import org.openjdk.jmh.annotations.Benchmark

// full traversal via foreach, accumulating into a returned scalar
class ForeachBenchmark extends Inputs {
  @Benchmark def list(): Int = {
    var acc = 0
    listInput.foreach(acc += _.length)
    acc
  }
  @Benchmark def farray(): Int = {
    var acc = 0
    farrayInput.foreach(acc += _.length)
    acc
  }
  @Benchmark def iarray(): Int = {
    var acc = 0
    iarrayInput.foreach(acc += _.length)
    acc
  }
  @Benchmark def vector(): Int = {
    var acc = 0
    vectorInput.foreach(acc += _.length)
    acc
  }
  @Benchmark def fs2chunk(): Int = {
    var acc = 0
    fs2ChunkInput.foreach(acc += _.length)
    acc
  }
  @Benchmark def ziochunk(): Int = {
    var acc = 0
    zioChunkInput.foreach(acc += _.length)
    acc
  }
}
