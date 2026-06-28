package farray

import org.openjdk.jmh.annotations.Benchmark

// full traversal via iterator, accumulating into a returned scalar
class IteratorStrBenchmark extends Inputs {
  @Benchmark def list(): Int = {
    var acc = 0
    val it = listInput.iterator
    while (it.hasNext) acc += it.next().length
    acc
  }
  @Benchmark def farray(): Int = {
    var acc = 0
    val it = farrayInput.iterator
    while (it.hasNext) acc += it.next().length
    acc
  }
  @Benchmark def iarray(): Int = {
    var acc = 0
    val it = iarrayInput.iterator
    while (it.hasNext) acc += it.next().length
    acc
  }
  @Benchmark def vector(): Int = {
    var acc = 0
    val it = vectorInput.iterator
    while (it.hasNext) acc += it.next().length
    acc
  }
  @Benchmark def fs2chunk(): Int = {
    var acc = 0
    val it = fs2ChunkInput.iterator
    while (it.hasNext) acc += it.next().length
    acc
  }
  @Benchmark def ziochunk(): Int = {
    var acc = 0
    val it = zioChunkInput.iterator
    while (it.hasNext) acc += it.next().length
    acc
  }
}
