package farray

import org.openjdk.jmh.annotations.Benchmark

// foreach = dfsC (push, unboxed for prims); iterator = the flattening cursor (pull, boxes via Iterator[A]).
class IterIntBenchmark extends IntInputs:
  @Benchmark def farray_foreach(): Int  = { var s = 0; farrayInput.foreach(s += _); s }
  @Benchmark def farray_iterator(): Int = { var s = 0; val it = farrayInput.iterator; while it.hasNext do s += it.next(); s }
  @Benchmark def list_iterator(): Int   = { var s = 0; val it = listInput.iterator;   while it.hasNext do s += it.next(); s }
  @Benchmark def array_iterator(): Int  = { var s = 0; val it = arrayInput.iterator;   while it.hasNext do s += it.next(); s }
  @Benchmark def vector_iterator(): Int = { var s = 0; val it = vectorInput.iterator;  while it.hasNext do s += it.next(); s }

class IterStringBenchmark extends Inputs:
  @Benchmark def farray_foreach(): Int  = { var n = 0; farrayInput.foreach(s => n += s.length); n }
  @Benchmark def farray_iterator(): Int = { var n = 0; val it = farrayInput.iterator; while it.hasNext do n += it.next().length; n }
  @Benchmark def list_iterator(): Int   = { var n = 0; val it = listInput.iterator;   while it.hasNext do n += it.next().length; n }
  @Benchmark def vector_iterator(): Int = { var n = 0; val it = vectorInput.iterator;  while it.hasNext do n += it.next().length; n }
