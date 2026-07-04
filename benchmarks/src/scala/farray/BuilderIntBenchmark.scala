package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Imperative "accumulate N elements one at a time, then get the collection" cost, Int (primitive) kind.
// The source array is filled in @Setup so the JIT can't constant-fold the pushes; each benchmark reads
// src(i) and appends. FArray.tabulate is the known-N (no per-element append) baseline.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class BuilderIntBenchmark:
  @Param(Array("10", "1000", "100000"))
  var size: Int = 1000
  var src: Array[Int] = _

  @Setup def setup(): Unit =
    src = Array.tabulate(size)(i => i * 2 + 1)

  // ---- FArray: unboxed inline += ----
  @Benchmark def farray(): FArray[Int] =
    val b = FArray.newBuilder[Int]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  // ---- rivals ----
  @Benchmark def arraybuffer(): scala.collection.mutable.ArrayBuffer[Int] =
    val b = new scala.collection.mutable.ArrayBuffer[Int]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b

  @Benchmark def arraybuilder(): Array[Int] =
    val b = scala.collection.mutable.ArrayBuilder.make[Int]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def vector(): Vector[Int] =
    val b = Vector.newBuilder[Int]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def list(): List[Int] =
    val b = List.newBuilder[Int]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  // zio's specialized primitive builder (int[]-backed, held at the concrete type so += is unboxed).
  @Benchmark def ziochunk(): zio.Chunk[Int] =
    val b = new zio.ChunkBuilder.Int
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()
