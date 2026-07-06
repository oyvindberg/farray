package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// The FAIR sizeHint comparison: same as BuilderIntBenchmark, but EVERY builder is told the size up
// front via sizeHint(size) before the loop — so pre-sizing is measured on equal terms (Vector/List
// builders can't meaningfully pre-size, and that shows honestly). Int (primitive) kind.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class BuilderSizedIntBenchmark:
  @Param(Array("10", "1000", "100000"))
  var size: Int = 1000
  var src: Array[Int] = _

  @Setup def setup(): Unit =
    src = Array.tabulate(size)(i => i * 2 + 1)

  @Benchmark def farray(): FArray[Int] =
    val b = FArray.newBuilder[Int](size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def arraybuffer(): Array[Int] =
    val b = new scala.collection.mutable.ArrayBuffer[Int]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.toArray

  @Benchmark def arraybuilder(): Array[Int] =
    val b = scala.collection.mutable.ArrayBuilder.make[Int]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def iarray(): IArray[Int] =
    val b = IArray.newBuilder[Int]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def vector(): Vector[Int] =
    val b = Vector.newBuilder[Int]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def list(): List[Int] =
    val b = List.newBuilder[Int]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  // fs2.Chunk has no element-wise builder (Collector.Builder appends whole Chunks only)

  @Benchmark def ziochunk(): zio.Chunk[Int] =
    val b = new zio.ChunkBuilder.Int; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()
