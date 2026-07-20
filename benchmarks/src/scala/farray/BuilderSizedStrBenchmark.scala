package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// The FAIR sizeHint comparison: same as BuilderIntBenchmark, but EVERY builder is told the size up
// front via sizeHint(size) before the loop — so pre-sizing is measured on equal terms (Vector/List
// builders can't meaningfully pre-size, and that shows honestly). String (reference) kind.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class BuilderSizedStrBenchmark:
  @Param(Array("10", "1000", "100000"))
  var size: Int = 1000
  var src: Array[String] = _

  @Setup def setup(): Unit =
    src = Array.tabulate(size)(i => (i * 2 + 1).toString)

  @Benchmark def farray(): FArray[String] =
    val b = FArray.newBuilder[String](size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def arraybuffer(): Array[String] =
    val b = new scala.collection.mutable.ArrayBuffer[String]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.toArray

  @Benchmark def arraybuilder(): Array[String] =
    val b = scala.collection.mutable.ArrayBuilder.make[String]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def iarray(): IArray[String] =
    val b = IArray.newBuilder[String]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def vector(): Vector[String] =
    val b = Vector.newBuilder[String]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def list(): List[String] =
    val b = List.newBuilder[String]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  // fs2.Chunk has no element-wise builder (Collector.Builder appends whole Chunks only)

  @Benchmark def ziochunk(): zio.Chunk[String] =
    val b = zio.ChunkBuilder.make[String](); b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def kyochunk(): kyo.Chunk[String] =
    val b = kyo.ChunkBuilder.init[String]; b.sizeHint(size)
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()
