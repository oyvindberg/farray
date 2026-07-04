package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Same builder shootout for a REFERENCE element kind (String): here no competitor boxes elements, so this
// isolates the builder's structural overhead (grow strategy, result() slack) from the primitive-boxing win.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class BuilderStrBenchmark:
  @Param(Array("10", "1000", "100000"))
  var size: Int = 1000
  var src: Array[String] = _

  @Setup def setup(): Unit =
    src = Array.tabulate(size)(i => s"e$i")

  @Benchmark def farray(): FArray[String] =
    val b = FArray.newBuilder[String]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()



  @Benchmark def arraybuffer(): scala.collection.mutable.ArrayBuffer[String] =
    val b = new scala.collection.mutable.ArrayBuffer[String]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b

  @Benchmark def arraybuilder(): Array[String] =
    val b = scala.collection.mutable.ArrayBuilder.make[String]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def vector(): Vector[String] =
    val b = Vector.newBuilder[String]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def list(): List[String] =
    val b = List.newBuilder[String]
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()

  @Benchmark def ziochunk(): zio.Chunk[String] =
    val b = zio.ChunkBuilder.make[String]()
    var i = 0; val n = size
    while i < n do { b += src(i); i += 1 }
    b.result()
