package farray

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Param, Setup, Warmup}
import java.util.concurrent.TimeUnit

// The SAME 14-stage pipeline as LongMixedPipelineIntBenchmark, measured COLD: SingleShotTime with ZERO
// warmup, so each fork times a single uncompiled invocation while the JVM is still interpreting. This
// backs the site's cold-start claim — FArray is ahead before the JIT devirtualizes or escape-analyzes
// anything, and only widens the gap as it warms.
//
// Self-contained (own @Param, inputs, and method bodies) rather than a subclass of the warm benchmark,
// because it must be PINNED to size=100000: single-shot at tiny sizes is dominated by noise / dead-code
// elimination, and Scala won't let a subclass narrow the inherited 7-value @Param (a `var` can't be
// overridden). Keep the method bodies in sync with LongMixedPipelineIntBenchmark — they are a verbatim
// copy, minus the site's snippet-extraction marker (which stays unique to the warm benchmark).
@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@Fork(
  value = 12,
  jvmArgs = Array(
    "-Xms2g", "-Xmx2g", "-XX:NewSize=1g", "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m", "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC", "-XX:-UseAdaptiveSizePolicy",
    "-XX:MaxInlineLevel=20", "-XX:InlineSmallCode=1500", "-XX:+AlwaysPreTouch"
  )
)
class ColdPipelineIntBenchmark extends CommonParams {

  @Param(Array("100000"))
  var size: Int = 100000

  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var farrayInput: FArray[Int] = _
  var fs2ChunkInput: fs2.Chunk[Int] = _
  var zioChunkInput: zio.Chunk[Int] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(size)(i => i)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.tabulate(size)(i => i)
    farrayInput = FArray.tabulate(size)(i => i)
    fs2ChunkInput = fs2.Chunk.array(arr)
    zioChunkInput = zio.Chunk.fromArray(arr)
  }

  @Benchmark def list(): Int =
    listInput
      .flatMap(x => List(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => List(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(listInput.map(_ + 100)).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => List(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)

  @Benchmark def vector(): Int =
    vectorInput
      .flatMap(x => Vector(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => Vector(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(vectorInput.map(_ + 100)).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => Vector(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)

  @Benchmark def iarray(): Int =
    iarrayInput
      .flatMap(x => IArray(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => IArray(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(iarrayInput.map(_ + 100)).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => IArray(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)

  @Benchmark def farrayEager(): Int =
    farrayInput
      .flatMap(x => FArray(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => FArray(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(farrayInput.map(_ + 100)).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => FArray(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)

  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(_ + 100)
    farrayInput.fuse
      .flatMap(x => FArray(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => FArray(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(zipSrc).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => FArray(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)
  }

  @Benchmark def fs2chunk(): Int =
    fs2ChunkInput
      .flatMap(x => fs2.Chunk(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => fs2.Chunk(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(fs2ChunkInput.map(_ + 100)).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => fs2.Chunk(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)

  @Benchmark def ziochunk(): Int =
    zioChunkInput
      .flatMap(x => zio.Chunk(x, x + 1)).filter(_ % 3 != 0).map(_ * 2)
      .flatMap(x => zio.Chunk(x, x ^ 5)).filter(_ % 2 == 0).map(_ - 7)
      .zip(zioChunkInput.map(_ + 100)).map((a, b) => a + b)
      .zipWithIndex.filter((v, i) => (v + i) % 4 != 0).map((v, i) => v - i)
      .flatMap(x => zio.Chunk(x, x + 3)).filter(_ > 0).foldLeft(0)(_ + _)
}
