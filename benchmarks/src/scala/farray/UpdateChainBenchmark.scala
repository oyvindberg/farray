package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Adversarial update-chain stress: from a flat structure of `size` zeros, `updated(i, i)` for every index.
// Array/IArray copy the whole backing array per update => O(n^2). List.updated is O(n) per update => O(n^2).
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class UpdateChainIntBenchmark:
  @Param(Array("10000")) var size: Int = 10000

  var farrayBase: FArray[Int] = _
  var iarrayBase: IArray[Int] = _
  var listBase: List[Int] = _
  var vectorBase: Vector[Int] = _
  var zioBase: zio.Chunk[Int] = _

  @Setup def setup(): Unit =
    farrayBase = FArray.fill(size)(0)
    iarrayBase = IArray.fill(size)(0)
    listBase = List.fill(size)(0)
    vectorBase = Vector.fill(size)(0)
    zioBase = zio.Chunk.fill(size)(0)

  @Benchmark def farray_update(): FArray[Int] =
    var c = farrayBase; var i = 0
    while i < size do { c = c.updated(i, i); i += 1 }; c

  @Benchmark def iarray_update(): IArray[Int] =
    var c = iarrayBase; var i = 0
    while i < size do { c = c.updated(i, i); i += 1 }; c

  @Benchmark def list_update(): List[Int] =
    var c = listBase; var i = 0
    while i < size do { c = c.updated(i, i); i += 1 }; c

  @Benchmark def vector_update(): Vector[Int] =
    var c = vectorBase; var i = 0
    while i < size do { c = c.updated(i, i); i += 1 }; c

  @Benchmark def ziochunk_update(): zio.Chunk[Int] =
    var c = zioBase; var i = 0
    while i < size do { c = c.updated(i, i); i += 1 }; c

  // fs2chunk: no updated
