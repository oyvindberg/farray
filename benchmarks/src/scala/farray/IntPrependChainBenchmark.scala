package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Adversarial "deep prepend-chain" stress: starting from a single element, prepend one element `size` times.
// List prepend is O(1) — the natural winner; IArray/Vector copy O(n) per prepend.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class IntPrependChainBenchmark:
  @Param(Array("1000", "10000")) var size: Int = 1000

  @Benchmark def farray_prepend(): FArray[Int] =
    var c: FArray[Int] = FArray(1)
    var i = 0
    while i < size do { c = i +: c; i += 1 }
    c

  @Benchmark def iarray_prepend(): IArray[Int] =
    var c: IArray[Int] = IArray(1)
    var i = 0
    while i < size do { c = i +: c; i += 1 }
    c

  @Benchmark def list_prepend(): List[Int] =
    var c: List[Int] = List(1)
    var i = 0
    while i < size do { c = i +: c; i += 1 }
    c

  @Benchmark def vector_prepend(): Vector[Int] =
    var c: Vector[Int] = Vector(1)
    var i = 0
    while i < size do { c = i +: c; i += 1 }
    c

  @Benchmark def ziochunk_prepend(): zio.Chunk[Int] =
    var c: zio.Chunk[Int] = zio.Chunk.single(1)
    var i = 0
    while i < size do { c = i +: c; i += 1 }
    c

  // fs2chunk: no +: (prepend-one)
