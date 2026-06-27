package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Adversarial "deep append-chain" stress: starting from a single element, append one element `size` times,
// THEN .map(_ + 1) to force materialization. A build-only chain is meaningless here: farray's :+ is an O(1)
// lazy tree node, so without a traversal farray "wins" by deferring all work. The trailing .map forces every
// impl to produce a real traversed result, making the comparison fair — farray's O(1)-node build advantage
// shows, but it must still walk its tree.
// IArray and List are O(n) per append => O(n^2) overall — that's expected and the point of this benchmark.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class IntAppendChainBenchmark:
  @Param(Array("1000", "10000")) var size: Int = 1000

  @Benchmark def farray_append(): FArray[Int] =
    var c: FArray[Int] = FArray(1)
    var i = 0
    while i < size do { c = c :+ i; i += 1 }
    c.map(_ + 1)

  @Benchmark def iarray_append(): IArray[Int] =
    var c: IArray[Int] = IArray(1)
    var i = 0
    while i < size do { c = c :+ i; i += 1 }
    c.map(_ + 1)

  @Benchmark def list_append(): List[Int] =
    var c: List[Int] = List(1)
    var i = 0
    while i < size do { c = c :+ i; i += 1 }
    c.map(_ + 1)

  @Benchmark def vector_append(): Vector[Int] =
    var c: Vector[Int] = Vector(1)
    var i = 0
    while i < size do { c = c :+ i; i += 1 }
    c.map(_ + 1)

  @Benchmark def ziochunk_append(): zio.Chunk[Int] =
    var c: zio.Chunk[Int] = zio.Chunk.single(1)
    var i = 0
    while i < size do { c = c :+ i; i += 1 }
    c.map(_ + 1)

  @Benchmark def fs2chunk_append(): fs2.Chunk[Int] =
    var c: fs2.Chunk[Int] = fs2.Chunk.singleton(1)
    var i = 0
    while i < size do { c = c ++ fs2.Chunk.singleton(i); i += 1 }
    c.map(_ + 1)
