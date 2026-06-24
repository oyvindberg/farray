package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Replicates ZIO ChainBenchmarks: build a LARGE deep left-leaning concat structure
// by folding `++` over `numLeaves` array-backed leaves of `leafSize` each, then
// traverse it. Keeps total size moderate (default ~200k) so the run isn't absurd.
//
// For tree-capable impls (farray, ziochunk) the fold builds a deep left-leaning
// Concat tree. For vector/list we hold a flat equivalent (the same elements).
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class IntDeepConcatBenchmark:
  @Param(Array("200")) var numLeaves: Int = 200
  @Param(Array("1000")) var leafSize: Int = 1000

  var farrayInput: FArray[Int] = _
  var ziochunkInput: zio.Chunk[Int] = _
  var fs2chunkInput: fs2.Chunk[Int] = _
  var iarrayInput: IArray[Int] = _
  var vectorInput: Vector[Int] = _
  var listInput: List[Int] = _

  @Setup def setup(): Unit =
    val leafArr = Array.tabulate(leafSize)(i => i)

    val farrayLeaf = FArray.tabulate(leafSize)(i => i)
    farrayInput = (0 until numLeaves).foldLeft(FArray.empty[Int])((acc, _) => acc ++ farrayLeaf)

    val zioLeaf = zio.Chunk.fromArray(leafArr)
    ziochunkInput = (0 until numLeaves).foldLeft(zio.Chunk.empty[Int])((acc, _) => acc ++ zioLeaf)

    val fs2Leaf = fs2.Chunk.array(leafArr)
    fs2chunkInput = (0 until numLeaves).foldLeft(fs2.Chunk.empty[Int])((acc, _) => acc ++ fs2Leaf)

    // iarray: the same fold over ++, but each step is an O(n) flat copy (no tree) — the flat baseline
    val iarrayLeaf: IArray[Int] = IArray.unsafeFromArray(leafArr)
    iarrayInput = (0 until numLeaves).foldLeft(IArray.empty[Int])((acc, _) => acc ++ iarrayLeaf)

    val flat = Vector.tabulate(numLeaves * leafSize)(i => i % leafSize)
    vectorInput = flat
    listInput = flat.toList

  @Benchmark def farray_foldLeft(): Int = farrayInput.foldLeft(0)(_ + _)
  @Benchmark def ziochunk_foldLeft(): Int = ziochunkInput.foldLeft(0)(_ + _)
  @Benchmark def fs2chunk_foldLeft(): Int = fs2chunkInput.foldLeft(0)(_ + _)
  @Benchmark def iarray_foldLeft(): Int = iarrayInput.foldLeft(0)(_ + _)
  @Benchmark def vector_foldLeft(): Int = vectorInput.foldLeft(0)(_ + _)
  @Benchmark def list_foldLeft(): Int = listInput.foldLeft(0)(_ + _)

  @Benchmark def farray_map(): FArray[Int] = farrayInput.map(_ + 1)
  @Benchmark def ziochunk_map(): zio.Chunk[Int] = ziochunkInput.map(_ + 1)
  @Benchmark def fs2chunk_map(): fs2.Chunk[Int] = fs2chunkInput.map(_ + 1)
  @Benchmark def iarray_map(): IArray[Int] = iarrayInput.map(_ + 1)
  @Benchmark def vector_map(): Vector[Int] = vectorInput.map(_ + 1)
  @Benchmark def list_map(): List[Int] = listInput.map(_ + 1)
