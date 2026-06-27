package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Replicates fs2 ChunksBenchmark concat: concatenate `chunkCount` chunks of
// `chunkSize` Ints each into one, THEN .map(_ + 1) to force materialization. fs2 has a dedicated
// `Chunk.concat`; the others reduce via `++`.
// A build-only concat is meaningless here: farray's ++ is an O(1) lazy tree node, so without a traversal
// farray "wins" by deferring all work. The trailing .map forces every impl to produce a real traversed
// result, making the comparison fair — farray's O(1)-node build advantage shows, but it must still walk
// its tree.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class IntConcatNBenchmark:
  @Param(Array("20", "100")) var chunkCount: Int = 20
  @Param(Array("256")) var chunkSize: Int = 256

  var fs2chunks: Seq[fs2.Chunk[Int]] = _
  var ziochunks: Seq[zio.Chunk[Int]] = _
  var farrays: Seq[FArray[Int]] = _
  var iarrays: Seq[IArray[Int]] = _
  var vectors: Seq[Vector[Int]] = _
  var lists: Seq[List[Int]] = _

  @Setup def setup(): Unit =
    fs2chunks = (0 until chunkCount).map(c => fs2.Chunk.array(Array.tabulate(chunkSize)(i => c * chunkSize + i)))
    ziochunks = (0 until chunkCount).map(c => zio.Chunk.fromArray(Array.tabulate(chunkSize)(i => c * chunkSize + i)))
    farrays = (0 until chunkCount).map(c => FArray.tabulate(chunkSize)(i => c * chunkSize + i))
    iarrays = (0 until chunkCount).map(c => IArray.tabulate(chunkSize)(i => c * chunkSize + i))
    vectors = (0 until chunkCount).map(c => Vector.tabulate(chunkSize)(i => c * chunkSize + i))
    lists = (0 until chunkCount).map(c => List.tabulate(chunkSize)(i => c * chunkSize + i))

  @Benchmark def fs2chunk_concat(): fs2.Chunk[Int] = fs2.Chunk.concat(fs2chunks).map(_ + 1)
  @Benchmark def ziochunk_concat(): zio.Chunk[Int] = ziochunks.reduce(_ ++ _).map(_ + 1)
  @Benchmark def farray_concat(): FArray[Int] = farrays.reduce(_ ++ _).map(_ + 1)
  // iarray/array reduce via repeated ++ (O(n^2) flat copies) — the baseline FArray's O(1) concat beats
  @Benchmark def iarray_concat(): IArray[Int] = iarrays.reduce(_ ++ _).map(_ + 1)
  @Benchmark def vector_concat(): Vector[Int] = vectors.reduce(_ ++ _).map(_ + 1)
  @Benchmark def list_concat(): List[Int] = lists.reduce(_ ++ _).map(_ + 1)
