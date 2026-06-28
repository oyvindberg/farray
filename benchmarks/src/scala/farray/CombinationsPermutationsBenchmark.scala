package farray

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

// combinations / permutations: factorially huge, so use a small fixed n.
// fs2.Chunk has none of these. zio.Chunk is an IndexedSeq -> full API.
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class CombinationsPermutationsIntBenchmark {
  @Param(Array("6", "8"))
  var n: Int = 6

  var farrayInput: FArray[Int] = _
  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var zioChunkInput: zio.Chunk[Int] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(n)(i => i)
    farrayInput = FArray.fromArray(arr.clone())
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.unsafeFromArray(arr.clone())
    zioChunkInput = zio.Chunk.fromArray(arr.clone())
  }

  // combinations(n) taken k = n/2
  @Benchmark def farray_combinations(): Int = { var c = 0; farrayInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def list_combinations(): Int = { var c = 0; listInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def vector_combinations(): Int = { var c = 0; vectorInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def iarray_combinations(): Int = { var c = 0; iarrayInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def ziochunk_combinations(): Int = { var c = 0; zioChunkInput.combinations(n / 2).foreach(_ => c += 1); c }

  // permutations
  @Benchmark def farray_permutations(): Int = { var c = 0; farrayInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def list_permutations(): Int = { var c = 0; listInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def vector_permutations(): Int = { var c = 0; vectorInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def iarray_permutations(): Int = { var c = 0; iarrayInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def ziochunk_permutations(): Int = { var c = 0; zioChunkInput.permutations.foreach(_ => c += 1); c }
}
