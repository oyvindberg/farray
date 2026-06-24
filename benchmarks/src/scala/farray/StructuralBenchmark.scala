package farray

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

// Structural ops: transpose / inits / tails / patch / combinations / permutations.
// FArray exposes transpose ONLY; it has none of inits / tails / patch / combinations / permutations (API gap).
// fs2.Chunk has none of these. zio.Chunk is an IndexedSeq -> full API.

// --- transpose: a rows x cols matrix of Ints; transpose flips it. ---
class IntTransposeBenchmark extends IntInputs {
  var farrayMatrix: FArray[FArray[Int]] = _
  var listMatrix: List[List[Int]] = _
  var vectorMatrix: Vector[Vector[Int]] = _
  var zioMatrix: zio.Chunk[zio.Chunk[Int]] = _

  @Setup
  def setupMatrix(): Unit = {
    val rows = math.max(1, math.min(size, 64))
    val cols = math.max(1, size / rows)
    farrayMatrix = FArray.tabulate(rows)(r => FArray.tabulate(cols)(c => r * cols + c))
    listMatrix = List.tabulate(rows)(r => List.tabulate(cols)(c => r * cols + c))
    vectorMatrix = Vector.tabulate(rows)(r => Vector.tabulate(cols)(c => r * cols + c))
    zioMatrix = zio.Chunk.fromIterable((0 until rows).map(r => zio.Chunk.fromIterable((0 until cols).map(c => r * cols + c))))
  }

  @Benchmark def farray_transpose(): FArray[FArray[Int]] = farrayMatrix.transpose
  @Benchmark def list_transpose(): List[List[Int]] = listMatrix.transpose
  @Benchmark def vector_transpose(): Vector[Vector[Int]] = vectorMatrix.transpose
  @Benchmark def ziochunk_transpose(): zio.Chunk[zio.Chunk[Int]] = zioMatrix.transpose
}

// --- inits / tails / patch: FArray has none (API gap). inits/tails are O(n^2) so we count the iterator. ---
class IntInitsTailsPatchBenchmark extends IntInputs {
  var farrayPatch: FArray[Int] = _
  var listPatch: List[Int] = _
  var vectorPatch: Vector[Int] = _
  var iarrayPatch: IArray[Int] = _
  var zioPatch: zio.Chunk[Int] = _

  @Setup
  def setupPatch(): Unit = {
    val p = Array.tabulate(math.min(8, size))(i => -i)
    farrayPatch = FArray.fromArray(p.clone())
    listPatch = p.toList
    vectorPatch = p.toVector
    iarrayPatch = IArray.unsafeFromArray(p.clone())
    zioPatch = zio.Chunk.fromArray(p.clone())
  }

  // inits: FArray has none (API gap)
  @Benchmark def list_inits(): Int = { var n = 0; listInput.inits.foreach(_ => n += 1); n }
  @Benchmark def vector_inits(): Int = { var n = 0; vectorInput.inits.foreach(_ => n += 1); n }
  @Benchmark def iarray_inits(): Int = { var n = 0; iarrayInput.inits.foreach(_ => n += 1); n }
  @Benchmark def ziochunk_inits(): Int = { var n = 0; zioChunkInput.inits.foreach(_ => n += 1); n }

  // tails: FArray has none (API gap)
  @Benchmark def list_tails(): Int = { var n = 0; listInput.tails.foreach(_ => n += 1); n }
  @Benchmark def vector_tails(): Int = { var n = 0; vectorInput.tails.foreach(_ => n += 1); n }
  @Benchmark def iarray_tails(): Int = { var n = 0; iarrayInput.tails.foreach(_ => n += 1); n }
  @Benchmark def ziochunk_tails(): Int = { var n = 0; zioChunkInput.tails.foreach(_ => n += 1); n }

  // patch: FArray has none (API gap)
  @Benchmark def list_patch(): List[Int] = listInput.patch(size / 2, listPatch, 4)
  @Benchmark def vector_patch(): Vector[Int] = vectorInput.patch(size / 2, vectorPatch, 4)
  @Benchmark def iarray_patch(): IArray[Int] = iarrayInput.patch(size / 2, iarrayPatch, 4)
  @Benchmark def ziochunk_patch(): zio.Chunk[Int] = zioChunkInput.patch(size / 2, zioPatch, 4)
}

// --- combinations / permutations: factorially huge, so use a small fixed n. FArray has neither (API gap). ---
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntCombinationsPermutationsBenchmark {
  @Param(Array("6", "8"))
  var n: Int = 6

  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _
  var iarrayInput: IArray[Int] = _
  var zioChunkInput: zio.Chunk[Int] = _

  @Setup
  def setup(): Unit = {
    val arr = Array.tabulate(n)(i => i)
    listInput = arr.toList
    vectorInput = arr.toVector
    iarrayInput = IArray.unsafeFromArray(arr.clone())
    zioChunkInput = zio.Chunk.fromArray(arr.clone())
  }

  // combinations(n) taken k = n/2
  @Benchmark def list_combinations(): Int = { var c = 0; listInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def vector_combinations(): Int = { var c = 0; vectorInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def iarray_combinations(): Int = { var c = 0; iarrayInput.combinations(n / 2).foreach(_ => c += 1); c }
  @Benchmark def ziochunk_combinations(): Int = { var c = 0; zioChunkInput.combinations(n / 2).foreach(_ => c += 1); c }

  // permutations
  @Benchmark def list_permutations(): Int = { var c = 0; listInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def vector_permutations(): Int = { var c = 0; vectorInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def iarray_permutations(): Int = { var c = 0; iarrayInput.permutations.foreach(_ => c += 1); c }
  @Benchmark def ziochunk_permutations(): Int = { var c = 0; zioChunkInput.permutations.foreach(_ => c += 1); c }
}
