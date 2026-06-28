package farray

import org.openjdk.jmh.annotations._

// inits / tails / patch. inits/tails are O(n^2) so we count the iterator.
// fs2.Chunk has none of these. zio.Chunk is an IndexedSeq -> full API.
class InitsTailsPatchIntBenchmark extends IntInputs {
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

  @Benchmark def farray_inits(): Int = { var n = 0; farrayInput.inits.foreach(_ => n += 1); n }
  @Benchmark def list_inits(): Int = { var n = 0; listInput.inits.foreach(_ => n += 1); n }
  @Benchmark def vector_inits(): Int = { var n = 0; vectorInput.inits.foreach(_ => n += 1); n }
  @Benchmark def iarray_inits(): Int = { var n = 0; iarrayInput.inits.foreach(_ => n += 1); n }
  @Benchmark def ziochunk_inits(): Int = { var n = 0; zioChunkInput.inits.foreach(_ => n += 1); n }

  @Benchmark def farray_tails(): Int = { var n = 0; farrayInput.tails.foreach(_ => n += 1); n }
  @Benchmark def list_tails(): Int = { var n = 0; listInput.tails.foreach(_ => n += 1); n }
  @Benchmark def vector_tails(): Int = { var n = 0; vectorInput.tails.foreach(_ => n += 1); n }
  @Benchmark def iarray_tails(): Int = { var n = 0; iarrayInput.tails.foreach(_ => n += 1); n }
  @Benchmark def ziochunk_tails(): Int = { var n = 0; zioChunkInput.tails.foreach(_ => n += 1); n }

  @Benchmark def farray_patch(): FArray[Int] = farrayInput.patch(size / 2, farrayPatch, 4)
  @Benchmark def list_patch(): List[Int] = listInput.patch(size / 2, listPatch, 4)
  @Benchmark def vector_patch(): Vector[Int] = vectorInput.patch(size / 2, vectorPatch, 4)
  @Benchmark def iarray_patch(): IArray[Int] = iarrayInput.patch(size / 2, iarrayPatch, 4)
  @Benchmark def ziochunk_patch(): zio.Chunk[Int] = zioChunkInput.patch(size / 2, zioPatch, 4)
}
