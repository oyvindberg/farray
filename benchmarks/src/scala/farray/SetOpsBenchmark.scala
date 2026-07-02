package farray

import org.openjdk.jmh.annotations.{Benchmark, Setup}

// Set ops: diff / intersect / toSet / groupBy. FArray exposes all four.
// fs2.Chunk has none of them. zio.Chunk is an IndexedSeq -> full API.
// diff/intersect need a "that" — we use the even numbers in range as the second operand.
class SetOpsIntBenchmark extends IntInputs {
  var farrayThat: FArray[Int] = _
  var listThat: List[Int] = _
  var vectorThat: Vector[Int] = _
  var iarrayThat: IArray[Int] = _
  var zioThat: zio.Chunk[Int] = _

  @Setup
  def setupThat(): Unit = {
    val thatArr = Array.tabulate(size)(i => i * 2) // overlaps the lower half of [0, size)
    farrayThat = FArray.fromArray(thatArr.clone())
    listThat = thatArr.toList
    vectorThat = thatArr.toVector
    iarrayThat = IArray.unsafeFromArray(thatArr.clone())
    zioThat = zio.Chunk.fromArray(thatArr.clone())
  }

  // distinct on the base input (dense 0..n-1, ALL distinct — the identity-return + bitmap showcase) and
  // on a dup-heavy variant (75% duplicates, values in [0, size/4) — the rebuild path).
  var farrayDups: FArray[Int] = _
  var listDups: List[Int] = _
  var vectorDups: Vector[Int] = _
  var iarrayDups: IArray[Int] = _
  var zioDups: zio.Chunk[Int] = _

  @Setup
  def setupDups(): Unit = {
    val m = math.max(size / 4, 1)
    val dupArr = Array.tabulate(size)(i => (i * 31) % m)
    farrayDups = FArray.fromArray(dupArr.clone())
    listDups = dupArr.toList
    vectorDups = dupArr.toVector
    iarrayDups = IArray.unsafeFromArray(dupArr.clone())
    zioDups = zio.Chunk.fromArray(dupArr.clone())
  }

  @Benchmark def farray_distinct(): FArray[Int] = farrayInput.distinct
  @Benchmark def list_distinct(): List[Int] = listInput.distinct
  @Benchmark def vector_distinct(): Vector[Int] = vectorInput.distinct
  @Benchmark def iarray_distinct(): IArray[Int] = iarrayInput.distinct
  @Benchmark def ziochunk_distinct(): zio.Chunk[Int] = zioChunkInput.distinct

  @Benchmark def farray_distinctDups(): FArray[Int] = farrayDups.distinct
  @Benchmark def list_distinctDups(): List[Int] = listDups.distinct
  @Benchmark def vector_distinctDups(): Vector[Int] = vectorDups.distinct
  @Benchmark def iarray_distinctDups(): IArray[Int] = iarrayDups.distinct
  @Benchmark def ziochunk_distinctDups(): zio.Chunk[Int] = zioDups.distinct

  @Benchmark def farray_diff(): FArray[Int] = farrayInput.diff(farrayThat)
  @Benchmark def list_diff(): List[Int] = listInput.diff(listThat)
  @Benchmark def vector_diff(): Vector[Int] = vectorInput.diff(vectorThat)
  @Benchmark def iarray_diff(): IArray[Int] = iarrayInput.diff(iarrayThat)
  @Benchmark def ziochunk_diff(): zio.Chunk[Int] = zioChunkInput.diff(zioThat)

  @Benchmark def farray_intersect(): FArray[Int] = farrayInput.intersect(farrayThat)
  @Benchmark def list_intersect(): List[Int] = listInput.intersect(listThat)
  @Benchmark def vector_intersect(): Vector[Int] = vectorInput.intersect(vectorThat)
  @Benchmark def iarray_intersect(): IArray[Int] = iarrayInput.intersect(iarrayThat)
  @Benchmark def ziochunk_intersect(): zio.Chunk[Int] = zioChunkInput.intersect(zioThat)

  @Benchmark def farray_toSet(): Set[Int] = farrayInput.toSet
  @Benchmark def list_toSet(): Set[Int] = listInput.toSet
  @Benchmark def vector_toSet(): Set[Int] = vectorInput.toSet
  @Benchmark def iarray_toSet(): Set[Int] = iarrayInput.toSet
  @Benchmark def ziochunk_toSet(): Set[Int] = zioChunkInput.toSet

  @Benchmark def farray_groupBy(): Map[Int, FArray[Int]] = farrayInput.groupBy(_ % 8)
  @Benchmark def list_groupBy(): Map[Int, List[Int]] = listInput.groupBy(_ % 8)
  @Benchmark def vector_groupBy(): Map[Int, Vector[Int]] = vectorInput.groupBy(_ % 8)
  @Benchmark def iarray_groupBy(): Map[Int, IArray[Int]] = iarrayInput.groupBy(_ % 8)
  @Benchmark def ziochunk_groupBy(): Map[Int, zio.Chunk[Int]] = zioChunkInput.groupBy(_ % 8)
}
