package farray

import org.openjdk.jmh.annotations.{Benchmark, Setup}

// Search / compare ops. FArray exposes startsWith / endsWith only; it has NONE of
// segmentLength / lastIndexWhere / sameElements / indexOfSlice / lastIndexOf / lastIndexOfSlice / containsSlice (API gap).
// fs2.Chunk has startsWith only. zio.Chunk is an IndexedSeq -> full API.
//
// The slice-search ops need a needle. We build a 3-element needle drawn from the middle of the haystack,
// plus a full-copy "same" sequence for sameElements, in extra setup.
class IntSearchSliceBenchmark extends IntInputs {
  // needle (a small slice taken from the middle), one per impl. FArray/fs2 have no slice-search ops,
  // so only the stdlib + zio needles are used below.
  var listSlice: List[Int] = _
  var vectorSlice: Vector[Int] = _
  var iarraySlice: IArray[Int] = _
  var zioSlice: zio.Chunk[Int] = _

  // full-length copies for sameElements
  var listSame: List[Int] = _
  var vectorSame: Vector[Int] = _
  var iarraySame: IArray[Int] = _
  var zioSame: zio.Chunk[Int] = _

  @Setup
  def setupSlices(): Unit = {
    val mid = size / 2
    val sliceArr = Array.tabulate(math.min(3, size))(i => mid + i)
    listSlice = sliceArr.toList
    vectorSlice = sliceArr.toVector
    iarraySlice = IArray.unsafeFromArray(sliceArr.clone())
    zioSlice = zio.Chunk.fromArray(sliceArr.clone())

    val sameArr = Array.tabulate(size)(i => i)
    listSame = sameArr.toList
    vectorSame = sameArr.toVector
    iarraySame = IArray.unsafeFromArray(sameArr.clone())
    zioSame = zio.Chunk.fromArray(sameArr.clone())
  }

  // startsWith (FArray + fs2.Chunk + zio.Chunk + stdlib)
  @Benchmark def farray_startsWith(): Boolean = farrayInput.startsWith(FArray.tabulate(3)(i => i))
  @Benchmark def list_startsWith(): Boolean = listInput.startsWith(List(0, 1, 2))
  @Benchmark def vector_startsWith(): Boolean = vectorInput.startsWith(Vector(0, 1, 2))
  @Benchmark def iarray_startsWith(): Boolean = iarrayInput.startsWith(IArray(0, 1, 2))
  @Benchmark def ziochunk_startsWith(): Boolean = zioChunkInput.startsWith(zio.Chunk(0, 1, 2))
  @Benchmark def fs2chunk_startsWith(): Boolean = fs2ChunkInput.startsWith(fs2.Chunk(0, 1, 2))

  // endsWith: FArray has it; fs2.Chunk does not. zio.Chunk + stdlib do.
  @Benchmark def farray_endsWith(): Boolean = farrayInput.endsWith(FArray.tabulate(3)(i => size - 3 + i))
  @Benchmark def list_endsWith(): Boolean = listInput.endsWith(List(size - 3, size - 2, size - 1))
  @Benchmark def vector_endsWith(): Boolean = vectorInput.endsWith(Vector(size - 3, size - 2, size - 1))
  @Benchmark def iarray_endsWith(): Boolean = iarrayInput.endsWith(IArray(size - 3, size - 2, size - 1))
  @Benchmark def ziochunk_endsWith(): Boolean = zioChunkInput.endsWith(zio.Chunk(size - 3, size - 2, size - 1))

  // segmentLength: FArray has none (API gap)
  @Benchmark def list_segmentLength(): Int = listInput.segmentLength(_ < size / 2)
  @Benchmark def vector_segmentLength(): Int = vectorInput.segmentLength(_ < size / 2)
  @Benchmark def iarray_segmentLength(): Int = iarrayInput.segmentLength(_ < size / 2)
  @Benchmark def ziochunk_segmentLength(): Int = zioChunkInput.segmentLength(_ < size / 2)

  // lastIndexWhere: FArray has none (API gap)
  @Benchmark def list_lastIndexWhere(): Int = listInput.lastIndexWhere(_ == 5)
  @Benchmark def vector_lastIndexWhere(): Int = vectorInput.lastIndexWhere(_ == 5)
  @Benchmark def iarray_lastIndexWhere(): Int = iarrayInput.lastIndexWhere(_ == 5)
  @Benchmark def ziochunk_lastIndexWhere(): Int = zioChunkInput.lastIndexWhere(_ == 5)

  // sameElements: FArray has none (has corresponds; API gap for sameElements)
  @Benchmark def list_sameElements(): Boolean = listInput.sameElements(listSame)
  @Benchmark def vector_sameElements(): Boolean = vectorInput.sameElements(vectorSame)
  @Benchmark def iarray_sameElements(): Boolean = iarrayInput.sameElements(iarraySame)
  @Benchmark def ziochunk_sameElements(): Boolean = zioChunkInput.sameElements(zioSame)

  // indexOfSlice: FArray has none (API gap)
  @Benchmark def list_indexOfSlice(): Int = listInput.indexOfSlice(listSlice)
  @Benchmark def vector_indexOfSlice(): Int = vectorInput.indexOfSlice(vectorSlice)
  @Benchmark def iarray_indexOfSlice(): Int = iarrayInput.indexOfSlice(iarraySlice)
  @Benchmark def ziochunk_indexOfSlice(): Int = zioChunkInput.indexOfSlice(zioSlice)

  // lastIndexOf: FArray has none (API gap)
  @Benchmark def list_lastIndexOf(): Int = listInput.lastIndexOf(5)
  @Benchmark def vector_lastIndexOf(): Int = vectorInput.lastIndexOf(5)
  @Benchmark def iarray_lastIndexOf(): Int = iarrayInput.lastIndexOf(5)
  @Benchmark def ziochunk_lastIndexOf(): Int = zioChunkInput.lastIndexOf(5)

  // lastIndexOfSlice: FArray has none (API gap)
  @Benchmark def list_lastIndexOfSlice(): Int = listInput.lastIndexOfSlice(listSlice)
  @Benchmark def vector_lastIndexOfSlice(): Int = vectorInput.lastIndexOfSlice(vectorSlice)
  @Benchmark def iarray_lastIndexOfSlice(): Int = iarrayInput.lastIndexOfSlice(iarraySlice)
  @Benchmark def ziochunk_lastIndexOfSlice(): Int = zioChunkInput.lastIndexOfSlice(zioSlice)

  // containsSlice: FArray has none (API gap)
  @Benchmark def list_containsSlice(): Boolean = listInput.containsSlice(listSlice)
  @Benchmark def vector_containsSlice(): Boolean = vectorInput.containsSlice(vectorSlice)
  @Benchmark def iarray_containsSlice(): Boolean = iarrayInput.containsSlice(iarraySlice)
  @Benchmark def ziochunk_containsSlice(): Boolean = zioChunkInput.containsSlice(zioSlice)
}
