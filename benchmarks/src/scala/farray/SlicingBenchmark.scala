package farray

import org.openjdk.jmh.annotations.Benchmark

// partition / span / splitAt / distinct, plus grouped / sliding.
// fs2.Chunk has splitAt only (no partition/span/distinct/grouped/sliding). zio.Chunk -> full IndexedSeq API.
class IntSlicingBenchmark extends IntInputs {
  @Benchmark def farray_partition(): (FArray[Int], FArray[Int]) = farrayInput.partition(_ % 2 == 0)
  @Benchmark def list_partition(): (List[Int], List[Int]) = listInput.partition(_ % 2 == 0)
  @Benchmark def vector_partition(): (Vector[Int], Vector[Int]) = vectorInput.partition(_ % 2 == 0)
  @Benchmark def iarray_partition(): (IArray[Int], IArray[Int]) = iarrayInput.partition(_ % 2 == 0)
  @Benchmark def ziochunk_partition(): (zio.Chunk[Int], zio.Chunk[Int]) = zioChunkInput.partition(_ % 2 == 0)

  @Benchmark def farray_span(): (FArray[Int], FArray[Int]) = farrayInput.span(_ < size / 2)
  @Benchmark def list_span(): (List[Int], List[Int]) = listInput.span(_ < size / 2)
  @Benchmark def vector_span(): (Vector[Int], Vector[Int]) = vectorInput.span(_ < size / 2)
  @Benchmark def iarray_span(): (IArray[Int], IArray[Int]) = iarrayInput.span(_ < size / 2)
  @Benchmark def ziochunk_span(): (zio.Chunk[Int], zio.Chunk[Int]) = zioChunkInput.span(_ < size / 2)

  @Benchmark def farray_splitAt(): (FArray[Int], FArray[Int]) = farrayInput.splitAt(size / 2)
  @Benchmark def list_splitAt(): (List[Int], List[Int]) = listInput.splitAt(size / 2)
  @Benchmark def vector_splitAt(): (Vector[Int], Vector[Int]) = vectorInput.splitAt(size / 2)
  @Benchmark def iarray_splitAt(): (IArray[Int], IArray[Int]) = iarrayInput.splitAt(size / 2)
  @Benchmark def ziochunk_splitAt(): (zio.Chunk[Int], zio.Chunk[Int]) = zioChunkInput.splitAt(size / 2)
  @Benchmark def fs2chunk_splitAt(): (fs2.Chunk[Int], fs2.Chunk[Int]) = fs2ChunkInput.splitAt(size / 2)

  @Benchmark def farray_distinct(): FArray[Int] = farrayInput.distinct
  @Benchmark def list_distinct(): List[Int] = listInput.distinct
  @Benchmark def vector_distinct(): Vector[Int] = vectorInput.distinct
  @Benchmark def iarray_distinct(): IArray[Int] = iarrayInput.distinct
  @Benchmark def ziochunk_distinct(): zio.Chunk[Int] = zioChunkInput.distinct

  // grouped(size/2): FArray has no grouped (API gap)
  @Benchmark def list_grouped(): Int = { var n = 0; listInput.grouped(math.max(1, size / 2)).foreach(_ => n += 1); n }
  @Benchmark def vector_grouped(): Int = { var n = 0; vectorInput.grouped(math.max(1, size / 2)).foreach(_ => n += 1); n }
  @Benchmark def iarray_grouped(): Int = { var n = 0; iarrayInput.grouped(math.max(1, size / 2)).foreach(_ => n += 1); n }
  @Benchmark def ziochunk_grouped(): Int = { var n = 0; zioChunkInput.grouped(math.max(1, size / 2)).foreach(_ => n += 1); n }

  // sliding(2, 3): FArray has no sliding (API gap)
  @Benchmark def list_sliding(): Int = { var n = 0; listInput.sliding(2, 3).foreach(_ => n += 1); n }
  @Benchmark def vector_sliding(): Int = { var n = 0; vectorInput.sliding(2, 3).foreach(_ => n += 1); n }
  @Benchmark def iarray_sliding(): Int = { var n = 0; iarrayInput.sliding(2, 3).foreach(_ => n += 1); n }
  @Benchmark def ziochunk_sliding(): Int = { var n = 0; zioChunkInput.sliding(2, 3).foreach(_ => n += 1); n }
}
