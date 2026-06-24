package farray

import org.openjdk.jmh.annotations.Benchmark

// scan / scanLeft / scanRight: running-accumulation, allocate a new collection of length n (+1 for scanLeft/Right).
// FArray exposes none of these (API gap) — only the competitors are measured here so the gap is visible in results.
// fs2.Chunk has scanLeft only (no scan/scanRight). zio.Chunk is an IndexedSeq -> full API.
class IntScanBenchmark extends IntInputs {
  @Benchmark def list_scanLeft(): List[Int] = listInput.scanLeft(0)(_ + _)
  @Benchmark def vector_scanLeft(): Vector[Int] = vectorInput.scanLeft(0)(_ + _)
  @Benchmark def iarray_scanLeft(): IArray[Int] = iarrayInput.scanLeft(0)(_ + _)
  @Benchmark def array_scanLeft(): Array[Int] = arrayInput.scanLeft(0)(_ + _)
  @Benchmark def ziochunk_scanLeft(): zio.Chunk[Int] = zioChunkInput.scanLeft(0)(_ + _)
  @Benchmark def fs2chunk_scanLeft(): fs2.Chunk[Int] = fs2ChunkInput.scanLeft(0)(_ + _)

  @Benchmark def list_scanRight(): List[Int] = listInput.scanRight(0)(_ + _)
  @Benchmark def vector_scanRight(): Vector[Int] = vectorInput.scanRight(0)(_ + _)
  @Benchmark def iarray_scanRight(): IArray[Int] = iarrayInput.scanRight(0)(_ + _)
  @Benchmark def array_scanRight(): Array[Int] = arrayInput.scanRight(0)(_ + _)
  @Benchmark def ziochunk_scanRight(): zio.Chunk[Int] = zioChunkInput.scanRight(0)(_ + _)
  // fs2.Chunk has no scanRight

  @Benchmark def list_scan(): List[Int] = listInput.scan(0)(_ + _)
  @Benchmark def vector_scan(): Vector[Int] = vectorInput.scan(0)(_ + _)
  @Benchmark def iarray_scan(): IArray[Int] = iarrayInput.scan(0)(_ + _)
  @Benchmark def array_scan(): Array[Int] = arrayInput.scan(0)(_ + _)
  @Benchmark def ziochunk_scan(): zio.Chunk[Int] = zioChunkInput.scan(0)(_ + _)
  // fs2.Chunk has no scan
  // FArray has no scan/scanLeft/scanRight (API gap)
}
