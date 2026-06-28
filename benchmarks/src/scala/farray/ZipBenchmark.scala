package farray

import org.openjdk.jmh.annotations.Benchmark

class ZipIntBenchmark extends IntInputs:
  @Benchmark def farray_zip(): FArray[(Int, Int)] = farrayInput.zip(farrayInput)
  @Benchmark def iarray_zip(): IArray[(Int, Int)] = iarrayInput.zip(iarrayInput)
  @Benchmark def list_zip(): List[(Int, Int)] = listInput.zip(listInput)
  @Benchmark def vector_zip(): Vector[(Int, Int)] = vectorInput.zip(vectorInput)
  @Benchmark def fs2chunk_zip(): fs2.Chunk[(Int, Int)] = fs2ChunkInput.zip(fs2ChunkInput)
  @Benchmark def ziochunk_zip(): zio.Chunk[(Int, Int)] = zioChunkInput.zip(zioChunkInput)
  @Benchmark def farray_zipWithIndex(): FArray[(Int, Int)] = farrayInput.zipWithIndex
  @Benchmark def iarray_zipWithIndex(): IArray[(Int, Int)] = iarrayInput.zipWithIndex
  @Benchmark def list_zipWithIndex(): List[(Int, Int)] = listInput.zipWithIndex
  @Benchmark def vector_zipWithIndex(): Vector[(Int, Int)] = vectorInput.zipWithIndex
  @Benchmark def fs2chunk_zipWithIndex(): fs2.Chunk[(Int, Int)] = fs2ChunkInput.zipWithIndex
  @Benchmark def ziochunk_zipWithIndex(): zio.Chunk[(Int, Int)] = zioChunkInput.zipWithIndex

class ZipStrBenchmark extends Inputs:
  @Benchmark def farray_zip(): FArray[(String, String)] = farrayInput.zip(farrayInput)
  @Benchmark def list_zip(): List[(String, String)] = listInput.zip(listInput)
  @Benchmark def iarray_zip(): IArray[(String, String)] = iarrayInput.zip(iarrayInput)
  @Benchmark def vector_zip(): Vector[(String, String)] = vectorInput.zip(vectorInput)
  @Benchmark def fs2chunk_zip(): fs2.Chunk[(String, String)] = fs2ChunkInput.zip(fs2ChunkInput)
  @Benchmark def ziochunk_zip(): zio.Chunk[(String, String)] = zioChunkInput.zip(zioChunkInput)
  @Benchmark def farray_zipWithIndex(): FArray[(String, Int)] = farrayInput.zipWithIndex
  @Benchmark def list_zipWithIndex(): List[(String, Int)] = listInput.zipWithIndex
  @Benchmark def iarray_zipWithIndex(): IArray[(String, Int)] = iarrayInput.zipWithIndex
  @Benchmark def vector_zipWithIndex(): Vector[(String, Int)] = vectorInput.zipWithIndex
  @Benchmark def fs2chunk_zipWithIndex(): fs2.Chunk[(String, Int)] = fs2ChunkInput.zipWithIndex
  @Benchmark def ziochunk_zipWithIndex(): zio.Chunk[(String, Int)] = zioChunkInput.zipWithIndex
