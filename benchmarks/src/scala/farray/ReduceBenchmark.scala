package farray

import org.openjdk.jmh.annotations.Benchmark

// reduce family. fs2.Chunk has none of the reduce family.
// zio.Chunk is an IndexedSeq -> full API.
class ReduceIntBenchmark extends IntInputs {
  @Benchmark def farray_reduce(): Int = farrayInput.reduce(_ + _)
  @Benchmark def list_reduce(): Int = listInput.reduce(_ + _)
  @Benchmark def vector_reduce(): Int = vectorInput.reduce(_ + _)
  @Benchmark def iarray_reduce(): Int = iarrayInput.reduce(_ + _)
  @Benchmark def ziochunk_reduce(): Int = zioChunkInput.reduce(_ + _)

  @Benchmark def farray_reduceLeft(): Int = farrayInput.reduceLeft(_ + _)
  @Benchmark def list_reduceLeft(): Int = listInput.reduceLeft(_ + _)
  @Benchmark def vector_reduceLeft(): Int = vectorInput.reduceLeft(_ + _)
  @Benchmark def iarray_reduceLeft(): Int = iarrayInput.reduceLeft(_ + _)
  @Benchmark def ziochunk_reduceLeft(): Int = zioChunkInput.reduceLeft(_ + _)

  @Benchmark def farray_reduceRight(): Int = farrayInput.reduceRight(_ + _)
  @Benchmark def list_reduceRight(): Int = listInput.reduceRight(_ + _)
  @Benchmark def vector_reduceRight(): Int = vectorInput.reduceRight(_ + _)
  @Benchmark def iarray_reduceRight(): Int = iarrayInput.reduceRight(_ + _)
  @Benchmark def ziochunk_reduceRight(): Int = zioChunkInput.reduceRight(_ + _)

  @Benchmark def farray_reduceOption(): Option[Int] = farrayInput.reduceOption(_ + _)
  @Benchmark def list_reduceOption(): Option[Int] = listInput.reduceOption(_ + _)
  @Benchmark def vector_reduceOption(): Option[Int] = vectorInput.reduceOption(_ + _)
  @Benchmark def iarray_reduceOption(): Option[Int] = iarrayInput.reduceOption(_ + _)
  @Benchmark def ziochunk_reduceOption(): Option[Int] = zioChunkInput.reduceOption(_ + _)

  @Benchmark def farray_reduceLeftOption(): Option[Int] = farrayInput.reduceLeftOption(_ + _)
  @Benchmark def list_reduceLeftOption(): Option[Int] = listInput.reduceLeftOption(_ + _)
  @Benchmark def vector_reduceLeftOption(): Option[Int] = vectorInput.reduceLeftOption(_ + _)
  @Benchmark def iarray_reduceLeftOption(): Option[Int] = iarrayInput.reduceLeftOption(_ + _)
  @Benchmark def ziochunk_reduceLeftOption(): Option[Int] = zioChunkInput.reduceLeftOption(_ + _)

  @Benchmark def farray_reduceRightOption(): Option[Int] = farrayInput.reduceRightOption(_ + _)
  @Benchmark def list_reduceRightOption(): Option[Int] = listInput.reduceRightOption(_ + _)
  @Benchmark def vector_reduceRightOption(): Option[Int] = vectorInput.reduceRightOption(_ + _)
  @Benchmark def iarray_reduceRightOption(): Option[Int] = iarrayInput.reduceRightOption(_ + _)
  @Benchmark def ziochunk_reduceRightOption(): Option[Int] = zioChunkInput.reduceRightOption(_ + _)
  // fs2.Chunk has no reduce family
}
