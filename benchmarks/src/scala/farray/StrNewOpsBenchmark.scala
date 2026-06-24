package farray

import org.openjdk.jmh.annotations.Benchmark

// Reference-element (String) variant of a representative slice of the new ops, so the boxed / Object[]
// array path is measured alongside the primitive-Int path. Mirrors families: reduce, maxBy/minBy,
// distinct, groupBy, toSet, mkString, toArray. IArray is the raw-array baseline here.
class StrNewOpsBenchmark extends Inputs {
  @Benchmark def farray_reduce(): String = farrayInput.reduce(_ + _)
  @Benchmark def list_reduce(): String = listInput.reduce(_ + _)
  @Benchmark def vector_reduce(): String = vectorInput.reduce(_ + _)
  @Benchmark def iarray_reduce(): String = iarrayInput.reduce(_ + _)
  @Benchmark def ziochunk_reduce(): String = zioChunkInput.reduce(_ + _)
  // fs2.Chunk has no reduce

  @Benchmark def farray_maxBy(): String = farrayInput.maxBy(_.length)
  @Benchmark def list_maxBy(): String = listInput.maxBy(_.length)
  @Benchmark def vector_maxBy(): String = vectorInput.maxBy(_.length)
  @Benchmark def iarray_maxBy(): String = iarrayInput.maxBy(_.length)
  @Benchmark def ziochunk_maxBy(): String = zioChunkInput.maxBy(_.length)

  @Benchmark def farray_minBy(): String = farrayInput.minBy(_.length)
  @Benchmark def list_minBy(): String = listInput.minBy(_.length)
  @Benchmark def vector_minBy(): String = vectorInput.minBy(_.length)
  @Benchmark def iarray_minBy(): String = iarrayInput.minBy(_.length)
  @Benchmark def ziochunk_minBy(): String = zioChunkInput.minBy(_.length)

  @Benchmark def farray_distinct(): FArray[String] = farrayInput.distinct
  @Benchmark def list_distinct(): List[String] = listInput.distinct
  @Benchmark def vector_distinct(): Vector[String] = vectorInput.distinct
  @Benchmark def iarray_distinct(): IArray[String] = iarrayInput.distinct
  @Benchmark def ziochunk_distinct(): zio.Chunk[String] = zioChunkInput.distinct

  @Benchmark def farray_groupBy(): Map[Int, FArray[String]] = farrayInput.groupBy(_.length)
  @Benchmark def list_groupBy(): Map[Int, List[String]] = listInput.groupBy(_.length)
  @Benchmark def vector_groupBy(): Map[Int, Vector[String]] = vectorInput.groupBy(_.length)
  @Benchmark def iarray_groupBy(): Map[Int, IArray[String]] = iarrayInput.groupBy(_.length)
  @Benchmark def ziochunk_groupBy(): Map[Int, zio.Chunk[String]] = zioChunkInput.groupBy(_.length)

  @Benchmark def farray_toSet(): Set[String] = farrayInput.toSet
  @Benchmark def list_toSet(): Set[String] = listInput.toSet
  @Benchmark def vector_toSet(): Set[String] = vectorInput.toSet
  @Benchmark def iarray_toSet(): Set[String] = iarrayInput.toSet
  @Benchmark def ziochunk_toSet(): Set[String] = zioChunkInput.toSet

  @Benchmark def farray_mkString(): String = farrayInput.mkString(",")
  @Benchmark def list_mkString(): String = listInput.mkString(",")
  @Benchmark def vector_mkString(): String = vectorInput.mkString(",")
  @Benchmark def iarray_mkString(): String = iarrayInput.mkString(",")
  @Benchmark def ziochunk_mkString(): String = zioChunkInput.mkString(",")
  // fs2.Chunk has no mkString

  @Benchmark def farray_toArray(): Array[String] = farrayInput.toArray
  @Benchmark def list_toArray(): Array[String] = listInput.toArray
  @Benchmark def vector_toArray(): Array[String] = vectorInput.toArray
  @Benchmark def iarray_toArray(): IArray[String] = IArray.unsafeFromArray(IArray.genericWrapArray(iarrayInput).toArray)
  @Benchmark def ziochunk_toArray(): Array[String] = zioChunkInput.toArray
  @Benchmark def fs2chunk_toArray(): Array[String] = fs2ChunkInput.toArray
}
