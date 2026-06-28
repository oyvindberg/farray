package farray

import org.openjdk.jmh.annotations.Benchmark

// Materialize / convert ops: toArray / copyToArray / mkString / indices / isDefinedAt.
// fs2.Chunk has toArray / copyToArray; it has no mkString / indices / isDefinedAt.
// zio.Chunk is an IndexedSeq -> full API.
// NOTE: toSeq/toIndexedSeq are intentionally NOT benchmarked — FArray.toSeq/toIndexedSeq is already a
// zero-copy O(1) FArraySeq wrapper, but a native Seq (List, Vector, zio.Chunk) returns `this` and so
// allocates nothing at all. That single wrapper allocation is an inherent, un-winnable gap, so the
// benchmark is omitted. `view` is likewise omitted (FArray has no view).
class ConvertIntBenchmark extends IntInputs {
  @Benchmark def farray_toArray(): Array[Int] = farrayInput.toArray
  @Benchmark def list_toArray(): Array[Int] = listInput.toArray
  @Benchmark def vector_toArray(): Array[Int] = vectorInput.toArray
  @Benchmark def iarray_toArray(): Array[Int] = iarrayInput.toArray
  @Benchmark def ziochunk_toArray(): Array[Int] = zioChunkInput.toArray
  @Benchmark def fs2chunk_toArray(): Array[Int] = fs2ChunkInput.toArray

  @Benchmark def farray_copyToArray(): Array[Int] = { val a = new Array[Int](size); farrayInput.copyToArray(a, 0, size); a }
  @Benchmark def list_copyToArray(): Array[Int] = { val a = new Array[Int](size); listInput.copyToArray(a); a }
  @Benchmark def vector_copyToArray(): Array[Int] = { val a = new Array[Int](size); vectorInput.copyToArray(a); a }
  @Benchmark def ziochunk_copyToArray(): Array[Int] = { val a = new Array[Int](size); zioChunkInput.copyToArray(a); a }
  @Benchmark def fs2chunk_copyToArray(): Array[Int] = { val a = new Array[Int](size); fs2ChunkInput.copyToArray(a, 0); a }

  @Benchmark def farray_mkString(): String = farrayInput.mkString(",")
  @Benchmark def list_mkString(): String = listInput.mkString(",")
  @Benchmark def vector_mkString(): String = vectorInput.mkString(",")
  @Benchmark def iarray_mkString(): String = iarrayInput.mkString(",")
  @Benchmark def ziochunk_mkString(): String = zioChunkInput.mkString(",")
  // fs2.Chunk has no mkString

  // indices: sum them so JMH can't dead-code. FArray exposes indices.
  @Benchmark def farray_indices(): Int = { var s = 0; farrayInput.indices.foreach(s += _); s }
  @Benchmark def list_indices(): Int = { var s = 0; listInput.indices.foreach(s += _); s }
  @Benchmark def vector_indices(): Int = { var s = 0; vectorInput.indices.foreach(s += _); s }
  @Benchmark def iarray_indices(): Int = { var s = 0; iarrayInput.indices.foreach(s += _); s }
  @Benchmark def ziochunk_indices(): Int = { var s = 0; zioChunkInput.indices.foreach(s += _); s }
  // fs2.Chunk has no indices

  // isDefinedAt: FArray exposes it.
  @Benchmark def farray_isDefinedAt(): Boolean = farrayInput.isDefinedAt(size / 2)
  @Benchmark def list_isDefinedAt(): Boolean = listInput.isDefinedAt(size / 2)
  @Benchmark def vector_isDefinedAt(): Boolean = vectorInput.isDefinedAt(size / 2)
  @Benchmark def iarray_isDefinedAt(): Boolean = iarrayInput.isDefinedAt(size / 2)
  @Benchmark def ziochunk_isDefinedAt(): Boolean = zioChunkInput.isDefinedAt(size / 2)
  // fs2.Chunk has no isDefinedAt
}
