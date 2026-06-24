package farray

import org.openjdk.jmh.annotations.Benchmark

// Materialize / convert ops: toArray / copyToArray / mkString / toIndexedSeq / toSeq / indices / isDefinedAt / view.
// FArray exposes toArray / mkString / toSeq / indices / isDefinedAt; it has NO copyToArray / toIndexedSeq / view (API gaps).
// fs2.Chunk has toArray / copyToArray / toVector (-> asSeq for toIndexedSeq); it has no mkString / indices / isDefinedAt / view.
// zio.Chunk is an IndexedSeq -> full API.
class IntConvertBenchmark extends IntInputs {
  @Benchmark def farray_toArray(): Array[Int] = farrayInput.toArray
  @Benchmark def list_toArray(): Array[Int] = listInput.toArray
  @Benchmark def vector_toArray(): Array[Int] = vectorInput.toArray
  @Benchmark def iarray_toArray(): IArray[Int] = IArray.unsafeFromArray(IArray.genericWrapArray(iarrayInput).toArray)
  @Benchmark def array_toArray(): Array[Int] = arrayInput.clone()
  @Benchmark def ziochunk_toArray(): Array[Int] = zioChunkInput.toArray
  @Benchmark def fs2chunk_toArray(): Array[Int] = fs2ChunkInput.toArray

  // copyToArray: FArray has none (API gap)
  @Benchmark def list_copyToArray(): Array[Int] = { val a = new Array[Int](size); listInput.copyToArray(a); a }
  @Benchmark def vector_copyToArray(): Array[Int] = { val a = new Array[Int](size); vectorInput.copyToArray(a); a }
  @Benchmark def array_copyToArray(): Array[Int] = { val a = new Array[Int](size); arrayInput.copyToArray(a); a }
  @Benchmark def ziochunk_copyToArray(): Array[Int] = { val a = new Array[Int](size); zioChunkInput.copyToArray(a); a }
  @Benchmark def fs2chunk_copyToArray(): Array[Int] = { val a = new Array[Int](size); fs2ChunkInput.copyToArray(a, 0); a }

  @Benchmark def farray_mkString(): String = farrayInput.mkString(",")
  @Benchmark def list_mkString(): String = listInput.mkString(",")
  @Benchmark def vector_mkString(): String = vectorInput.mkString(",")
  @Benchmark def iarray_mkString(): String = iarrayInput.mkString(",")
  @Benchmark def array_mkString(): String = arrayInput.mkString(",")
  @Benchmark def ziochunk_mkString(): String = zioChunkInput.mkString(",")
  // fs2.Chunk has no mkString

  // toIndexedSeq: FArray has none (has toSeq/toVector; API gap)
  @Benchmark def list_toIndexedSeq(): IndexedSeq[Int] = listInput.toIndexedSeq
  @Benchmark def vector_toIndexedSeq(): IndexedSeq[Int] = vectorInput.toIndexedSeq
  @Benchmark def iarray_toIndexedSeq(): IndexedSeq[Int] = iarrayInput.toIndexedSeq
  @Benchmark def array_toIndexedSeq(): IndexedSeq[Int] = arrayInput.toIndexedSeq
  @Benchmark def ziochunk_toIndexedSeq(): IndexedSeq[Int] = zioChunkInput.toIndexedSeq
  @Benchmark def fs2chunk_toIndexedSeq(): IndexedSeq[Int] = fs2ChunkInput.asSeq // fs2's IndexedSeq view

  @Benchmark def farray_toSeq(): Seq[Int] = farrayInput.toSeq
  @Benchmark def list_toSeq(): Seq[Int] = listInput.toSeq
  @Benchmark def vector_toSeq(): Seq[Int] = vectorInput.toSeq
  @Benchmark def iarray_toSeq(): Seq[Int] = iarrayInput.toSeq
  @Benchmark def array_toSeq(): Seq[Int] = arrayInput.toSeq
  @Benchmark def ziochunk_toSeq(): Seq[Int] = zioChunkInput.toSeq

  // indices: sum them so JMH can't dead-code. FArray exposes indices.
  @Benchmark def farray_indices(): Int = { var s = 0; farrayInput.indices.foreach(s += _); s }
  @Benchmark def list_indices(): Int = { var s = 0; listInput.indices.foreach(s += _); s }
  @Benchmark def vector_indices(): Int = { var s = 0; vectorInput.indices.foreach(s += _); s }
  @Benchmark def iarray_indices(): Int = { var s = 0; iarrayInput.indices.foreach(s += _); s }
  @Benchmark def array_indices(): Int = { var s = 0; arrayInput.indices.foreach(s += _); s }
  @Benchmark def ziochunk_indices(): Int = { var s = 0; zioChunkInput.indices.foreach(s += _); s }
  // fs2.Chunk has no indices

  // isDefinedAt: FArray exposes it.
  @Benchmark def farray_isDefinedAt(): Boolean = farrayInput.isDefinedAt(size / 2)
  @Benchmark def list_isDefinedAt(): Boolean = listInput.isDefinedAt(size / 2)
  @Benchmark def vector_isDefinedAt(): Boolean = vectorInput.isDefinedAt(size / 2)
  @Benchmark def iarray_isDefinedAt(): Boolean = iarrayInput.isDefinedAt(size / 2)
  @Benchmark def array_isDefinedAt(): Boolean = arrayInput.isDefinedAt(size / 2)
  @Benchmark def ziochunk_isDefinedAt(): Boolean = zioChunkInput.isDefinedAt(size / 2)
  // fs2.Chunk has no isDefinedAt

  // view: a lazy view materialized by summing. FArray has no view (API gap).
  @Benchmark def list_view(): Int = listInput.view.map(_ + 1).sum
  @Benchmark def vector_view(): Int = vectorInput.view.map(_ + 1).sum
  @Benchmark def iarray_view(): Int = iarrayInput.view.map(_ + 1).sum
  @Benchmark def array_view(): Int = arrayInput.view.map(_ + 1).sum
  @Benchmark def ziochunk_view(): Int = zioChunkInput.view.map(_ + 1).sum
  // fs2.Chunk has no view
}
