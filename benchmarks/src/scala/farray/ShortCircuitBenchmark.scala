package farray

import org.openjdk.jmh.annotations.Benchmark

// Match near the START (element 5). A short-circuiting op stops at ~index 5; a full scan walks all N.
class ShortCircuitIntBenchmark extends IntInputs {
  @Benchmark def farray_exists(): Boolean = farrayInput.exists(_ == 5)
  @Benchmark def list_exists(): Boolean = listInput.exists(_ == 5)
  @Benchmark def vector_exists(): Boolean = vectorInput.exists(_ == 5)
  @Benchmark def array_exists(): Boolean = arrayInput.exists(_ == 5)
  @Benchmark def farray_indexWhere(): Int = farrayInput.indexWhere(_ == 5)
  @Benchmark def array_indexWhere(): Int = arrayInput.indexWhere(_ == 5)
  @Benchmark def farray_find(): Option[Int] = farrayInput.find(_ == 5)
  @Benchmark def list_find(): Option[Int] = listInput.find(_ == 5)
  @Benchmark def fs2chunk_exists(): Boolean = fs2ChunkInput.exists(_ == 5)
  @Benchmark def ziochunk_exists(): Boolean = zioChunkInput.exists(_ == 5)
  @Benchmark def fs2chunk_indexWhere(): Option[Int] = fs2ChunkInput.indexWhere(_ == 5) // fs2 indexWhere returns Option[Int]
  @Benchmark def ziochunk_indexWhere(): Int = zioChunkInput.indexWhere(_ == 5)
  @Benchmark def fs2chunk_find(): Option[Int] = fs2ChunkInput.find(_ == 5)
  @Benchmark def ziochunk_find(): Option[Int] = zioChunkInput.find(_ == 5)
}

class ShortCircuitStringBenchmark extends Inputs {
  @Benchmark def farray_exists(): Boolean = farrayInput.exists(_ == "5")
  @Benchmark def list_exists(): Boolean = listInput.exists(_ == "5")
  @Benchmark def vector_exists(): Boolean = vectorInput.exists(_ == "5")
  @Benchmark def iarray_exists(): Boolean = iarrayInput.exists(_ == "5")
  @Benchmark def farray_indexWhere(): Int = farrayInput.indexWhere(_ == "5")
  @Benchmark def iarray_indexWhere(): Int = iarrayInput.indexWhere(_ == "5")
  @Benchmark def farray_find(): Option[String] = farrayInput.find(_ == "5")
  @Benchmark def list_find(): Option[String] = listInput.find(_ == "5")
  @Benchmark def fs2chunk_exists(): Boolean = fs2ChunkInput.exists(_ == "5")
  @Benchmark def ziochunk_exists(): Boolean = zioChunkInput.exists(_ == "5")
  @Benchmark def fs2chunk_indexWhere(): Option[Int] = fs2ChunkInput.indexWhere(_ == "5") // fs2 indexWhere returns Option[Int]
  @Benchmark def ziochunk_indexWhere(): Int = zioChunkInput.indexWhere(_ == "5")
  @Benchmark def fs2chunk_find(): Option[String] = fs2ChunkInput.find(_ == "5")
  @Benchmark def ziochunk_find(): Option[String] = zioChunkInput.find(_ == "5")
}
