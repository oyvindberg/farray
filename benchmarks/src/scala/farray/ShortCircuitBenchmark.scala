package farray

import org.openjdk.jmh.annotations.Benchmark

// Match near the START (element 5). A short-circuiting op stops at ~index 5; a full scan walks all N.
class ShortCircuitIntBenchmark extends IntInputs {
  @Benchmark def farray_exists(): Boolean = farrayInput.exists(_ == 5)
  @Benchmark def list_exists(): Boolean   = listInput.exists(_ == 5)
  @Benchmark def vector_exists(): Boolean = vectorInput.exists(_ == 5)
  @Benchmark def array_exists(): Boolean  = arrayInput.exists(_ == 5)
  @Benchmark def farray_indexWhere(): Int = farrayInput.indexWhere(_ == 5)
  @Benchmark def array_indexWhere(): Int  = arrayInput.indexWhere(_ == 5)
  @Benchmark def farray_find(): Option[Int] = farrayInput.find(_ == 5)
  @Benchmark def list_find(): Option[Int]   = listInput.find(_ == 5)
}

class ShortCircuitStringBenchmark extends Inputs {
  @Benchmark def farray_exists(): Boolean = farrayInput.exists(_ == "5")
  @Benchmark def list_exists(): Boolean   = listInput.exists(_ == "5")
  @Benchmark def vector_exists(): Boolean = vectorInput.exists(_ == "5")
  @Benchmark def iarray_exists(): Boolean = iarrayInput.exists(_ == "5")
  @Benchmark def farray_indexWhere(): Int = farrayInput.indexWhere(_ == "5")
  @Benchmark def iarray_indexWhere(): Int = iarrayInput.indexWhere(_ == "5")
  @Benchmark def farray_find(): Option[String] = farrayInput.find(_ == "5")
  @Benchmark def list_find(): Option[String]   = listInput.find(_ == "5")
}
