package farray

import org.openjdk.jmh.annotations.Benchmark

// Match near the START (element 5 in an ascending 0..N input). A short-circuiting op stops at ~index 5;
// a full scan walks all N. Shows the win of foreachBreakable over the current foreach-with-flag.
class ShortCircuitBenchmark extends IntInputs {
  @Benchmark def farray_exists(): Boolean = farrayInput.exists(_ == 5)
  @Benchmark def list_exists(): Boolean   = listInput.exists(_ == 5)
  @Benchmark def vector_exists(): Boolean = vectorInput.exists(_ == 5)
  @Benchmark def array_exists(): Boolean  = arrayInput.exists(_ == 5)

  @Benchmark def farray_indexWhere(): Int = farrayInput.indexWhere(_ == 5)
  @Benchmark def list_indexWhere(): Int   = listInput.indexWhere(_ == 5)
  @Benchmark def array_indexWhere(): Int  = arrayInput.indexWhere(_ == 5)

  @Benchmark def farray_find(): Option[Int] = farrayInput.find(_ == 5)
  @Benchmark def list_find(): Option[Int]   = listInput.find(_ == 5)

  // full scan (no match): measures pure per-iteration throughput, not per-call overhead
  @Benchmark def farray_exists_full(): Boolean = farrayInput.exists(_ == -1)
  @Benchmark def array_exists_full(): Boolean   = arrayInput.exists(_ == -1)

  // realistic match position (index 1000): real iteration work, per-call dispatch amortized
  @Benchmark def farray_exists_mid(): Boolean = farrayInput.exists(_ == 1000)
  @Benchmark def array_exists_mid(): Boolean   = arrayInput.exists(_ == 1000)
}
