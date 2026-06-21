package farray

import org.openjdk.jmh.annotations.Benchmark

// Many folds in ONE method (each was its own inlined dfs before -> blow-up risk). With the consumer dfsC
// each fold is a small "build consumer + call" so they no longer compound past the method-size limit.
class ManyFoldsIntBenchmark extends IntInputs {
  @Benchmark def farray(): Int =
    val a = farrayInput.foldLeft(0)(_ + _)
    val b = farrayInput.foldLeft(0)((s, x) => s + x * 2)
    val c = farrayInput.foldLeft(0)((s, x) => s + x * 3)
    val d = farrayInput.foldLeft(0)((s, x) => s + x * 4)
    val e = farrayInput.foldLeft(0)((s, x) => s + x * 5)
    val g = farrayInput.foldLeft(0)((s, x) => s + x * 6)
    a + b + c + d + e + g
  @Benchmark def array(): Int =
    val a = arrayInput.foldLeft(0)(_ + _)
    val b = arrayInput.foldLeft(0)((s, x) => s + x * 2)
    val c = arrayInput.foldLeft(0)((s, x) => s + x * 3)
    val d = arrayInput.foldLeft(0)((s, x) => s + x * 4)
    val e = arrayInput.foldLeft(0)((s, x) => s + x * 5)
    val g = arrayInput.foldLeft(0)((s, x) => s + x * 6)
    a + b + c + d + e + g
}
