package farray

import org.openjdk.jmh.annotations.Benchmark

// How many dfs-inlining ops can chain in ONE method before it blows past the JIT HugeMethodLimit and runs
// interpreted? Each .map inlines one dfs (~1.5KB). Compare FArray to Array at chain lengths 3 / 6 / 9 / 12.
class MapChainIntBenchmark extends IntInputs {
  @Benchmark def farray3(): Int = farrayInput.map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def array3(): Int  = arrayInput.map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)

  @Benchmark def farray6(): Int = farrayInput.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def array6(): Int  = arrayInput.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)

  @Benchmark def farray9(): Int = farrayInput.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def array9(): Int  = arrayInput.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)

  @Benchmark def farray12(): Int = farrayInput.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def array12(): Int  = arrayInput.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1).foldLeft(0)(_ + _)
}
