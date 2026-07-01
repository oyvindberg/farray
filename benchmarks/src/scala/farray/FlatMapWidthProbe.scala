package farray

import org.openjdk.jmh.annotations.Benchmark

// Primitive flatMap(width W) -> map -> fold across inner widths, to find where the segmented ${K}FlatMap node
// stops regressing throughput vs the flatten-copy baseline (the between-segment cache-miss amortises over W).
// Measured node (current build) vs flatten (git checkout the pre-node GenCores) at each width; farray-only.
class FlatMapWidthIntProbe extends IntInputs {
  @Benchmark def w8(): Int   = farrayInput.flatMap(x => FArray.tabulate(8)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def w16(): Int  = farrayInput.flatMap(x => FArray.tabulate(16)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def w32(): Int  = farrayInput.flatMap(x => FArray.tabulate(32)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def w64(): Int  = farrayInput.flatMap(x => FArray.tabulate(64)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def w128(): Int = farrayInput.flatMap(x => FArray.tabulate(128)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
}

// Reference (String) counterpart — refs cross over to a node win at a LOWER width than primitives.
class FlatMapWidthStrProbe extends Inputs {
  @Benchmark def w2(): Int  = farrayInput.flatMap(s => FArray.tabulate(2)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def w4(): Int  = farrayInput.flatMap(s => FArray.tabulate(4)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def w8(): Int  = farrayInput.flatMap(s => FArray.tabulate(8)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def w16(): Int = farrayInput.flatMap(s => FArray.tabulate(16)(_ => s)).map(_.length).foldLeft(0)(_ + _)
}
