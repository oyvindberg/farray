package farray

import org.openjdk.jmh.annotations.Benchmark

// flatMap with WIDE inners (width 16) then an element-wise `map`, then `fold` to force traversal.
//
// Contrast with FlatMapChain{Int,Str} (size-2 inners): those are the PESSIMAL case for an array-of-arrays
// flatMap node (per-inner header + outer pointer array dominate; per-segment loop-entry overhead isn't
// amortized over 2 elements). WIDE inners amortize both — so this is the WIN case for a flatMap node that
// stores the inner arrays directly instead of flatten-copying them into one contiguous leaf. One flatMap
// (n -> 16n) keeps the working set reasonable while still exercising "flatMap builds a node, then a
// downstream element-wise combinator traverses more arrays".
private inline val W = 16

class FlatMapWideIntBenchmark extends IntInputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(x => FArray.tabulate(W)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def list(): Int = listInput.flatMap(x => List.tabulate(W)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.flatMap(x => Vector.tabulate(W)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.flatMap(x => IArray.tabulate(W)(j => x + j)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.flatMap(x => fs2.Chunk.array(Array.tabulate(W)(j => x + j))).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.flatMap(x => zio.Chunk.fromArray(Array.tabulate(W)(j => x + j))).map(_ + 1).foldLeft(0)(_ + _)
}

class FlatMapWideStrBenchmark extends Inputs {
  @Benchmark def farray(): Int = farrayInput.flatMap(s => FArray.tabulate(W)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def list(): Int = listInput.flatMap(s => List.tabulate(W)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.flatMap(s => Vector.tabulate(W)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.flatMap(s => IArray.tabulate(W)(_ => s)).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.flatMap(s => fs2.Chunk.array(Array.fill(W)(s))).map(_.length).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.flatMap(s => zio.Chunk.fromArray(Array.fill(W)(s))).map(_.length).foldLeft(0)(_ + _)
}
