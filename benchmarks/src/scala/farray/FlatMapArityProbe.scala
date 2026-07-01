package farray

import org.openjdk.jmh.annotations.Benchmark

// Isolate the flatMap-node regression: SAME narrow (size-2) inners, SAME trailing `.map.foldLeft`, differing
// ONLY in how many flatMaps precede. If `single` is neutral vs baseline but `double` regresses, the cost is
// the SECOND flatMap re-flattening the first flatMap's node (its tree-source path materialises the source),
// not the node itself. farray-only (we compare before/after the node via git stash).
class FlatMapArityIntProbe extends IntInputs {
  @Benchmark def fm1(): Int =
    farrayInput.flatMap(x => FArray(x, x + 1)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def fm2(): Int =
    farrayInput.flatMap(x => FArray(x, x + 1)).flatMap(y => FArray(y, y * 2)).map(_ + 1).foldLeft(0)(_ + _)
  @Benchmark def fm3(): Int =
    farrayInput.flatMap(x => FArray(x, x + 1)).flatMap(y => FArray(y, y * 2)).flatMap(z => FArray(z, z + 3)).map(_ + 1).foldLeft(0)(_ + _)
}
