package farray

import org.openjdk.jmh.annotations.Benchmark

// map -> filter -> foldLeft
class MapFilterFoldStrBenchmark extends Inputs {
  // start:bench-mapfilterfold
  // the SAME pipeline, only the backing collection differs — that is the whole apples-to-apples premise
  @Benchmark def list(): Int = listInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def farray(): Int = farrayInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  // stop:bench-mapfilterfold
  @Benchmark def iarray(): Int = iarrayInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def vector(): Int = vectorInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  @Benchmark def ziochunk(): Int = zioChunkInput.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
  // java.util.stream: the foldLeft is expressed as the idiomatic mapToInt(...).sum
  @Benchmark def javastream(): Int = java.util.Arrays.stream(strArrInput).map(_ + "x").filter(_.length > 1).mapToInt(_.length).sum
  @Benchmark def farrayFused(): Int = farrayInput.fuse.map(_ + "x").filter(_.length > 1).foldLeft(0)(_ + _.length)
}

// the Int twin — where unboxing turns the same chain into a rout: every competitor stage boxes
// between map/filter/fold, FArray's stays int[] end to end.
class MapFilterFoldIntBenchmark extends IntInputs {
  @Benchmark def list(): Int = listInput.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
  @Benchmark def farray(): Int = farrayInput.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
  @Benchmark def iarray(): Int = iarrayInput.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
  @Benchmark def vector(): Int = vectorInput.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
  @Benchmark def fs2chunk(): Int = fs2ChunkInput.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
  @Benchmark def ziochunk(): Int = zioChunkInput.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
  // java.util.stream: IntStream is the unboxed, fusing pipeline Java reaches for on exactly this shape
  @Benchmark def javastream(): Int = java.util.Arrays.stream(arrInput).map(_ + 1).filter(_ % 2 == 0).reduce(0, (a, b) => a + b)
  @Benchmark def farrayFused(): Int = farrayInput.fuse.map(_ + 1).filter(_ % 2 == 0).foldLeft(0)(_ + _)
}
