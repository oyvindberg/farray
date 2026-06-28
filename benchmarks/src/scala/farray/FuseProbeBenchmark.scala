package farray

import org.openjdk.jmh.annotations.Benchmark

// Probe WHICH fuse cost hurts C2: a NO-flatMap pipeline (map/filter/zip/zipWithIndex only). If fused
// beats eager here on C2, then flatMap's per-element IntArr alloc + intAt read is the culprit.
class FuseProbeNoFlatMapBenchmark extends IntInputs {
  @Benchmark def farrayEager(): Int = {
    farrayInput
      .filter(_ % 3 != 0)
      .map(_ * 2)
      .filter(_ % 2 == 0)
      .map(_ - 7)
      .zip(farrayInput.map(_ + 100))
      .map((a, b) => a + b)
      .zipWithIndex
      .filter((v, i) => (v + i) % 4 != 0)
      .map((v, i) => v - i)
      .filter(_ > 0)
      .foldLeft(0)(_ + _)
  }
  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(_ + 100)
    farrayInput.fuse
      .filter(_ % 3 != 0)
      .map(_ * 2)
      .filter(_ % 2 == 0)
      .map(_ - 7)
      .zip(zipSrc)
      .map((a, b) => a + b)
      .zipWithIndex
      .filter((v, i) => (v + i) % 4 != 0)
      .map((v, i) => v - i)
      .filter(_ > 0)
      .foldLeft(0)(_ + _)
  }
}

// Probe the OTHER extreme: ONLY flatMaps + a fold (no zip/zipWithIndex). Isolates the flatMap inner cost.
// One class PER element-kind (Int/Long/Double/Ref) to prove the inline `${K}Arr` leaf fast-path in
// `loopOver` covers EVERY kind, not just Int — the inner FArray each flatMap builds is of that kind.
class FuseProbeFlatMapOnlyBenchmark extends IntInputs {
  @Benchmark def farrayEager(): Int =
    farrayInput.flatMap(x => FArray(x, x + 1)).flatMap(x => FArray(x, x ^ 5)).flatMap(x => FArray(x, x + 3)).foldLeft(0)(_ + _)
  @Benchmark def farrayFused(): Int =
    farrayInput.fuse.flatMap(x => FArray(x, x + 1)).flatMap(x => FArray(x, x ^ 5)).flatMap(x => FArray(x, x + 3)).foldLeft(0)(_ + _)
}

class FuseProbeFlatMapLongBenchmark extends IntInputs {
  @Benchmark def farrayEager(): Long =
    farrayInput.flatMap(x => FArray(x.toLong, x.toLong + 1)).flatMap(y => FArray(y, y ^ 5L)).flatMap(y => FArray(y, y + 3)).foldLeft(0L)(_ + _)
  @Benchmark def farrayFused(): Long =
    farrayInput.fuse.flatMap(x => FArray(x.toLong, x.toLong + 1)).flatMap(y => FArray(y, y ^ 5L)).flatMap(y => FArray(y, y + 3)).foldLeft(0L)(_ + _)
}

class FuseProbeFlatMapDoubleBenchmark extends IntInputs {
  @Benchmark def farrayEager(): Double =
    farrayInput.flatMap(x => FArray(x.toDouble, x + 1.0)).flatMap(y => FArray(y, y * 2.0)).flatMap(y => FArray(y, y - 3.0)).foldLeft(0.0)(_ + _)
  @Benchmark def farrayFused(): Double =
    farrayInput.fuse.flatMap(x => FArray(x.toDouble, x + 1.0)).flatMap(y => FArray(y, y * 2.0)).flatMap(y => FArray(y, y - 3.0)).foldLeft(0.0)(_ + _)
}

class FuseProbeFlatMapRefBenchmark extends Inputs {
  @Benchmark def farrayEager(): Int =
    farrayInput.flatMap(s => FArray(s, s + "a")).flatMap(s => FArray(s, s + "b")).flatMap(s => FArray(s, s + "c")).foldLeft(0)(_ + _.length)
  @Benchmark def farrayFused(): Int =
    farrayInput.fuse.flatMap(s => FArray(s, s + "a")).flatMap(s => FArray(s, s + "b")).flatMap(s => FArray(s, s + "c")).foldLeft(0)(_ + _.length)
}
