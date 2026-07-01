package farray

import org.openjdk.jmh.annotations.Benchmark

/** VARIED-probe contains: 1024 scrambled lookups (~50% hit / 50% miss) per invocation. Unlike the single-probe
  * ContainsHit/Miss, the branch predictor can't memorize the lookup — this exposes the real branch/ctrl-scan
  * behavior (branchy byte-at-a-time probing collapses at high probe variety; branch-free holds). */
class IntSetContainsMixedBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (fsetA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def scalaset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (sSetA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def immbitset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (immBitA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def jubitset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (juBitA.get(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def fastutil(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (fuA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def eclipseimm(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (ecImmA.contains(p(i))) acc += 1; i += 1 }; acc }
}
