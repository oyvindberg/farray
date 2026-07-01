package farray

import org.openjdk.jmh.annotations.Benchmark

/** VARIED-probe contains for String: 1024 scrambled lookups (~50% hit / 50% miss) per invocation, so the
  * branch predictor can't memorize the probe — exposes the real ctrl-scan behavior at high probe variety. */
class StrSetContainsMixedBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (fsetA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def scalaset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (sSetA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def scalamut(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (smSetA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def juhashset(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (juA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def guava(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (guavaA.contains(p(i))) acc += 1; i += 1 }; acc }
  @Benchmark def fastutil(): Int = { var acc = 0; var i = 0; val p = probes; while (i < p.length) { if (fuA.contains(p(i))) acc += 1; i += 1 }; acc }
}
