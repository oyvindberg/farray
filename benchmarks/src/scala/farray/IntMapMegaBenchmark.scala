package farray

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

// MEGAMORPHIC map: 8 DISTINCT compile-time lambdas through `map` in one method. For iarray these all
// funnel into the single shared `Array.map`; if the JIT can't inline all 8 copies, its `f.apply` site
// goes megamorphic (≥3 types) and is NOT devirtualised -> a real per-element virtual call. farray's
// `map` splices each lambda at COMPILE time, so it never depends on the JIT devirtualising anything.
// This is where by-construction inlining should beat JIT-dependent inlining.
class IntMapMegaBenchmark extends IntInputs {
  @Benchmark def iarray(bh: Blackhole): Unit = {
    bh.consume(iarrayInput.map(_ + 1))
    bh.consume(iarrayInput.map(_ * 2))
    bh.consume(iarrayInput.map(_ - 3))
    bh.consume(iarrayInput.map(_ ^ 7))
    bh.consume(iarrayInput.map(_ | 4))
    bh.consume(iarrayInput.map(_ & 13))
    bh.consume(iarrayInput.map(_ + 9))
    bh.consume(iarrayInput.map(_ * 5))
  }
  @Benchmark def committed(bh: Blackhole): Unit = {
    bh.consume(farrayInput.map(_ + 1))
    bh.consume(farrayInput.map(_ * 2))
    bh.consume(farrayInput.map(_ - 3))
    bh.consume(farrayInput.map(_ ^ 7))
    bh.consume(farrayInput.map(_ | 4))
    bh.consume(farrayInput.map(_ & 13))
    bh.consume(farrayInput.map(_ + 9))
    bh.consume(farrayInput.map(_ * 5))
  }
}
