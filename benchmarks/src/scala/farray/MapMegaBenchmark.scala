package farray

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

// MEGAMORPHIC map: 8 DISTINCT compile-time lambdas through `map` in one method. For iarray these all
// funnel into the single shared `Array.map`; if the JIT can't inline all 8 copies, its `f.apply` site
// goes megamorphic (≥3 types) and is NOT devirtualised -> a real per-element virtual call. farray's
// `map` splices each lambda at COMPILE time, so it never depends on the JIT devirtualising anything.
// This is where by-construction inlining should beat JIT-dependent inlining.
//
// All impls run the SAME 8 distinct lambdas so the megamorphic-dispatch cost is isolated and comparable.
class MapMegaIntBenchmark extends IntInputs {
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
  @Benchmark def farray(bh: Blackhole): Unit = {
    bh.consume(farrayInput.map(_ + 1))
    bh.consume(farrayInput.map(_ * 2))
    bh.consume(farrayInput.map(_ - 3))
    bh.consume(farrayInput.map(_ ^ 7))
    bh.consume(farrayInput.map(_ | 4))
    bh.consume(farrayInput.map(_ & 13))
    bh.consume(farrayInput.map(_ + 9))
    bh.consume(farrayInput.map(_ * 5))
  }
  @Benchmark def list(bh: Blackhole): Unit = {
    bh.consume(listInput.map(_ + 1))
    bh.consume(listInput.map(_ * 2))
    bh.consume(listInput.map(_ - 3))
    bh.consume(listInput.map(_ ^ 7))
    bh.consume(listInput.map(_ | 4))
    bh.consume(listInput.map(_ & 13))
    bh.consume(listInput.map(_ + 9))
    bh.consume(listInput.map(_ * 5))
  }
  @Benchmark def vector(bh: Blackhole): Unit = {
    bh.consume(vectorInput.map(_ + 1))
    bh.consume(vectorInput.map(_ * 2))
    bh.consume(vectorInput.map(_ - 3))
    bh.consume(vectorInput.map(_ ^ 7))
    bh.consume(vectorInput.map(_ | 4))
    bh.consume(vectorInput.map(_ & 13))
    bh.consume(vectorInput.map(_ + 9))
    bh.consume(vectorInput.map(_ * 5))
  }
  @Benchmark def fs2chunk(bh: Blackhole): Unit = {
    bh.consume(fs2ChunkInput.map(_ + 1))
    bh.consume(fs2ChunkInput.map(_ * 2))
    bh.consume(fs2ChunkInput.map(_ - 3))
    bh.consume(fs2ChunkInput.map(_ ^ 7))
    bh.consume(fs2ChunkInput.map(_ | 4))
    bh.consume(fs2ChunkInput.map(_ & 13))
    bh.consume(fs2ChunkInput.map(_ + 9))
    bh.consume(fs2ChunkInput.map(_ * 5))
  }
  @Benchmark def ziochunk(bh: Blackhole): Unit = {
    bh.consume(zioChunkInput.map(_ + 1))
    bh.consume(zioChunkInput.map(_ * 2))
    bh.consume(zioChunkInput.map(_ - 3))
    bh.consume(zioChunkInput.map(_ ^ 7))
    bh.consume(zioChunkInput.map(_ | 4))
    bh.consume(zioChunkInput.map(_ & 13))
    bh.consume(zioChunkInput.map(_ + 9))
    bh.consume(zioChunkInput.map(_ * 5))
  }
}

// MEGAMORPHIC map over REFERENCES (String). 8 distinct compile-time lambdas funnel into the shared
// Array.map for iarray. Unlike the Int case there is no element-boxing (Strings are already objects),
// so this isolates the megamorphic-DISPATCH cost: iarray pays a real virtual Function call per element
// once its map site goes megamorphic; farray inlines each lambda at compile time. Expect a smaller win
// than the Int case (no boxing), driven purely by dispatch.
class MapMegaStrBenchmark extends Inputs {
  @Benchmark def iarray(bh: Blackhole): Unit = {
    bh.consume(iarrayInput.map(_ + "a"))
    bh.consume(iarrayInput.map(_ + "b"))
    bh.consume(iarrayInput.map(_.toUpperCase))
    bh.consume(iarrayInput.map(_.toLowerCase))
    bh.consume(iarrayInput.map(s => s + s))
    bh.consume(iarrayInput.map(_.trim))
    bh.consume(iarrayInput.map(_ + "z"))
    bh.consume(iarrayInput.map(s => s.reverse))
  }
  @Benchmark def farray(bh: Blackhole): Unit = {
    bh.consume(farrayInput.map(_ + "a"))
    bh.consume(farrayInput.map(_ + "b"))
    bh.consume(farrayInput.map(_.toUpperCase))
    bh.consume(farrayInput.map(_.toLowerCase))
    bh.consume(farrayInput.map(s => s + s))
    bh.consume(farrayInput.map(_.trim))
    bh.consume(farrayInput.map(_ + "z"))
    bh.consume(farrayInput.map(s => s.reverse))
  }
  @Benchmark def list(bh: Blackhole): Unit = {
    bh.consume(listInput.map(_ + "a"))
    bh.consume(listInput.map(_ + "b"))
    bh.consume(listInput.map(_.toUpperCase))
    bh.consume(listInput.map(_.toLowerCase))
    bh.consume(listInput.map(s => s + s))
    bh.consume(listInput.map(_.trim))
    bh.consume(listInput.map(_ + "z"))
    bh.consume(listInput.map(s => s.reverse))
  }
  @Benchmark def vector(bh: Blackhole): Unit = {
    bh.consume(vectorInput.map(_ + "a"))
    bh.consume(vectorInput.map(_ + "b"))
    bh.consume(vectorInput.map(_.toUpperCase))
    bh.consume(vectorInput.map(_.toLowerCase))
    bh.consume(vectorInput.map(s => s + s))
    bh.consume(vectorInput.map(_.trim))
    bh.consume(vectorInput.map(_ + "z"))
    bh.consume(vectorInput.map(s => s.reverse))
  }
  @Benchmark def fs2chunk(bh: Blackhole): Unit = {
    bh.consume(fs2ChunkInput.map(_ + "a"))
    bh.consume(fs2ChunkInput.map(_ + "b"))
    bh.consume(fs2ChunkInput.map(_.toUpperCase))
    bh.consume(fs2ChunkInput.map(_.toLowerCase))
    bh.consume(fs2ChunkInput.map(s => s + s))
    bh.consume(fs2ChunkInput.map(_.trim))
    bh.consume(fs2ChunkInput.map(_ + "z"))
    bh.consume(fs2ChunkInput.map(s => s.reverse))
  }
  @Benchmark def ziochunk(bh: Blackhole): Unit = {
    bh.consume(zioChunkInput.map(_ + "a"))
    bh.consume(zioChunkInput.map(_ + "b"))
    bh.consume(zioChunkInput.map(_.toUpperCase))
    bh.consume(zioChunkInput.map(_.toLowerCase))
    bh.consume(zioChunkInput.map(s => s + s))
    bh.consume(zioChunkInput.map(_.trim))
    bh.consume(zioChunkInput.map(_ + "z"))
    bh.consume(zioChunkInput.map(s => s.reverse))
  }
}
