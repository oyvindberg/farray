package farray

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

// MEGAMORPHIC collect: 8 DISTINCT PartialFunction literals through `collect` in one method — MapMega's
// experiment pointed at the op whose rivals funnel every PF through one shared collect loop, whose
// pf.applyOrElse site goes megamorphic (>=3 PF classes) and stops devirtualising. farray's collect is
// picked apart at compile time (CollectMacro): each call site owns its loop, so the shared-site
// question doesn't arise — though its p/f SAM calls inside the shared collectLeaf* methods face the
// same megamorphism, unboxed. All impls run the SAME 8 PFs so the dispatch cost is isolated.
class CollectMegaIntBenchmark extends IntInputs {
  @Benchmark def farray(bh: Blackhole): Unit = {
    bh.consume(farrayInput.collect { case x if x % 2 == 0 => x + 1 })
    bh.consume(farrayInput.collect { case x if x % 3 != 0 => x * 2 })
    bh.consume(farrayInput.collect { case x if x > 16 => x - 3 })
    bh.consume(farrayInput.collect { case x if (x & 1) == 1 => x ^ 7 })
    bh.consume(farrayInput.collect { case x if x % 5 != 4 => x | 4 })
    bh.consume(farrayInput.collect { case x if x % 7 < 5 => x & 13 })
    bh.consume(farrayInput.collect { case x if x % 4 != 2 => x + 9 })
    bh.consume(farrayInput.collect { case x if x % 6 > 0 => x * 5 })
  }
  @Benchmark def iarray(bh: Blackhole): Unit = {
    bh.consume(iarrayInput.collect { case x if x % 2 == 0 => x + 1 })
    bh.consume(iarrayInput.collect { case x if x % 3 != 0 => x * 2 })
    bh.consume(iarrayInput.collect { case x if x > 16 => x - 3 })
    bh.consume(iarrayInput.collect { case x if (x & 1) == 1 => x ^ 7 })
    bh.consume(iarrayInput.collect { case x if x % 5 != 4 => x | 4 })
    bh.consume(iarrayInput.collect { case x if x % 7 < 5 => x & 13 })
    bh.consume(iarrayInput.collect { case x if x % 4 != 2 => x + 9 })
    bh.consume(iarrayInput.collect { case x if x % 6 > 0 => x * 5 })
  }
  @Benchmark def list(bh: Blackhole): Unit = {
    bh.consume(listInput.collect { case x if x % 2 == 0 => x + 1 })
    bh.consume(listInput.collect { case x if x % 3 != 0 => x * 2 })
    bh.consume(listInput.collect { case x if x > 16 => x - 3 })
    bh.consume(listInput.collect { case x if (x & 1) == 1 => x ^ 7 })
    bh.consume(listInput.collect { case x if x % 5 != 4 => x | 4 })
    bh.consume(listInput.collect { case x if x % 7 < 5 => x & 13 })
    bh.consume(listInput.collect { case x if x % 4 != 2 => x + 9 })
    bh.consume(listInput.collect { case x if x % 6 > 0 => x * 5 })
  }
  @Benchmark def vector(bh: Blackhole): Unit = {
    bh.consume(vectorInput.collect { case x if x % 2 == 0 => x + 1 })
    bh.consume(vectorInput.collect { case x if x % 3 != 0 => x * 2 })
    bh.consume(vectorInput.collect { case x if x > 16 => x - 3 })
    bh.consume(vectorInput.collect { case x if (x & 1) == 1 => x ^ 7 })
    bh.consume(vectorInput.collect { case x if x % 5 != 4 => x | 4 })
    bh.consume(vectorInput.collect { case x if x % 7 < 5 => x & 13 })
    bh.consume(vectorInput.collect { case x if x % 4 != 2 => x + 9 })
    bh.consume(vectorInput.collect { case x if x % 6 > 0 => x * 5 })
  }
  @Benchmark def fs2chunk(bh: Blackhole): Unit = {
    bh.consume(fs2ChunkInput.collect { case x if x % 2 == 0 => x + 1 })
    bh.consume(fs2ChunkInput.collect { case x if x % 3 != 0 => x * 2 })
    bh.consume(fs2ChunkInput.collect { case x if x > 16 => x - 3 })
    bh.consume(fs2ChunkInput.collect { case x if (x & 1) == 1 => x ^ 7 })
    bh.consume(fs2ChunkInput.collect { case x if x % 5 != 4 => x | 4 })
    bh.consume(fs2ChunkInput.collect { case x if x % 7 < 5 => x & 13 })
    bh.consume(fs2ChunkInput.collect { case x if x % 4 != 2 => x + 9 })
    bh.consume(fs2ChunkInput.collect { case x if x % 6 > 0 => x * 5 })
  }
  @Benchmark def ziochunk(bh: Blackhole): Unit = {
    bh.consume(zioChunkInput.collect { case x if x % 2 == 0 => x + 1 })
    bh.consume(zioChunkInput.collect { case x if x % 3 != 0 => x * 2 })
    bh.consume(zioChunkInput.collect { case x if x > 16 => x - 3 })
    bh.consume(zioChunkInput.collect { case x if (x & 1) == 1 => x ^ 7 })
    bh.consume(zioChunkInput.collect { case x if x % 5 != 4 => x | 4 })
    bh.consume(zioChunkInput.collect { case x if x % 7 < 5 => x & 13 })
    bh.consume(zioChunkInput.collect { case x if x % 4 != 2 => x + 9 })
    bh.consume(zioChunkInput.collect { case x if x % 6 > 0 => x * 5 })
  }
}

class CollectMegaStrBenchmark extends Inputs {
  @Benchmark def farray(bh: Blackhole): Unit = {
    bh.consume(farrayInput.collect { case s if s.length > 1 => s })
    bh.consume(farrayInput.collect { case s if s.length < 9 => s + "b" })
    bh.consume(farrayInput.collect { case s if !s.isEmpty => s.toUpperCase })
    bh.consume(farrayInput.collect { case s if s.length != 3 => s.toLowerCase })
    bh.consume(farrayInput.collect { case s if s.length > 0 => s + s })
    bh.consume(farrayInput.collect { case s if s.length < 12 => s })
    bh.consume(farrayInput.collect { case s if s.length != 5 => s + "z" })
    bh.consume(farrayInput.collect { case s if s.length > 1 => s.reverse })
  }
  @Benchmark def iarray(bh: Blackhole): Unit = {
    bh.consume(iarrayInput.collect { case s if s.length > 1 => s })
    bh.consume(iarrayInput.collect { case s if s.length < 9 => s + "b" })
    bh.consume(iarrayInput.collect { case s if !s.isEmpty => s.toUpperCase })
    bh.consume(iarrayInput.collect { case s if s.length != 3 => s.toLowerCase })
    bh.consume(iarrayInput.collect { case s if s.length > 0 => s + s })
    bh.consume(iarrayInput.collect { case s if s.length < 12 => s })
    bh.consume(iarrayInput.collect { case s if s.length != 5 => s + "z" })
    bh.consume(iarrayInput.collect { case s if s.length > 1 => s.reverse })
  }
  @Benchmark def list(bh: Blackhole): Unit = {
    bh.consume(listInput.collect { case s if s.length > 1 => s })
    bh.consume(listInput.collect { case s if s.length < 9 => s + "b" })
    bh.consume(listInput.collect { case s if !s.isEmpty => s.toUpperCase })
    bh.consume(listInput.collect { case s if s.length != 3 => s.toLowerCase })
    bh.consume(listInput.collect { case s if s.length > 0 => s + s })
    bh.consume(listInput.collect { case s if s.length < 12 => s })
    bh.consume(listInput.collect { case s if s.length != 5 => s + "z" })
    bh.consume(listInput.collect { case s if s.length > 1 => s.reverse })
  }
  @Benchmark def vector(bh: Blackhole): Unit = {
    bh.consume(vectorInput.collect { case s if s.length > 1 => s })
    bh.consume(vectorInput.collect { case s if s.length < 9 => s + "b" })
    bh.consume(vectorInput.collect { case s if !s.isEmpty => s.toUpperCase })
    bh.consume(vectorInput.collect { case s if s.length != 3 => s.toLowerCase })
    bh.consume(vectorInput.collect { case s if s.length > 0 => s + s })
    bh.consume(vectorInput.collect { case s if s.length < 12 => s })
    bh.consume(vectorInput.collect { case s if s.length != 5 => s + "z" })
    bh.consume(vectorInput.collect { case s if s.length > 1 => s.reverse })
  }
  @Benchmark def fs2chunk(bh: Blackhole): Unit = {
    bh.consume(fs2ChunkInput.collect { case s if s.length > 1 => s })
    bh.consume(fs2ChunkInput.collect { case s if s.length < 9 => s + "b" })
    bh.consume(fs2ChunkInput.collect { case s if !s.isEmpty => s.toUpperCase })
    bh.consume(fs2ChunkInput.collect { case s if s.length != 3 => s.toLowerCase })
    bh.consume(fs2ChunkInput.collect { case s if s.length > 0 => s + s })
    bh.consume(fs2ChunkInput.collect { case s if s.length < 12 => s })
    bh.consume(fs2ChunkInput.collect { case s if s.length != 5 => s + "z" })
    bh.consume(fs2ChunkInput.collect { case s if s.length > 1 => s.reverse })
  }
  @Benchmark def ziochunk(bh: Blackhole): Unit = {
    bh.consume(zioChunkInput.collect { case s if s.length > 1 => s })
    bh.consume(zioChunkInput.collect { case s if s.length < 9 => s + "b" })
    bh.consume(zioChunkInput.collect { case s if !s.isEmpty => s.toUpperCase })
    bh.consume(zioChunkInput.collect { case s if s.length != 3 => s.toLowerCase })
    bh.consume(zioChunkInput.collect { case s if s.length > 0 => s + s })
    bh.consume(zioChunkInput.collect { case s if s.length < 12 => s })
    bh.consume(zioChunkInput.collect { case s if s.length != 5 => s + "z" })
    bh.consume(zioChunkInput.collect { case s if s.length > 1 => s.reverse })
  }
}
