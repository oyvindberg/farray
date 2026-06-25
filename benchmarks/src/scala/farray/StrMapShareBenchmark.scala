package farray

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

// Does a SHARED-method leaf map fix the @100k multi-map cliff without hurting the single map?
// inlined* = mapProtoRef (leaf loop inlined at each call site). shared* = mapProtoRefShared (leaf loop
// in ONE shared method, called per site like iarray's Array.map). Compare 1-map vs 8-map at @10k/@100k.
class StrMapShareBenchmark extends Inputs {
  @Benchmark def iarray1(bh: Blackhole): Unit =
    bh.consume(iarrayInput.map(_ + "a"))
  @Benchmark def inlined1(bh: Blackhole): Unit =
    bh.consume(Prototype.mapProtoRef(farrayInput.asInstanceOf[FBase])((s: String) => s + "a"))
  @Benchmark def shared1(bh: Blackhole): Unit =
    bh.consume(Prototype.mapProtoRefShared(farrayInput.asInstanceOf[FBase])((s: String) => s + "a"))

  @Benchmark def iarray8(bh: Blackhole): Unit = {
    bh.consume(iarrayInput.map(_ + "a"))
    bh.consume(iarrayInput.map(_ + "b"))
    bh.consume(iarrayInput.map(_.toUpperCase))
    bh.consume(iarrayInput.map(_.toLowerCase))
    bh.consume(iarrayInput.map(s => s + s))
    bh.consume(iarrayInput.map(_.trim))
    bh.consume(iarrayInput.map(_ + "z"))
    bh.consume(iarrayInput.map(s => s.reverse))
  }
  @Benchmark def inlined8(bh: Blackhole): Unit = {
    val xs = farrayInput.asInstanceOf[FBase]
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s + "a"))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s + "b"))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s.toUpperCase))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s.toLowerCase))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s + s))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s.trim))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s + "z"))
    bh.consume(Prototype.mapProtoRef(xs)((s: String) => s.reverse))
  }
  @Benchmark def shared8(bh: Blackhole): Unit = {
    val xs = farrayInput.asInstanceOf[FBase]
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s + "a"))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s + "b"))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s.toUpperCase))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s.toLowerCase))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s + s))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s.trim))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s + "z"))
    bh.consume(Prototype.mapProtoRefShared(xs)((s: String) => s.reverse))
  }
  @Benchmark def small1(bh: Blackhole): Unit =
    bh.consume(Prototype.mapProtoRefSmall(farrayInput.asInstanceOf[FBase])((s: String) => s + "a"))
  @Benchmark def small8(bh: Blackhole): Unit = {
    val xs = farrayInput.asInstanceOf[FBase]
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s + "a"))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s + "b"))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s.toUpperCase))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s.toLowerCase))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s + s))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s.trim))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s + "z"))
    bh.consume(Prototype.mapProtoRefSmall(xs)((s: String) => s.reverse))
  }
}
