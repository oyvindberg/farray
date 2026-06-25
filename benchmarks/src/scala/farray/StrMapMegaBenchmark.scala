package farray

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

// MEGAMORPHIC map over REFERENCES (String). 8 distinct compile-time lambdas funnel into the shared
// Array.map for iarray. Unlike the Int case there is no element-boxing (Strings are already objects),
// so this isolates the megamorphic-DISPATCH cost: iarray pays a real virtual Function call per element
// once its map site goes megamorphic; farray inlines each lambda at compile time. Expect a smaller win
// than the Int case (no boxing), driven purely by dispatch.
class StrMapMegaBenchmark extends Inputs {
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
  @Benchmark def proto(bh: Blackhole): Unit = {
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
  @Benchmark def committed(bh: Blackhole): Unit = {
    bh.consume(farrayInput.map(_ + "a"))
    bh.consume(farrayInput.map(_ + "b"))
    bh.consume(farrayInput.map(_.toUpperCase))
    bh.consume(farrayInput.map(_.toLowerCase))
    bh.consume(farrayInput.map(s => s + s))
    bh.consume(farrayInput.map(_.trim))
    bh.consume(farrayInput.map(_ + "z"))
    bh.consume(farrayInput.map(s => s.reverse))
  }
}
