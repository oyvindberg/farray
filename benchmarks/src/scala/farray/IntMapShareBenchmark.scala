package farray

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

// Does NOT-INLINE (shared method) keep Int maps UNBOXED + crushing iarray, with no multi-map cliff?
// The B/op tell: shared should allocate only the int[] outputs (no boxing); iarray boxes when megamorphic.
class IntMapShareBenchmark extends IntInputs {
  @Benchmark def iarray1(bh: Blackhole): Unit = bh.consume(iarrayInput.map(_ + 1))
  @Benchmark def inlined1(bh: Blackhole): Unit = bh.consume(Prototype.mapProtoInt(farrayInput.asInstanceOf[FBase])(_ + 1))
  @Benchmark def shared1(bh: Blackhole): Unit = bh.consume(Prototype.mapProtoIntShared(farrayInput.asInstanceOf[FBase])(_ + 1))
  @Benchmark def iarray8(bh: Blackhole): Unit = {
    bh.consume(iarrayInput.map(_ + 1)); bh.consume(iarrayInput.map(_ * 2)); bh.consume(iarrayInput.map(_ - 3)); bh.consume(iarrayInput.map(_ ^ 7))
    bh.consume(iarrayInput.map(_ | 4)); bh.consume(iarrayInput.map(_ & 13)); bh.consume(iarrayInput.map(_ + 9)); bh.consume(iarrayInput.map(_ * 5))
  }
  @Benchmark def inlined8(bh: Blackhole): Unit = {
    val xs = farrayInput.asInstanceOf[FBase]
    bh.consume(Prototype.mapProtoInt(xs)(_ + 1)); bh.consume(Prototype.mapProtoInt(xs)(_ * 2)); bh.consume(Prototype.mapProtoInt(xs)(_ - 3)); bh.consume(Prototype.mapProtoInt(xs)(_ ^ 7))
    bh.consume(Prototype.mapProtoInt(xs)(_ | 4)); bh.consume(Prototype.mapProtoInt(xs)(_ & 13)); bh.consume(Prototype.mapProtoInt(xs)(_ + 9)); bh.consume(Prototype.mapProtoInt(xs)(_ * 5))
  }
  @Benchmark def shared8(bh: Blackhole): Unit = {
    val xs = farrayInput.asInstanceOf[FBase]
    bh.consume(Prototype.mapProtoIntShared(xs)(_ + 1)); bh.consume(Prototype.mapProtoIntShared(xs)(_ * 2)); bh.consume(Prototype.mapProtoIntShared(xs)(_ - 3)); bh.consume(Prototype.mapProtoIntShared(xs)(_ ^ 7))
    bh.consume(Prototype.mapProtoIntShared(xs)(_ | 4)); bh.consume(Prototype.mapProtoIntShared(xs)(_ & 13)); bh.consume(Prototype.mapProtoIntShared(xs)(_ + 9)); bh.consume(Prototype.mapProtoIntShared(xs)(_ * 5))
  }
}
