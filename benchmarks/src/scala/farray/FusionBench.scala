package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Prototype: push-based fused pipeline. Each stage composes a SPECIALIZED Function1 (Int=>Int etc. are
// @specialized, so no boxing), and `run` feeds the source through the whole composed sink in ONE pass —
// no intermediate FArray per stage. Compare to the eager chain (3 intermediate IntArrs).
object Fusion:
  final class IntProducer(val run: (Int => Unit) => Unit):
    def map(f: Int => Int): IntProducer       = IntProducer(push => run(x => push(f(x))))
    def filter(p: Int => Boolean): IntProducer = IntProducer(push => run(x => if p(x) then push(x)))
    def toArray: Array[Int] =
      val b = scala.collection.mutable.ArrayBuilder.make[Int]
      run(b.addOne)
      b.result()
  def from(xs: FArray[Int]): IntProducer = IntProducer(push => xs.foreach(push))

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class FusionBench extends IntInputs:
  // eager: 3 intermediate IntArr allocations, 3 passes
  @Benchmark def eager(): Array[Int] =
    farrayInput.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toArray

  // fused: one pass, one output, no intermediate FArrays — but stages are stored (specialized) closures
  @Benchmark def fused(): Array[Int] =
    Fusion.from(farrayInput).map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toArray

  // hand-inlined: what a macro/staging would generate — one pass, NO closures, fully inlined+unboxed
  @Benchmark def handFused(): Array[Int] =
    val b = scala.collection.mutable.ArrayBuilder.make[Int]
    farrayInput.foreach { x => val y = x + 1; if (y % 2 == 0) b.addOne(y * 2) }
    b.result()
