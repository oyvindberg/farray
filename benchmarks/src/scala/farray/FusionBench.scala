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

  // ---- FArray -> FArray (apples-to-apples): same pipeline, eager 3-pass vs the fused macro ----
  // eager: 2 intermediate FArrays + 3 passes, returns FArray[Int]
  @Benchmark def eagerF(): FArray[Int] =
    farrayInput.map(_ + 1).filter(_ % 2 == 0).map(_ * 2)

  // fused macro: one unboxed pass, one output FArray, no intermediates, no per-element closures
  @Benchmark def fuseMacro(): FArray[Int] =
    farrayInput.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).toFArray

  // ---- flatMap: eager (intermediate FArrays) vs fused (one nested loop, growable out) ----
  @Benchmark def flatMapEager(): FArray[Int] =
    farrayInput.map(_ + 1).flatMap(x => FArray(x, x * 2))
  @Benchmark def flatMapFused(): FArray[Int] =
    farrayInput.fuse.map(_ + 1).flatMap(x => FArray(x, x * 2)).toFArray

  // ---- compute-for-survivors: an expensive independent column. Eager computes it for EVERY element; the
  //      fused macro sinks it past the selective filter, so it runs only for the ~1/8 survivors. ----
  private def expensive(x: Int): Int = { var s = x; var k = 0; while (k < 24) { s = s * 1103515245 + 12345; k += 1 }; s }
  @Benchmark def survivorEager(): FArray[Int] =
    farrayInput.map(x => (x, expensive(x))).filter(_._1 % 8 == 0).map(_._2)
  @Benchmark def survivorFused(): FArray[Int] =
    farrayInput.fuse.map(x => (x, expensive(x))).filter(_._1 % 8 == 0).map(_._2).toFArray

  // ---- DEAD column: a tuple component that is never used downstream. Eager computes expensive(x) for every
  //      element (and allocates the tuple); the fused macro never computes it at all (DCE). ----
  @Benchmark def deadColEager(): FArray[Int] =
    farrayInput.map(x => (x + 1, expensive(x))).map(_._1)
  @Benchmark def deadColFused(): FArray[Int] =
    farrayInput.fuse.map(x => (x + 1, expensive(x))).map(_._1).toFArray

  // ---- DEAD zipped side: zip with another source but discard it. Eager reads the operand and builds pairs;
  //      the fused macro never reads the zipped source at all (lazy that(pos)). ----
  @Benchmark def deadZipEager(): FArray[Int] =
    farrayInput.zip(farrayInput).map(_._1)
  @Benchmark def deadZipFused(): FArray[Int] =
    farrayInput.fuse.zip(farrayInput).map(_._1).toFArray

  // ---- COMBINED: zip, build a tuple whose 2nd column is expensive(zippedValue), filter on the 1st, keep the
  //      1st. In fused, the 2nd column is dead → neither expensive(b) NOR the zipped read ever happen. ----
  @Benchmark def deadZipColEager(): FArray[Int] =
    farrayInput.zip(farrayInput).map((a, b) => (a, expensive(b))).filter(_._1 % 4 == 0).map(_._1)
  @Benchmark def deadZipColFused(): FArray[Int] =
    farrayInput.fuse.zip(farrayInput).map((a, b) => (a, expensive(b))).filter(_._1 % 4 == 0).map(_._1).toFArray
