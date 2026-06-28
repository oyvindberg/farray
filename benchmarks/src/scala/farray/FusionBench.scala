package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class FusionBench extends IntInputs:
  // eager: 3 intermediate IntArr allocations, 3 passes
  @Benchmark def eager(): FArray[Int] =
    farrayInput.map(_ + 1).filter(_ % 2 == 0).map(_ * 2)

  // fused: one pass, one output, no intermediate FArrays — but stages are stored (specialized) closures
  @Benchmark def fused(): FArray[Int] =
    farrayInput.fuse.map(_ + 1).filter(_ % 2 == 0).map(_ * 2).run

  // hand-inlined: what a macro/staging would generate — one pass, NO closures, fully inlined+unboxed
  @Benchmark def handFused(): Array[Int] =
    val b = scala.collection.mutable.ArrayBuilder.make[Int]
    farrayInput.foreach { x =>
      val y = x + 1; if (y % 2 == 0) b.addOne(y * 2)
    }
    b.result()

  // ---- flatMap: eager (intermediate FArrays) vs fused (one nested loop, growable out) ----
  @Benchmark def flatMapEager(): FArray[Int] =
    farrayInput.map(_ + 1).flatMap(x => FArray(x, x * 2))
  @Benchmark def flatMapFused(): FArray[Int] =
    farrayInput.fuse.map(_ + 1).flatMap(x => FArray(x, x * 2)).run

  // ---- compute-for-survivors: an expensive independent column. Eager computes it for EVERY element; the
  //      fused macro sinks it past the selective filter, so it runs only for the ~1/8 survivors. ----
  private def expensive(x: Int): Int = { var s = x; var k = 0; while (k < 24) { s = s * 1103515245 + 12345; k += 1 }; s }
  @Benchmark def survivorEager(): FArray[Int] =
    farrayInput.map(x => (x, expensive(x))).filter(_._1 % 8 == 0).map(_._2)
  @Benchmark def survivorFused(): FArray[Int] =
    farrayInput.fuse.map(x => (x, expensive(x))).filter(_._1 % 8 == 0).map(_._2).run

  // ---- DEAD column: a tuple component that is never used downstream. Eager computes expensive(x) for every
  //      element (and allocates the tuple); the fused macro never computes it at all (DCE). ----
  @Benchmark def deadColEager(): FArray[Int] =
    farrayInput.map(x => (x + 1, expensive(x))).map(_._1)
  @Benchmark def deadColFused(): FArray[Int] =
    farrayInput.fuse.map(x => (x + 1, expensive(x))).map(_._1).run

  // ---- DEAD zipped side: zip with another source but discard it. Eager reads the operand and builds pairs;
  //      the fused macro never reads the zipped source at all (lazy that(pos)). ----
  @Benchmark def deadZipEager(): FArray[Int] =
    farrayInput.zip(farrayInput).map(_._1)
  @Benchmark def deadZipFused(): FArray[Int] =
    farrayInput.fuse.zip(farrayInput).map(_._1).run

  // ---- COMBINED: zip, build a tuple whose 2nd column is expensive(zippedValue), filter on the 1st, keep the
  //      1st. In fused, the 2nd column is dead → neither expensive(b) NOR the zipped read ever happen. ----
  @Benchmark def deadZipColEager(): FArray[Int] =
    farrayInput.zip(farrayInput).map((a, b) => (a, expensive(b))).filter(_._1 % 4 == 0).map(_._1)
  @Benchmark def deadZipColFused(): FArray[Int] =
    farrayInput.fuse.zip(farrayInput).map((a, b) => (a, expensive(b))).filter(_._1 % 4 == 0).map(_._1).run
