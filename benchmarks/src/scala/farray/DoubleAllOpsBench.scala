package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Double pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Double; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed DoubleArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class DoubleAllOpsBench extends DoubleInputs {

  @Benchmark def list(): Double = listInput
    .flatMap(x => List(x, x + 1.0))
    .filter(_ > 1.5)
    .map(_ * 2.0)
    .flatMap(x => List(x, x - 0.5))
    .filter(_ < 5.0e6)
    .map(_ + 0.25)
    .zip(listInput.map(_ + 100.0))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => List(x, x + 3.0))
    .filter(_ > 0.0)
    .foldLeft(0.0)((acc, x) => acc + x)

  @Benchmark def vector(): Double = vectorInput
    .flatMap(x => Vector(x, x + 1.0))
    .filter(_ > 1.5)
    .map(_ * 2.0)
    .flatMap(x => Vector(x, x - 0.5))
    .filter(_ < 5.0e6)
    .map(_ + 0.25)
    .zip(vectorInput.map(_ + 100.0))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => Vector(x, x + 3.0))
    .filter(_ > 0.0)
    .foldLeft(0.0)((acc, x) => acc + x)

  @Benchmark def iarray(): Double = iarrayInput
    .flatMap(x => IArray(x, x + 1.0))
    .filter(_ > 1.5)
    .map(_ * 2.0)
    .flatMap(x => IArray(x, x - 0.5))
    .filter(_ < 5.0e6)
    .map(_ + 0.25)
    .zip(iarrayInput.map(_ + 100.0))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => IArray(x, x + 3.0))
    .filter(_ > 0.0)
    .foldLeft(0.0)((acc, x) => acc + x)

  @Benchmark def farray(): Double = farrayInput
    .flatMap(x => FArray(x, x + 1.0))
    .filter(_ > 1.5)
    .map(_ * 2.0)
    .flatMap(x => FArray(x, x - 0.5))
    .filter(_ < 5.0e6)
    .map(_ + 0.25)
    .zip(farrayInput.map(_ + 100.0))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => FArray(x, x + 3.0))
    .filter(_ > 0.0)
    .foldLeft(0.0)((acc, x) => acc + x)

  @Benchmark def fs2chunk(): Double = fs2ChunkInput
    .flatMap(x => fs2.Chunk(x, x + 1.0))
    .filter(_ > 1.5)
    .map(_ * 2.0)
    .flatMap(x => fs2.Chunk(x, x - 0.5))
    .filter(_ < 5.0e6)
    .map(_ + 0.25)
    .zip(fs2ChunkInput.map(_ + 100.0))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => fs2.Chunk(x, x + 3.0))
    .filter(_ > 0.0)
    .foldLeft(0.0)((acc, x) => acc + x)

  @Benchmark def ziochunk(): Double = zioChunkInput
    .flatMap(x => zio.Chunk(x, x + 1.0))
    .filter(_ > 1.5)
    .map(_ * 2.0)
    .flatMap(x => zio.Chunk(x, x - 0.5))
    .filter(_ < 5.0e6)
    .map(_ + 0.25)
    .zip(zioChunkInput.map(_ + 100.0))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => zio.Chunk(x, x + 3.0))
    .filter(_ > 0.0)
    .foldLeft(0.0)((acc, x) => acc + x)

  @Benchmark def farrayFused(): Double = {
    val zipSrc = farrayInput.map(_ + 100.0)
    farrayInput.fuse
      .flatMap(x => FArray(x, x + 1.0))
      .filter(_ > 1.5)
      .map(_ * 2.0)
      .flatMap(x => FArray(x, x - 0.5))
      .filter(_ < 5.0e6)
      .map(_ + 0.25)
      .zip(zipSrc)
      .map((a, b) => a + b)
      .zipWithIndex
      .filter((v, i) => i % 4 != 0)
      .map((v, i) => v - i)
      .flatMap(x => FArray(x, x + 3.0))
      .filter(_ > 0.0)
      .foldLeft(0.0)((acc, x) => acc + x)
  }
}
