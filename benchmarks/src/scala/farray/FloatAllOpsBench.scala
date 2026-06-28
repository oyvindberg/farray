package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Float pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Float; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed FloatArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class FloatAllOpsBench extends FloatInputs {

  @Benchmark def list(): Float = listInput
    .flatMap(x => List(x, x + 1.0f))
    .filter(_ > 1.5f)
    .map(_ * 2.0f)
    .flatMap(x => List(x, x - 0.5f))
    .filter(_ < 5.0e6f)
    .map(_ + 0.25f)
    .zip(listInput.map(_ + 100.0f))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => List(x, x + 3.0f))
    .filter(_ > 0.0f)
    .foldLeft(0.0f)((acc, x) => acc + x)

  @Benchmark def vector(): Float = vectorInput
    .flatMap(x => Vector(x, x + 1.0f))
    .filter(_ > 1.5f)
    .map(_ * 2.0f)
    .flatMap(x => Vector(x, x - 0.5f))
    .filter(_ < 5.0e6f)
    .map(_ + 0.25f)
    .zip(vectorInput.map(_ + 100.0f))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => Vector(x, x + 3.0f))
    .filter(_ > 0.0f)
    .foldLeft(0.0f)((acc, x) => acc + x)

  @Benchmark def iarray(): Float = iarrayInput
    .flatMap(x => IArray(x, x + 1.0f))
    .filter(_ > 1.5f)
    .map(_ * 2.0f)
    .flatMap(x => IArray(x, x - 0.5f))
    .filter(_ < 5.0e6f)
    .map(_ + 0.25f)
    .zip(iarrayInput.map(_ + 100.0f))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => IArray(x, x + 3.0f))
    .filter(_ > 0.0f)
    .foldLeft(0.0f)((acc, x) => acc + x)

  @Benchmark def farray(): Float = farrayInput
    .flatMap(x => FArray(x, x + 1.0f))
    .filter(_ > 1.5f)
    .map(_ * 2.0f)
    .flatMap(x => FArray(x, x - 0.5f))
    .filter(_ < 5.0e6f)
    .map(_ + 0.25f)
    .zip(farrayInput.map(_ + 100.0f))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => FArray(x, x + 3.0f))
    .filter(_ > 0.0f)
    .foldLeft(0.0f)((acc, x) => acc + x)

  @Benchmark def fs2chunk(): Float = fs2ChunkInput
    .flatMap(x => fs2.Chunk(x, x + 1.0f))
    .filter(_ > 1.5f)
    .map(_ * 2.0f)
    .flatMap(x => fs2.Chunk(x, x - 0.5f))
    .filter(_ < 5.0e6f)
    .map(_ + 0.25f)
    .zip(fs2ChunkInput.map(_ + 100.0f))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => fs2.Chunk(x, x + 3.0f))
    .filter(_ > 0.0f)
    .foldLeft(0.0f)((acc, x) => acc + x)

  @Benchmark def ziochunk(): Float = zioChunkInput
    .flatMap(x => zio.Chunk(x, x + 1.0f))
    .filter(_ > 1.5f)
    .map(_ * 2.0f)
    .flatMap(x => zio.Chunk(x, x - 0.5f))
    .filter(_ < 5.0e6f)
    .map(_ + 0.25f)
    .zip(zioChunkInput.map(_ + 100.0f))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => zio.Chunk(x, x + 3.0f))
    .filter(_ > 0.0f)
    .foldLeft(0.0f)((acc, x) => acc + x)

  @Benchmark def farrayFused(): Float = {
    val zipSrc = farrayInput.map(_ + 100.0f)
    farrayInput.fuse
      .flatMap(x => FArray(x, x + 1.0f))
      .filter(_ > 1.5f)
      .map(_ * 2.0f)
      .flatMap(x => FArray(x, x - 0.5f))
      .filter(_ < 5.0e6f)
      .map(_ + 0.25f)
      .zip(zipSrc)
      .map((a, b) => a + b)
      .zipWithIndex
      .filter((v, i) => i % 4 != 0)
      .map((v, i) => v - i)
      .flatMap(x => FArray(x, x + 3.0f))
      .filter(_ > 0.0f)
      .foldLeft(0.0f)((acc, x) => acc + x)
  }
}
