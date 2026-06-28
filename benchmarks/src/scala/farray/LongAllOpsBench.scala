package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Long pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Long; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed LongArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class LongAllOpsBench extends LongInputs {

  @Benchmark def list(): Long = listInput
    .flatMap(x => List(x, x + 1L))
    .filter(_ % 3L != 0L)
    .map(_ * 2L)
    .flatMap(x => List(x, x ^ 5L))
    .filter(_ % 2L == 0L)
    .map(_ - 7L)
    .zip(listInput.map(_ + 100L))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4L != 0L)
    .map((v, i) => v - i)
    .flatMap(x => List(x, x + 3L))
    .filter(_ > 0L)
    .foldLeft(0L)((acc, x) => acc + x)

  @Benchmark def vector(): Long = vectorInput
    .flatMap(x => Vector(x, x + 1L))
    .filter(_ % 3L != 0L)
    .map(_ * 2L)
    .flatMap(x => Vector(x, x ^ 5L))
    .filter(_ % 2L == 0L)
    .map(_ - 7L)
    .zip(vectorInput.map(_ + 100L))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4L != 0L)
    .map((v, i) => v - i)
    .flatMap(x => Vector(x, x + 3L))
    .filter(_ > 0L)
    .foldLeft(0L)((acc, x) => acc + x)

  @Benchmark def iarray(): Long = iarrayInput
    .flatMap(x => IArray(x, x + 1L))
    .filter(_ % 3L != 0L)
    .map(_ * 2L)
    .flatMap(x => IArray(x, x ^ 5L))
    .filter(_ % 2L == 0L)
    .map(_ - 7L)
    .zip(iarrayInput.map(_ + 100L))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4L != 0L)
    .map((v, i) => v - i)
    .flatMap(x => IArray(x, x + 3L))
    .filter(_ > 0L)
    .foldLeft(0L)((acc, x) => acc + x)

  @Benchmark def farray(): Long = farrayInput
    .flatMap(x => FArray(x, x + 1L))
    .filter(_ % 3L != 0L)
    .map(_ * 2L)
    .flatMap(x => FArray(x, x ^ 5L))
    .filter(_ % 2L == 0L)
    .map(_ - 7L)
    .zip(farrayInput.map(_ + 100L))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4L != 0L)
    .map((v, i) => v - i)
    .flatMap(x => FArray(x, x + 3L))
    .filter(_ > 0L)
    .foldLeft(0L)((acc, x) => acc + x)

  @Benchmark def fs2chunk(): Long = fs2ChunkInput
    .flatMap(x => fs2.Chunk(x, x + 1L))
    .filter(_ % 3L != 0L)
    .map(_ * 2L)
    .flatMap(x => fs2.Chunk(x, x ^ 5L))
    .filter(_ % 2L == 0L)
    .map(_ - 7L)
    .zip(fs2ChunkInput.map(_ + 100L))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4L != 0L)
    .map((v, i) => v - i)
    .flatMap(x => fs2.Chunk(x, x + 3L))
    .filter(_ > 0L)
    .foldLeft(0L)((acc, x) => acc + x)

  @Benchmark def ziochunk(): Long = zioChunkInput
    .flatMap(x => zio.Chunk(x, x + 1L))
    .filter(_ % 3L != 0L)
    .map(_ * 2L)
    .flatMap(x => zio.Chunk(x, x ^ 5L))
    .filter(_ % 2L == 0L)
    .map(_ - 7L)
    .zip(zioChunkInput.map(_ + 100L))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4L != 0L)
    .map((v, i) => v - i)
    .flatMap(x => zio.Chunk(x, x + 3L))
    .filter(_ > 0L)
    .foldLeft(0L)((acc, x) => acc + x)

  @Benchmark def farrayFused(): Long = {
    val zipSrc = farrayInput.map(_ + 100L)
    farrayInput.fuse
      .flatMap(x => FArray(x, x + 1L))
      .filter(_ % 3L != 0L)
      .map(_ * 2L)
      .flatMap(x => FArray(x, x ^ 5L))
      .filter(_ % 2L == 0L)
      .map(_ - 7L)
      .zip(zipSrc)
      .map((a, b) => a + b)
      .zipWithIndex
      .filter((v, i) => (v + i) % 4L != 0L)
      .map((v, i) => v - i)
      .flatMap(x => FArray(x, x + 3L))
      .filter(_ > 0L)
      .foldLeft(0L)((acc, x) => acc + x)
  }
}
