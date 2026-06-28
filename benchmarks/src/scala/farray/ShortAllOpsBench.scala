package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Short pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Short; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed ShortArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class ShortAllOpsBench extends ShortInputs {

  @Benchmark def list(): Int = listInput
    .flatMap(x => List(x, (x + 1).toShort))
    .filter(_ % 3 != 0)
    .map(s => (s * 2).toShort)
    .flatMap(x => List(x, (x ^ 5).toShort))
    .filter(_ % 2 == 0)
    .map(s => (s - 7).toShort)
    .zip(listInput.map(s => (s + 100).toShort))
    .map((a, b) => (a + b).toShort)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toShort)
    .flatMap(x => List(x, (x + 3).toShort))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def vector(): Int = vectorInput
    .flatMap(x => Vector(x, (x + 1).toShort))
    .filter(_ % 3 != 0)
    .map(s => (s * 2).toShort)
    .flatMap(x => Vector(x, (x ^ 5).toShort))
    .filter(_ % 2 == 0)
    .map(s => (s - 7).toShort)
    .zip(vectorInput.map(s => (s + 100).toShort))
    .map((a, b) => (a + b).toShort)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toShort)
    .flatMap(x => Vector(x, (x + 3).toShort))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def iarray(): Int = iarrayInput
    .flatMap(x => IArray(x, (x + 1).toShort))
    .filter(_ % 3 != 0)
    .map(s => (s * 2).toShort)
    .flatMap(x => IArray(x, (x ^ 5).toShort))
    .filter(_ % 2 == 0)
    .map(s => (s - 7).toShort)
    .zip(iarrayInput.map(s => (s + 100).toShort))
    .map((a, b) => (a + b).toShort)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toShort)
    .flatMap(x => IArray(x, (x + 3).toShort))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def farray(): Int = farrayInput
    .flatMap(x => FArray(x, (x + 1).toShort))
    .filter(_ % 3 != 0)
    .map(s => (s * 2).toShort)
    .flatMap(x => FArray(x, (x ^ 5).toShort))
    .filter(_ % 2 == 0)
    .map(s => (s - 7).toShort)
    .zip(farrayInput.map(s => (s + 100).toShort))
    .map((a, b) => (a + b).toShort)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toShort)
    .flatMap(x => FArray(x, (x + 3).toShort))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def fs2chunk(): Int = fs2ChunkInput
    .flatMap(x => fs2.Chunk(x, (x + 1).toShort))
    .filter(_ % 3 != 0)
    .map(s => (s * 2).toShort)
    .flatMap(x => fs2.Chunk(x, (x ^ 5).toShort))
    .filter(_ % 2 == 0)
    .map(s => (s - 7).toShort)
    .zip(fs2ChunkInput.map(s => (s + 100).toShort))
    .map((a, b) => (a + b).toShort)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toShort)
    .flatMap(x => fs2.Chunk(x, (x + 3).toShort))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def ziochunk(): Int = zioChunkInput
    .flatMap(x => zio.Chunk(x, (x + 1).toShort))
    .filter(_ % 3 != 0)
    .map(s => (s * 2).toShort)
    .flatMap(x => zio.Chunk(x, (x ^ 5).toShort))
    .filter(_ % 2 == 0)
    .map(s => (s - 7).toShort)
    .zip(zioChunkInput.map(s => (s + 100).toShort))
    .map((a, b) => (a + b).toShort)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toShort)
    .flatMap(x => zio.Chunk(x, (x + 3).toShort))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(s => (s + 100).toShort)
    farrayInput.fuse
      .flatMap(x => FArray(x, (x + 1).toShort))
      .filter(_ % 3 != 0)
      .map(s => (s * 2).toShort)
      .flatMap(x => FArray(x, (x ^ 5).toShort))
      .filter(_ % 2 == 0)
      .map(s => (s - 7).toShort)
      .zip(zipSrc)
      .map((a, b) => (a + b).toShort)
      .zipWithIndex
      .filter((v, i) => (v + i) % 4 != 0)
      .map((v, i) => (v - i).toShort)
      .flatMap(x => FArray(x, (x + 3).toShort))
      .filter(_ > 0)
      .foldLeft(0)((acc, x) => acc + x)
  }
}
