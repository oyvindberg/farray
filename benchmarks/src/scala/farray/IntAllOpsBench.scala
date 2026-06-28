package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Int pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Int; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed IntArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class IntAllOpsBench extends IntInputs {

  @Benchmark def list(): Int = listInput
    .flatMap(x => List(x, x + 1))
    .filter(_ % 3 != 0)
    .map(_ * 2)
    .flatMap(x => List(x, x ^ 5))
    .filter(_ % 2 == 0)
    .map(_ - 7)
    .zip(listInput.map(_ + 100))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => List(x, x + 3))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def vector(): Int = vectorInput
    .flatMap(x => Vector(x, x + 1))
    .filter(_ % 3 != 0)
    .map(_ * 2)
    .flatMap(x => Vector(x, x ^ 5))
    .filter(_ % 2 == 0)
    .map(_ - 7)
    .zip(vectorInput.map(_ + 100))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => Vector(x, x + 3))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def iarray(): Int = iarrayInput
    .flatMap(x => IArray(x, x + 1))
    .filter(_ % 3 != 0)
    .map(_ * 2)
    .flatMap(x => IArray(x, x ^ 5))
    .filter(_ % 2 == 0)
    .map(_ - 7)
    .zip(iarrayInput.map(_ + 100))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => IArray(x, x + 3))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def farray(): Int = farrayInput
    .flatMap(x => FArray(x, x + 1))
    .filter(_ % 3 != 0)
    .map(_ * 2)
    .flatMap(x => FArray(x, x ^ 5))
    .filter(_ % 2 == 0)
    .map(_ - 7)
    .zip(farrayInput.map(_ + 100))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => FArray(x, x + 3))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def fs2chunk(): Int = fs2ChunkInput
    .flatMap(x => fs2.Chunk(x, x + 1))
    .filter(_ % 3 != 0)
    .map(_ * 2)
    .flatMap(x => fs2.Chunk(x, x ^ 5))
    .filter(_ % 2 == 0)
    .map(_ - 7)
    .zip(fs2ChunkInput.map(_ + 100))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => fs2.Chunk(x, x + 3))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def ziochunk(): Int = zioChunkInput
    .flatMap(x => zio.Chunk(x, x + 1))
    .filter(_ % 3 != 0)
    .map(_ * 2)
    .flatMap(x => zio.Chunk(x, x ^ 5))
    .filter(_ % 2 == 0)
    .map(_ - 7)
    .zip(zioChunkInput.map(_ + 100))
    .map((a, b) => a + b)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => v - i)
    .flatMap(x => zio.Chunk(x, x + 3))
    .filter(_ > 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(_ + 100)
    farrayInput.fuse
      .flatMap(x => FArray(x, x + 1))
      .filter(_ % 3 != 0)
      .map(_ * 2)
      .flatMap(x => FArray(x, x ^ 5))
      .filter(_ % 2 == 0)
      .map(_ - 7)
      .zip(zipSrc)
      .map((a, b) => a + b)
      .zipWithIndex
      .filter((v, i) => (v + i) % 4 != 0)
      .map((v, i) => v - i)
      .flatMap(x => FArray(x, x + 3))
      .filter(_ > 0)
      .foldLeft(0)((acc, x) => acc + x)
  }
}
