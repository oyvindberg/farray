package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Byte pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Byte; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed ByteArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class ByteAllOpsBench extends ByteInputs {

  @Benchmark def list(): Int = listInput
    .flatMap(x => List(x, (x + 1).toByte))
    .filter(_ % 3 != 0)
    .map(b => (b * 2).toByte)
    .flatMap(x => List(x, (x ^ 5).toByte))
    .filter(_ % 2 == 0)
    .map(b => (b - 7).toByte)
    .zip(listInput.map(b => (b + 100).toByte))
    .map((a, b) => (a + b).toByte)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toByte)
    .flatMap(x => List(x, (x + 3).toByte))
    .filter(_ != 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def vector(): Int = vectorInput
    .flatMap(x => Vector(x, (x + 1).toByte))
    .filter(_ % 3 != 0)
    .map(b => (b * 2).toByte)
    .flatMap(x => Vector(x, (x ^ 5).toByte))
    .filter(_ % 2 == 0)
    .map(b => (b - 7).toByte)
    .zip(vectorInput.map(b => (b + 100).toByte))
    .map((a, b) => (a + b).toByte)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toByte)
    .flatMap(x => Vector(x, (x + 3).toByte))
    .filter(_ != 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def iarray(): Int = iarrayInput
    .flatMap(x => IArray(x, (x + 1).toByte))
    .filter(_ % 3 != 0)
    .map(b => (b * 2).toByte)
    .flatMap(x => IArray(x, (x ^ 5).toByte))
    .filter(_ % 2 == 0)
    .map(b => (b - 7).toByte)
    .zip(iarrayInput.map(b => (b + 100).toByte))
    .map((a, b) => (a + b).toByte)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toByte)
    .flatMap(x => IArray(x, (x + 3).toByte))
    .filter(_ != 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def farray(): Int = farrayInput
    .flatMap(x => FArray(x, (x + 1).toByte))
    .filter(_ % 3 != 0)
    .map(b => (b * 2).toByte)
    .flatMap(x => FArray(x, (x ^ 5).toByte))
    .filter(_ % 2 == 0)
    .map(b => (b - 7).toByte)
    .zip(farrayInput.map(b => (b + 100).toByte))
    .map((a, b) => (a + b).toByte)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toByte)
    .flatMap(x => FArray(x, (x + 3).toByte))
    .filter(_ != 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def fs2chunk(): Int = fs2ChunkInput
    .flatMap(x => fs2.Chunk(x, (x + 1).toByte))
    .filter(_ % 3 != 0)
    .map(b => (b * 2).toByte)
    .flatMap(x => fs2.Chunk(x, (x ^ 5).toByte))
    .filter(_ % 2 == 0)
    .map(b => (b - 7).toByte)
    .zip(fs2ChunkInput.map(b => (b + 100).toByte))
    .map((a, b) => (a + b).toByte)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toByte)
    .flatMap(x => fs2.Chunk(x, (x + 3).toByte))
    .filter(_ != 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def ziochunk(): Int = zioChunkInput
    .flatMap(x => zio.Chunk(x, (x + 1).toByte))
    .filter(_ % 3 != 0)
    .map(b => (b * 2).toByte)
    .flatMap(x => zio.Chunk(x, (x ^ 5).toByte))
    .filter(_ % 2 == 0)
    .map(b => (b - 7).toByte)
    .zip(zioChunkInput.map(b => (b + 100).toByte))
    .map((a, b) => (a + b).toByte)
    .zipWithIndex
    .filter((v, i) => (v + i) % 4 != 0)
    .map((v, i) => (v - i).toByte)
    .flatMap(x => zio.Chunk(x, (x + 3).toByte))
    .filter(_ != 0)
    .foldLeft(0)((acc, x) => acc + x)

  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(b => (b + 100).toByte)
    farrayInput.fuse
      .flatMap(x => FArray(x, (x + 1).toByte))
      .filter(_ % 3 != 0)
      .map(b => (b * 2).toByte)
      .flatMap(x => FArray(x, (x ^ 5).toByte))
      .filter(_ % 2 == 0)
      .map(b => (b - 7).toByte)
      .zip(zipSrc)
      .map((a, b) => (a + b).toByte)
      .zipWithIndex
      .filter((v, i) => (v + i) % 4 != 0)
      .map((v, i) => (v - i).toByte)
      .flatMap(x => FArray(x, (x + 3).toByte))
      .filter(_ != 0)
      .foldLeft(0)((acc, x) => acc + x)
  }
}
