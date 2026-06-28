package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Char pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Char; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed CharArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class CharAllOpsBench extends CharInputs {

  @Benchmark def list(): Int = listInput
    .flatMap(c => List(c, c.toUpper))
    .filter(_ != 'b')
    .map(c => (c + 1).toChar)
    .flatMap(c => List(c, c.toLower))
    .filter(_ != 'z')
    .map(c => (c ^ 2).toChar)
    .zip(listInput.map(c => (c + 1).toChar))
    .map((a, b) => (a + b).toChar)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => (v + i).toChar)
    .flatMap(c => List(c, c.toUpper))
    .filter(_ != 'q')
    .foldLeft(0)((acc, x) => acc + x.toInt)

  @Benchmark def vector(): Int = vectorInput
    .flatMap(c => Vector(c, c.toUpper))
    .filter(_ != 'b')
    .map(c => (c + 1).toChar)
    .flatMap(c => Vector(c, c.toLower))
    .filter(_ != 'z')
    .map(c => (c ^ 2).toChar)
    .zip(vectorInput.map(c => (c + 1).toChar))
    .map((a, b) => (a + b).toChar)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => (v + i).toChar)
    .flatMap(c => Vector(c, c.toUpper))
    .filter(_ != 'q')
    .foldLeft(0)((acc, x) => acc + x.toInt)

  @Benchmark def iarray(): Int = iarrayInput
    .flatMap(c => IArray(c, c.toUpper))
    .filter(_ != 'b')
    .map(c => (c + 1).toChar)
    .flatMap(c => IArray(c, c.toLower))
    .filter(_ != 'z')
    .map(c => (c ^ 2).toChar)
    .zip(iarrayInput.map(c => (c + 1).toChar))
    .map((a, b) => (a + b).toChar)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => (v + i).toChar)
    .flatMap(c => IArray(c, c.toUpper))
    .filter(_ != 'q')
    .foldLeft(0)((acc, x) => acc + x.toInt)

  @Benchmark def farray(): Int = farrayInput
    .flatMap(c => FArray(c, c.toUpper))
    .filter(_ != 'b')
    .map(c => (c + 1).toChar)
    .flatMap(c => FArray(c, c.toLower))
    .filter(_ != 'z')
    .map(c => (c ^ 2).toChar)
    .zip(farrayInput.map(c => (c + 1).toChar))
    .map((a, b) => (a + b).toChar)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => (v + i).toChar)
    .flatMap(c => FArray(c, c.toUpper))
    .filter(_ != 'q')
    .foldLeft(0)((acc, x) => acc + x.toInt)

  @Benchmark def fs2chunk(): Int = fs2ChunkInput
    .flatMap(c => fs2.Chunk(c, c.toUpper))
    .filter(_ != 'b')
    .map(c => (c + 1).toChar)
    .flatMap(c => fs2.Chunk(c, c.toLower))
    .filter(_ != 'z')
    .map(c => (c ^ 2).toChar)
    .zip(fs2ChunkInput.map(c => (c + 1).toChar))
    .map((a, b) => (a + b).toChar)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => (v + i).toChar)
    .flatMap(c => fs2.Chunk(c, c.toUpper))
    .filter(_ != 'q')
    .foldLeft(0)((acc, x) => acc + x.toInt)

  @Benchmark def ziochunk(): Int = zioChunkInput
    .flatMap(c => zio.Chunk(c, c.toUpper))
    .filter(_ != 'b')
    .map(c => (c + 1).toChar)
    .flatMap(c => zio.Chunk(c, c.toLower))
    .filter(_ != 'z')
    .map(c => (c ^ 2).toChar)
    .zip(zioChunkInput.map(c => (c + 1).toChar))
    .map((a, b) => (a + b).toChar)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => (v + i).toChar)
    .flatMap(c => zio.Chunk(c, c.toUpper))
    .filter(_ != 'q')
    .foldLeft(0)((acc, x) => acc + x.toInt)

  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(c => (c + 1).toChar)
    farrayInput.fuse
      .flatMap(c => FArray(c, c.toUpper))
      .filter(_ != 'b')
      .map(c => (c + 1).toChar)
      .flatMap(c => FArray(c, c.toLower))
      .filter(_ != 'z')
      .map(c => (c ^ 2).toChar)
      .zip(zipSrc)
      .map((a, b) => (a + b).toChar)
      .zipWithIndex
      .filter((v, i) => i % 4 != 0)
      .map((v, i) => (v + i).toChar)
      .flatMap(c => FArray(c, c.toUpper))
      .filter(_ != 'q')
      .foldLeft(0)((acc, x) => acc + x.toInt)
  }
}
