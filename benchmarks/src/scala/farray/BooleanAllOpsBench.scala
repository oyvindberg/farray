package farray

import org.openjdk.jmh.annotations.Benchmark

// ONE long, mixed-stage Boolean pipeline run apples-to-apples across all 6 collections + a fused FArray
// variant. flatMap·filter·map·flatMap·filter·map · zip·map · zipWithIndex · filter·map·flatMap·filter · fold.
// Every stage exists on every collection; every lambda is distinct; the chain is long enough that each
// eager stage allocates a fresh intermediate (List/Vector box every Boolean; IArray/Chunk box once their
// shared map/fold goes megamorphic) while FArray stays an unboxed BooleanArr the whole way, and `.fuse`
// runs it in ONE pass with no intermediate collections.
class BooleanAllOpsBench extends BooleanInputs {

  @Benchmark def list(): Int = listInput
    .flatMap(b => List(b, !b))
    .filter(identity)
    .map(!_)
    .flatMap(b => List(b, b & true))
    .filter(b => b | true)
    .map(!_)
    .zip(listInput.map(!_))
    .map((a, b) => a ^ b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v ^ (i % 2 == 0))
    .flatMap(b => List(b, !b))
    .filter(identity)
    .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))

  @Benchmark def vector(): Int = vectorInput
    .flatMap(b => Vector(b, !b))
    .filter(identity)
    .map(!_)
    .flatMap(b => Vector(b, b & true))
    .filter(b => b | true)
    .map(!_)
    .zip(vectorInput.map(!_))
    .map((a, b) => a ^ b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v ^ (i % 2 == 0))
    .flatMap(b => Vector(b, !b))
    .filter(identity)
    .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))

  @Benchmark def iarray(): Int = iarrayInput
    .flatMap(b => IArray(b, !b))
    .filter(identity)
    .map(!_)
    .flatMap(b => IArray(b, b & true))
    .filter(b => b | true)
    .map(!_)
    .zip(iarrayInput.map(!_))
    .map((a, b) => a ^ b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v ^ (i % 2 == 0))
    .flatMap(b => IArray(b, !b))
    .filter(identity)
    .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))

  @Benchmark def farray(): Int = farrayInput
    .flatMap(b => FArray(b, !b))
    .filter(identity)
    .map(!_)
    .flatMap(b => FArray(b, b & true))
    .filter(b => b | true)
    .map(!_)
    .zip(farrayInput.map(!_))
    .map((a, b) => a ^ b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v ^ (i % 2 == 0))
    .flatMap(b => FArray(b, !b))
    .filter(identity)
    .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))

  @Benchmark def fs2chunk(): Int = fs2ChunkInput
    .flatMap(b => fs2.Chunk(b, !b))
    .filter(identity)
    .map(!_)
    .flatMap(b => fs2.Chunk(b, b & true))
    .filter(b => b | true)
    .map(!_)
    .zip(fs2ChunkInput.map(!_))
    .map((a, b) => a ^ b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v ^ (i % 2 == 0))
    .flatMap(b => fs2.Chunk(b, !b))
    .filter(identity)
    .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))

  @Benchmark def ziochunk(): Int = zioChunkInput
    .flatMap(b => zio.Chunk(b, !b))
    .filter(identity)
    .map(!_)
    .flatMap(b => zio.Chunk(b, b & true))
    .filter(b => b | true)
    .map(!_)
    .zip(zioChunkInput.map(!_))
    .map((a, b) => a ^ b)
    .zipWithIndex
    .filter((v, i) => i % 4 != 0)
    .map((v, i) => v ^ (i % 2 == 0))
    .flatMap(b => zio.Chunk(b, !b))
    .filter(identity)
    .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))

  @Benchmark def farrayFused(): Int = {
    val zipSrc = farrayInput.map(!_)
    farrayInput.fuse
      .flatMap(b => FArray(b, !b))
      .filter(identity)
      .map(!_)
      .flatMap(b => FArray(b, b & true))
      .filter(b => b | true)
      .map(!_)
      .zip(zipSrc)
      .map((a, b) => a ^ b)
      .zipWithIndex
      .filter((v, i) => i % 4 != 0)
      .map((v, i) => v ^ (i % 2 == 0))
      .flatMap(b => FArray(b, !b))
      .filter(identity)
      .foldLeft(0)((acc, x) => acc + (if (x) 1 else 0))
  }
}
