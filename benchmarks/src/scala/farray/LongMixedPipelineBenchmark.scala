package farray

import org.openjdk.jmh.annotations.Benchmark

// A LONG, mixed-stage pipeline run apples-to-apples across all 6 collections. flatMap (×2) · filter ·
// map · zip · zipWithIndex · filter · map · flatMap · filter · foldLeft. Every op exists on every
// collection (no `collect`/`takeWhile`, which fs2/zio lack), every lambda is distinct (no CSE), and the
// pipeline is long enough that (a) each stage allocates a fresh intermediate collection and (b) the
// enclosing method is large. This probes BOTH axes of the hypothesis at once: per-element boxing of the
// primitive payload (Int) AND the intermediate-allocation / inlining-budget cost of a realistic chain.
//
// Each collection runs the IDENTICAL logical transform; the only differences are forced by API shape
// (zip tuple types). The result is a single Int folded out, so the whole chain is consumed.
class LongMixedPipelineIntBenchmark extends IntInputs {

  @Benchmark def list(): Int = {
    listInput
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
      .foldLeft(0)(_ + _)
  }

  @Benchmark def vector(): Int = {
    vectorInput
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
      .foldLeft(0)(_ + _)
  }

  @Benchmark def iarray(): Int = {
    iarrayInput
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
      .foldLeft(0)(_ + _)
  }

  @Benchmark def farray(): Int = {
    farrayInput
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
      .foldLeft(0)(_ + _)
  }

  // The SAME logical pipeline through FArray's `fuse` — one pass, no intermediate collections, no
  // Function1, unboxed Int throughout. Same stages as `farray` above. The `zip` source is hoisted to a
  // `val` so the fuse macro sees a simple reference (it reads the `that` argument off the AST).
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
      .foldLeft(0)(_ + _)
  }

  @Benchmark def fs2chunk(): Int = {
    fs2ChunkInput
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
      .foldLeft(0)(_ + _)
  }

  @Benchmark def ziochunk(): Int = {
    zioChunkInput
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
      .foldLeft(0)(_ + _)
  }
}
