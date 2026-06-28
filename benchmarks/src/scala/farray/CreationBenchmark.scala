package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Varargs / small-construction cost at N = 2, 4, 8, 16, 32.
// Fields are set in @Setup so the JIT can't constant-fold the literals away.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class CreationIntBenchmark:
  var a1, a2, a3, a4, a5, a6, a7, a8 = 0
  var a9, a10, a11, a12, a13, a14, a15, a16 = 0
  var a17, a18, a19, a20, a21, a22, a23, a24 = 0
  var a25, a26, a27, a28, a29, a30, a31, a32 = 0

  @Setup def setup(): Unit =
    a1 = 1; a2 = 2; a3 = 3; a4 = 4; a5 = 5; a6 = 6; a7 = 7; a8 = 8
    a9 = 9; a10 = 10; a11 = 11; a12 = 12; a13 = 13; a14 = 14; a15 = 15; a16 = 16
    a17 = 17; a18 = 18; a19 = 19; a20 = 20; a21 = 21; a22 = 22; a23 = 23; a24 = 24
    a25 = 25; a26 = 26; a27 = 27; a28 = 28; a29 = 29; a30 = 30; a31 = 31; a32 = 32

  // ---- N = 2 ----
  @Benchmark def farray_create02(): FArray[Int] = FArray(a1, a2)
  @Benchmark def iarray_create02(): IArray[Int] = IArray(a1, a2)
  @Benchmark def list_create02(): List[Int] = List(a1, a2)
  @Benchmark def vector_create02(): Vector[Int] = Vector(a1, a2)
  @Benchmark def ziochunk_create02(): zio.Chunk[Int] = zio.Chunk(a1, a2)
  @Benchmark def fs2chunk_create02(): fs2.Chunk[Int] = fs2.Chunk(a1, a2)

  // ---- N = 4 ----
  @Benchmark def farray_create04(): FArray[Int] = FArray(a1, a2, a3, a4)
  @Benchmark def iarray_create04(): IArray[Int] = IArray(a1, a2, a3, a4)
  @Benchmark def list_create04(): List[Int] = List(a1, a2, a3, a4)
  @Benchmark def vector_create04(): Vector[Int] = Vector(a1, a2, a3, a4)
  @Benchmark def ziochunk_create04(): zio.Chunk[Int] = zio.Chunk(a1, a2, a3, a4)
  @Benchmark def fs2chunk_create04(): fs2.Chunk[Int] = fs2.Chunk(a1, a2, a3, a4)

  // ---- N = 8 ----
  @Benchmark def farray_create08(): FArray[Int] = FArray(a1, a2, a3, a4, a5, a6, a7, a8)
  @Benchmark def iarray_create08(): IArray[Int] = IArray(a1, a2, a3, a4, a5, a6, a7, a8)
  @Benchmark def list_create08(): List[Int] = List(a1, a2, a3, a4, a5, a6, a7, a8)
  @Benchmark def vector_create08(): Vector[Int] = Vector(a1, a2, a3, a4, a5, a6, a7, a8)
  @Benchmark def ziochunk_create08(): zio.Chunk[Int] = zio.Chunk(a1, a2, a3, a4, a5, a6, a7, a8)
  @Benchmark def fs2chunk_create08(): fs2.Chunk[Int] = fs2.Chunk(a1, a2, a3, a4, a5, a6, a7, a8)

  // ---- N = 16 ----
  @Benchmark def farray_create16(): FArray[Int] =
    FArray(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  @Benchmark def iarray_create16(): IArray[Int] =
    IArray(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  @Benchmark def list_create16(): List[Int] =
    List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  @Benchmark def vector_create16(): Vector[Int] =
    Vector(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  @Benchmark def ziochunk_create16(): zio.Chunk[Int] =
    zio.Chunk(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  @Benchmark def fs2chunk_create16(): fs2.Chunk[Int] =
    fs2.Chunk(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

  // ---- N = 32 ----
  @Benchmark def farray_create32(): FArray[Int] =
    FArray(
      a1,
      a2,
      a3,
      a4,
      a5,
      a6,
      a7,
      a8,
      a9,
      a10,
      a11,
      a12,
      a13,
      a14,
      a15,
      a16,
      a17,
      a18,
      a19,
      a20,
      a21,
      a22,
      a23,
      a24,
      a25,
      a26,
      a27,
      a28,
      a29,
      a30,
      a31,
      a32
    )
  @Benchmark def iarray_create32(): IArray[Int] =
    IArray(
      a1,
      a2,
      a3,
      a4,
      a5,
      a6,
      a7,
      a8,
      a9,
      a10,
      a11,
      a12,
      a13,
      a14,
      a15,
      a16,
      a17,
      a18,
      a19,
      a20,
      a21,
      a22,
      a23,
      a24,
      a25,
      a26,
      a27,
      a28,
      a29,
      a30,
      a31,
      a32
    )
  @Benchmark def list_create32(): List[Int] =
    List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32)
  @Benchmark def vector_create32(): Vector[Int] =
    Vector(
      a1,
      a2,
      a3,
      a4,
      a5,
      a6,
      a7,
      a8,
      a9,
      a10,
      a11,
      a12,
      a13,
      a14,
      a15,
      a16,
      a17,
      a18,
      a19,
      a20,
      a21,
      a22,
      a23,
      a24,
      a25,
      a26,
      a27,
      a28,
      a29,
      a30,
      a31,
      a32
    )
  @Benchmark def ziochunk_create32(): zio.Chunk[Int] =
    zio.Chunk(
      a1,
      a2,
      a3,
      a4,
      a5,
      a6,
      a7,
      a8,
      a9,
      a10,
      a11,
      a12,
      a13,
      a14,
      a15,
      a16,
      a17,
      a18,
      a19,
      a20,
      a21,
      a22,
      a23,
      a24,
      a25,
      a26,
      a27,
      a28,
      a29,
      a30,
      a31,
      a32
    )
  @Benchmark def fs2chunk_create32(): fs2.Chunk[Int] =
    fs2.Chunk(
      a1,
      a2,
      a3,
      a4,
      a5,
      a6,
      a7,
      a8,
      a9,
      a10,
      a11,
      a12,
      a13,
      a14,
      a15,
      a16,
      a17,
      a18,
      a19,
      a20,
      a21,
      a22,
      a23,
      a24,
      a25,
      a26,
      a27,
      a28,
      a29,
      a30,
      a31,
      a32
    )
