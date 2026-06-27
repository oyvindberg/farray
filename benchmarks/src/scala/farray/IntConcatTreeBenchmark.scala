package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Adversarial concat-tree stress, three shapes, each followed by .map(_ + 1) to force materialization:
//  - leftConcat:  current = leaf ++ current  (left-growing chain)
//  - rightConcat: current = current ++ leaf  (right-growing chain)
//  - balanced:    a balanced binary concat tree built recursively
// A build-only chain is meaningless here: farray's ++ is an O(1) lazy tree node, so without a traversal
// farray "wins" by deferring all work. The trailing .map forces every impl to produce a real traversed
// result, making the comparison fair — farray's O(1)-node build advantage shows, but it must still walk
// its tree.
// Tree-backed structures (FArray, zio.Chunk, fs2.Chunk, Vector) should handle these far better than
// flat copies (IArray, List).
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class IntConcatTreeBenchmark:
  @Param(Array("10000")) var size: Int = 10000

  // --- balanced helpers: rec(n) builds a tree of n single-element leaves via ++ ---
  private def farrayRec(n: Int): FArray[Int] = if n <= 1 then FArray(1) else farrayRec(n / 2) ++ farrayRec(n - n / 2)
  private def iarrayRec(n: Int): IArray[Int] = if n <= 1 then IArray(1) else iarrayRec(n / 2) ++ iarrayRec(n - n / 2)
  private def listRec(n: Int): List[Int] = if n <= 1 then List(1) else listRec(n / 2) ++ listRec(n - n / 2)
  private def vectorRec(n: Int): Vector[Int] = if n <= 1 then Vector(1) else vectorRec(n / 2) ++ vectorRec(n - n / 2)
  private def zioRec(n: Int): zio.Chunk[Int] = if n <= 1 then zio.Chunk.single(1) else zioRec(n / 2) ++ zioRec(n - n / 2)
  private def fs2Rec(n: Int): fs2.Chunk[Int] = if n <= 1 then fs2.Chunk.singleton(1) else fs2Rec(n / 2) ++ fs2Rec(n - n / 2)

  // --- leftConcat ---
  @Benchmark def farray_leftConcat(): FArray[Int] =
    val leaf = FArray(1); var c: FArray[Int] = FArray(0); var i = 0
    while i < size do { c = leaf ++ c; i += 1 }; c.map(_ + 1)
  @Benchmark def iarray_leftConcat(): IArray[Int] =
    val leaf = IArray(1); var c: IArray[Int] = IArray(0); var i = 0
    while i < size do { c = leaf ++ c; i += 1 }; c.map(_ + 1)
  @Benchmark def list_leftConcat(): List[Int] =
    val leaf = List(1); var c: List[Int] = List(0); var i = 0
    while i < size do { c = leaf ++ c; i += 1 }; c.map(_ + 1)
  @Benchmark def vector_leftConcat(): Vector[Int] =
    val leaf = Vector(1); var c: Vector[Int] = Vector(0); var i = 0
    while i < size do { c = leaf ++ c; i += 1 }; c.map(_ + 1)
  @Benchmark def ziochunk_leftConcat(): zio.Chunk[Int] =
    val leaf = zio.Chunk.single(1); var c: zio.Chunk[Int] = zio.Chunk.single(0); var i = 0
    while i < size do { c = leaf ++ c; i += 1 }; c.map(_ + 1)
  @Benchmark def fs2chunk_leftConcat(): fs2.Chunk[Int] =
    val leaf = fs2.Chunk.singleton(1); var c: fs2.Chunk[Int] = fs2.Chunk.singleton(0); var i = 0
    while i < size do { c = leaf ++ c; i += 1 }; c.map(_ + 1)

  // --- rightConcat ---
  @Benchmark def farray_rightConcat(): FArray[Int] =
    val leaf = FArray(1); var c: FArray[Int] = FArray(0); var i = 0
    while i < size do { c = c ++ leaf; i += 1 }; c.map(_ + 1)
  @Benchmark def iarray_rightConcat(): IArray[Int] =
    val leaf = IArray(1); var c: IArray[Int] = IArray(0); var i = 0
    while i < size do { c = c ++ leaf; i += 1 }; c.map(_ + 1)
  @Benchmark def list_rightConcat(): List[Int] =
    val leaf = List(1); var c: List[Int] = List(0); var i = 0
    while i < size do { c = c ++ leaf; i += 1 }; c.map(_ + 1)
  @Benchmark def vector_rightConcat(): Vector[Int] =
    val leaf = Vector(1); var c: Vector[Int] = Vector(0); var i = 0
    while i < size do { c = c ++ leaf; i += 1 }; c.map(_ + 1)
  @Benchmark def ziochunk_rightConcat(): zio.Chunk[Int] =
    val leaf = zio.Chunk.single(1); var c: zio.Chunk[Int] = zio.Chunk.single(0); var i = 0
    while i < size do { c = c ++ leaf; i += 1 }; c.map(_ + 1)
  @Benchmark def fs2chunk_rightConcat(): fs2.Chunk[Int] =
    val leaf = fs2.Chunk.singleton(1); var c: fs2.Chunk[Int] = fs2.Chunk.singleton(0); var i = 0
    while i < size do { c = c ++ leaf; i += 1 }; c.map(_ + 1)

  // --- balanced ---
  @Benchmark def farray_balanced(): FArray[Int] = farrayRec(size).map(_ + 1)
  @Benchmark def iarray_balanced(): IArray[Int] = iarrayRec(size).map(_ + 1)
  @Benchmark def list_balanced(): List[Int] = listRec(size).map(_ + 1)
  @Benchmark def vector_balanced(): Vector[Int] = vectorRec(size).map(_ + 1)
  @Benchmark def ziochunk_balanced(): zio.Chunk[Int] = zioRec(size).map(_ + 1)
  @Benchmark def fs2chunk_balanced(): fs2.Chunk[Int] = fs2Rec(size).map(_ + 1)
