package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec

// FArray used exactly like a List: `x :: xs` to build, `case h :: t` to pick apart, via recursion.
// Recursion lives in two objects so `ListSyntax.::` (FArray) and `scala.::` (List) don't collide.

object FArrayRec:
  import farray.ListSyntax.*           // FArray's `::`/`Nil` shadow scala's here
  def buildInt(n: Int): FArray[Int]    = if n == 0 then Nil else n :: buildInt(n - 1)
  def sumInt(xs: FArray[Int]): Int     = xs match { case h :: t => h + sumInt(t); case _ => 0 }
  def buildStr(n: Int): FArray[String] = if n == 0 then Nil else n.toString :: buildStr(n - 1)
  def lenStr(xs: FArray[String]): Int  = xs match { case h :: t => h.length + lenStr(t); case _ => 0 }
  // a map (or two) between build and pick-apart: map materialises the chain to a leaf, so pick-apart
  // then walks a leaf (tail = drop(1) = SliceNode), not a Prepend (tail = base).
  def sum1MapInt(xs: FArray[Int]): Int   = sumInt(xs.map(_ + 1))
  def sum2MapInt(xs: FArray[Int]): Int   = sumInt(xs.map(_ + 1).map(_ * 2))
  def len1MapStr(xs: FArray[String]): Int = lenStr(xs.map(_ + "x"))
  def len2MapStr(xs: FArray[String]): Int = lenStr(xs.map(_ + "x").map(_ + "y"))

object ListRec:
  def buildInt(n: Int): List[Int]    = if n == 0 then Nil else n :: buildInt(n - 1)
  def sumInt(xs: List[Int]): Int     = xs match { case h :: t => h + sumInt(t); case _ => 0 }
  def buildStr(n: Int): List[String] = if n == 0 then Nil else n.toString :: buildStr(n - 1)
  def lenStr(xs: List[String]): Int  = xs match { case h :: t => h.length + lenStr(t); case _ => 0 }
  def sum1MapInt(xs: List[Int]): Int   = sumInt(xs.map(_ + 1))
  def sum2MapInt(xs: List[Int]): Int   = sumInt(xs.map(_ + 1).map(_ * 2))
  def len1MapStr(xs: List[String]): Int = lenStr(xs.map(_ + "x"))
  def len2MapStr(xs: List[String]): Int = lenStr(xs.map(_ + "x").map(_ + "y"))

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ListLikeIntBenchmark:
  @Param(Array("1000")) var size: Int = 1000
  var farrayInput: FArray[Int] = _
  var listInput: List[Int] = _
  @Setup def setup(): Unit = { farrayInput = FArrayRec.buildInt(size); listInput = ListRec.buildInt(size) }
  @Benchmark def farray_build(): FArray[Int] = FArrayRec.buildInt(size)
  @Benchmark def list_build(): List[Int]     = ListRec.buildInt(size)
  @Benchmark def farray_sum(): Int = FArrayRec.sumInt(farrayInput)
  @Benchmark def list_sum(): Int   = ListRec.sumInt(listInput)
  @Benchmark def farray_1mapSum(): Int = FArrayRec.sum1MapInt(farrayInput)
  @Benchmark def list_1mapSum(): Int   = ListRec.sum1MapInt(listInput)
  @Benchmark def farray_2mapSum(): Int = FArrayRec.sum2MapInt(farrayInput)
  @Benchmark def list_2mapSum(): Int   = ListRec.sum2MapInt(listInput)

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ListLikeStringBenchmark:
  @Param(Array("1000")) var size: Int = 1000
  var farrayInput: FArray[String] = _
  var listInput: List[String] = _
  @Setup def setup(): Unit = { farrayInput = FArrayRec.buildStr(size); listInput = ListRec.buildStr(size) }
  @Benchmark def farray_build(): FArray[String] = FArrayRec.buildStr(size)
  @Benchmark def list_build(): List[String]     = ListRec.buildStr(size)
  @Benchmark def farray_len(): Int = FArrayRec.lenStr(farrayInput)
  @Benchmark def list_len(): Int   = ListRec.lenStr(listInput)
  @Benchmark def farray_1mapLen(): Int = FArrayRec.len1MapStr(farrayInput)
  @Benchmark def list_1mapLen(): Int   = ListRec.len1MapStr(listInput)
  @Benchmark def farray_2mapLen(): Int = FArrayRec.len2MapStr(farrayInput)
  @Benchmark def list_2mapLen(): Int   = ListRec.len2MapStr(listInput)

// Scaling to 1e5 / 1e6: build iteratively and pick apart tail-recursively (natural `h + rec(t)` would
// stack-overflow past ~1e4 on both List and FArray). Int (unboxed win) and String (reference parity) —
// String build cons-es a fixed string, so it measures node/cell structure, not string creation. Fair both sides.
object FArrayScale:
  import farray.ListSyntax.*
  private val s = "abc"
  def buildInt(n: Int): FArray[Int]    = { var xs: FArray[Int] = Nil; var i = 0; while i < n do { xs = i :: xs; i += 1 }; xs }
  @tailrec def sumInt(xs: FArray[Int], acc: Int): Int = xs match { case h :: t => sumInt(t, acc + h); case _ => acc }
  def buildStr(n: Int): FArray[String] = { var xs: FArray[String] = Nil; var i = 0; while i < n do { xs = s :: xs; i += 1 }; xs }
  @tailrec def sumLen(xs: FArray[String], acc: Int): Int = xs match { case h :: t => sumLen(t, acc + h.length); case _ => acc }

object ListScale:
  private val s = "abc"
  def buildInt(n: Int): List[Int]    = { var xs: List[Int] = Nil; var i = 0; while i < n do { xs = i :: xs; i += 1 }; xs }
  @tailrec def sumInt(xs: List[Int], acc: Int): Int = xs match { case h :: t => sumInt(t, acc + h); case _ => acc }
  def buildStr(n: Int): List[String] = { var xs: List[String] = Nil; var i = 0; while i < n do { xs = s :: xs; i += 1 }; xs }
  @tailrec def sumLen(xs: List[String], acc: Int): Int = xs match { case h :: t => sumLen(t, acc + h.length); case _ => acc }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ListLikeScalingIntBenchmark:
  @Param(Array("1000", "100000", "1000000")) var size: Int = 1000
  var farrayInput: FArray[Int] = _
  var listInput: List[Int] = _
  @Setup def setup(): Unit = { farrayInput = FArrayScale.buildInt(size); listInput = ListScale.buildInt(size) }
  @Benchmark def farray_build(): FArray[Int] = FArrayScale.buildInt(size)
  @Benchmark def list_build(): List[Int]     = ListScale.buildInt(size)
  @Benchmark def farray_sum(): Int = FArrayScale.sumInt(farrayInput, 0)
  @Benchmark def list_sum(): Int   = ListScale.sumInt(listInput, 0)
  @Benchmark def farray_mapSum(): Int = FArrayScale.sumInt(farrayInput.map(_ + 1), 0)
  @Benchmark def list_mapSum(): Int   = ListScale.sumInt(listInput.map(_ + 1), 0)

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ListLikeScalingStringBenchmark:
  @Param(Array("1000", "100000", "1000000")) var size: Int = 1000
  var farrayInput: FArray[String] = _
  var listInput: List[String] = _
  @Setup def setup(): Unit = { farrayInput = FArrayScale.buildStr(size); listInput = ListScale.buildStr(size) }
  @Benchmark def farray_build(): FArray[String] = FArrayScale.buildStr(size)
  @Benchmark def list_build(): List[String]     = ListScale.buildStr(size)
  @Benchmark def farray_sum(): Int = FArrayScale.sumLen(farrayInput, 0)
  @Benchmark def list_sum(): Int   = ListScale.sumLen(listInput, 0)
  @Benchmark def farray_mapSum(): Int = FArrayScale.sumLen(farrayInput.map(_ + "x"), 0)
  @Benchmark def list_mapSum(): Int   = ListScale.sumLen(listInput.map(_ + "x"), 0)
