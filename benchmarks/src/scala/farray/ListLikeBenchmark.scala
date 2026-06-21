package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// FArray used exactly like a List: `x :: xs` to build, `case h :: t` to pick apart, via recursion.
// Recursion lives in two objects so `ListSyntax.::` (FArray) and `scala.::` (List) don't collide.

object FArrayRec:
  import farray.ListSyntax.*           // FArray's `::`/`Nil` shadow scala's here
  def buildInt(n: Int): FArray[Int]    = if n == 0 then Nil else n :: buildInt(n - 1)
  def sumInt(xs: FArray[Int]): Int     = xs match { case h :: t => h + sumInt(t); case _ => 0 }
  def buildStr(n: Int): FArray[String] = if n == 0 then Nil else n.toString :: buildStr(n - 1)
  def lenStr(xs: FArray[String]): Int  = xs match { case h :: t => h.length + lenStr(t); case _ => 0 }

object ListRec:
  def buildInt(n: Int): List[Int]    = if n == 0 then Nil else n :: buildInt(n - 1)
  def sumInt(xs: List[Int]): Int     = xs match { case h :: t => h + sumInt(t); case _ => 0 }
  def buildStr(n: Int): List[String] = if n == 0 then Nil else n.toString :: buildStr(n - 1)
  def lenStr(xs: List[String]): Int  = xs match { case h :: t => h.length + lenStr(t); case _ => 0 }

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
