package farray

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

// Does the ClassTag "typed-backing" trick actually pay? A reference FArray built via tabulate/fill
// (with a ClassTag in scope) is backed by a real String[], so `toArray[String]` is a bulk
// System.arraycopy. Built via map/filter, the backing is Object[], so the same toArray is a CHECKED
// arraycopy (per-element ArrayStoreException test copying Object[] -> String[]). This isolates that
// one difference: same elements, same result, only the backing runtime type differs.
@State(Scope.Thread)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ToArrayRefBenchmark {
  @Param(Array("10", "1000", "100000")) var size: Int = 1000

  var farrayTyped: FArray[String] = _ // String[] backing (tabulate)
  var farrayObj: FArray[String] = _ // Object[] backing (mapped)
  var iarrayInput: IArray[String] = _
  var listInput: List[String] = _
  var vectorInput: Vector[String] = _

  @Setup
  def setup(): Unit = {
    farrayTyped = FArray.tabulate(size)(_.toString) // ClassTag[String] in scope -> String[]
    farrayObj = FArray.tabulate(size)(i => i).map(_.toString) // map -> Object[] backing
    iarrayInput = IArray.tabulate(size)(_.toString)
    listInput = List.tabulate(size)(_.toString)
    vectorInput = Vector.tabulate(size)(_.toString)
  }

  @Benchmark def farray_typedBacking(): Array[String] = farrayTyped.toArray
  @Benchmark def farray_objBacking(): Array[String] = farrayObj.toArray
  @Benchmark def iarray(): Array[String] = iarrayInput.toArray
  @Benchmark def list(): Array[String] = listInput.toArray
  @Benchmark def vector(): Array[String] = vectorInput.toArray
}
