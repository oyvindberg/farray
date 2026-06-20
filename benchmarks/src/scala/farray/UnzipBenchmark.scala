package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}

// unzip reads each (Int, Int) tuple ONCE and fills both output arrays in a single pass, instead of two
// tabulate passes each re-reading the source. (Tuples box on the way in regardless — that's inherent.)
class UnzipBenchmark extends CommonParams {
  @Param(Array("1", "10", "100", "1000", "10000"))
  var size: Int = 1000

  var farrayInput: FArray[(Int, Int)] = _
  var listInput: List[(Int, Int)] = _
  var vectorInput: Vector[(Int, Int)] = _

  @Setup def setup(): Unit =
    farrayInput = FArray.tabulate(size)(i => (i, i * 2))
    listInput = List.tabulate(size)(i => (i, i * 2))
    vectorInput = Vector.tabulate(size)(i => (i, i * 2))

  @Benchmark def farray(): (FArray[Int], FArray[Int]) = farrayInput.unzip
  @Benchmark def list(): (List[Int], List[Int])       = listInput.unzip
  @Benchmark def vector(): (Vector[Int], Vector[Int]) = vectorInput.unzip
}
