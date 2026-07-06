package farray

import org.openjdk.jmh.annotations.Benchmark

// Construction from a rule: `tabulate` (element from its index) and `fill` (a constant). Building a
// sequence is inherently allocating — you cannot have one without allocating its storage — so the
// honest baseline is IArray (a raw array); the win to look for is over the boxing collections, which
// pay a box and a node per element on top of the storage. FArray builds a flat primitive leaf, so it
// tracks the raw array and leaves List/Vector behind.
class ConstructIntBenchmark extends IntInputs {
  @Benchmark def farray_tabulate(): FArray[Int] = FArray.tabulate(size)(_ * 2)
  @Benchmark def iarray_tabulate(): IArray[Int] = IArray.tabulate(size)(_ * 2)
  @Benchmark def list_tabulate(): List[Int] = List.tabulate(size)(_ * 2)
  @Benchmark def vector_tabulate(): Vector[Int] = Vector.tabulate(size)(_ * 2)

  @Benchmark def farray_fill(): FArray[Int] = FArray.fill(size)(7)
  @Benchmark def iarray_fill(): IArray[Int] = IArray.fill(size)(7)
  @Benchmark def list_fill(): List[Int] = List.fill(size)(7)
  @Benchmark def vector_fill(): Vector[Int] = Vector.fill(size)(7)
}

// Reference twin. `tabulate` here also exercises FArray's typed-backing path: given a ClassTag it
// builds a real `String[]` leaf rather than an `Object[]`. Every element allocates a String, so the
// structural difference is a smaller share of the total than on Int, but the ordering holds.
class ConstructStrBenchmark extends Inputs {
  @Benchmark def farray_tabulate(): FArray[String] = FArray.tabulate(size)(i => i.toString)
  @Benchmark def iarray_tabulate(): IArray[String] = IArray.tabulate(size)(i => i.toString)
  @Benchmark def list_tabulate(): List[String] = List.tabulate(size)(i => i.toString)
  @Benchmark def vector_tabulate(): Vector[String] = Vector.tabulate(size)(i => i.toString)

  @Benchmark def farray_fill(): FArray[String] = FArray.fill(size)("x")
  @Benchmark def iarray_fill(): IArray[String] = IArray.fill(size)("x")
  @Benchmark def list_fill(): List[String] = List.fill(size)("x")
  @Benchmark def vector_fill(): Vector[String] = Vector.fill(size)("x")
}
