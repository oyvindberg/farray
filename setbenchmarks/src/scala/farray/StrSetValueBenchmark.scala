package farray

import org.openjdk.jmh.annotations.Benchmark

/** Value ops for String: equals (two equal sets — full compare), order-independent hashCode, subsetOf (HALF ⊆ A). */
class StrSetEqualsBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = fsetA === fsetACopy
  @Benchmark def scalaset(): Boolean = sSetA == sSetACopy
  @Benchmark def scalamut(): Boolean = smSetA == smSetACopy
  @Benchmark def fastutil(): Boolean = fuA == fuACopy
}

class StrSetSetHashBenchmark extends StrSetInputs {
  @Benchmark def fset(): Int = fsetA.setHashCode
  @Benchmark def scalaset(): Int = sSetA.hashCode
  @Benchmark def scalamut(): Int = smSetA.hashCode
  @Benchmark def fastutil(): Int = fuA.hashCode
}

class StrSetSubsetOfBenchmark extends StrSetInputs {
  @Benchmark def fset(): Boolean = fsetHalf.subsetOf(fsetA)
  @Benchmark def scalaset(): Boolean = sSetHalf.subsetOf(sSetA)
  @Benchmark def fastutil(): Boolean = fuA.containsAll(fuHalf)
}
