package farray

import org.openjdk.jmh.annotations.Benchmark

/** Value ops: equals of two structurally-equal sets (worst case — full compare), order-independent hashCode, and subsetOf (a HALF of A ⊆ A → confirms all).
  */
class IntSetEqualsBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = fsetA === fsetACopy
  @Benchmark def scalaset(): Boolean = sSetA == sSetACopy
  @Benchmark def immbitset(): Boolean = immBitA == immBitACopy
  @Benchmark def fastutil(): Boolean = fuA == fuACopy
}

class IntSetSetHashBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = fsetA.setHashCode
  @Benchmark def scalaset(): Int = sSetA.hashCode
  @Benchmark def immbitset(): Int = immBitA.hashCode
  @Benchmark def fastutil(): Int = fuA.hashCode
}

class IntSetSubsetOfBenchmark extends IntSetInputs {
  @Benchmark def fset(): Boolean = fsetHalf.subsetOf(fsetA)
  @Benchmark def scalaset(): Boolean = sSetHalf.subsetOf(sSetA)
  @Benchmark def immbitset(): Boolean = immBitHalf.subsetOf(immBitA)
  @Benchmark def fastutil(): Boolean = fuA.containsAll(fuHalf)
}
