package farray

import org.openjdk.jmh.annotations.Benchmark
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}

/** The MANY-query pattern: materialize the 3-way union ONCE, then probe contains n = size times. This is the
  * realistic usage when you query a combination a lot (the lazy IntSetUnion3ContainsBenchmark is the FEW-query
  * moat). With the dense bitmap leaf + word-parallel union, FSet's materialized union is a single bitmap → the
  * follow-up contains are O(1) bit-tests, so FSet should reach BitSet territory (the plan's Step-2 target). */
class IntSetUnion3MatContainsBenchmark extends IntSetInputs {
  @Benchmark def fset(): Int = {
    val u = (fsetA ++ fsetB ++ fsetC).materialize
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def immbitset(): Int = {
    val u = immBitA | immBitB | immBitC
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def jubitset(): Int = {
    val u = juBitA.clone().asInstanceOf[java.util.BitSet]; u.or(juBitB); u.or(juBitC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.get(arrA(i))) h += 1; i += 1 }; h
  }
  @Benchmark def fastutil(): Int = {
    val u = new FuIntSet(fuA); u.addAll(fuB); u.addAll(fuC)
    var h = 0; var i = 0; while (i < arrA.length) { if (u.contains(arrA(i))) h += 1; i += 1 }; h
  }
}
