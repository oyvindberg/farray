package farray

import org.openjdk.jmh.annotations.Benchmark
import scala.collection.immutable.BitSet
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import com.carrotsearch.hppc.{IntHashSet => HppcIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}
import org.eclipse.collections.api.factory.primitive.IntSets
import org.roaringbitmap.RoaringBitmap

/** Build a set from an int[] — the dominant "build-once" cost. */
class IntSetBuildBenchmark extends IntSetInputs {
  @Benchmark def fset(): Any = FSet.fromArray(arrA)
  @Benchmark def scalaset(): Any = arrA.toSet
  @Benchmark def immbitset(): Any = BitSet(arrA*)
  @Benchmark def jubitset(): Any = { val b = new java.util.BitSet(); var i = 0; while (i < arrA.length) { b.set(arrA(i)); i += 1 }; b }
  @Benchmark def fastutil(): Any = new FuIntSet(arrA)
  @Benchmark def hppc(): Any = HppcIntSet.from(arrA*)
  @Benchmark def eclipsemut(): Any = { val s = new EcIntSet(); s.addAll(arrA*); s }
  @Benchmark def eclipseimm(): Any = IntSets.immutable.`with`(arrA*)
  @Benchmark def roaring(): Any = RoaringBitmap.bitmapOf(arrA*)
}
