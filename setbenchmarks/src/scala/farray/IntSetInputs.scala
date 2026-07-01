package farray

import org.openjdk.jmh.annotations.{Param, Setup}
import scala.collection.immutable.BitSet
import scala.collection.mutable.{BitSet => MBitSet}
import it.unimi.dsi.fastutil.ints.{IntOpenHashSet => FuIntSet}
import com.carrotsearch.hppc.{IntHashSet => HppcIntSet}
import org.eclipse.collections.impl.set.mutable.primitive.{IntHashSet => EcIntSet}
import org.eclipse.collections.api.set.primitive.ImmutableIntSet
import org.eclipse.collections.api.factory.primitive.IntSets
import org.roaringbitmap.RoaringBitmap

/** Int-set benchmark inputs: two overlapping sets `a`,`b` per competitor (for ∪/∩/∖), the raw int[] for the
  * build benchmark, hit/miss probes for contains. Every competitor is built once in @Setup. Element domain is
  * non-negative (0..1.5·size) so the dense BitSet/Roaring competitors are valid. */
abstract class IntSetInputs extends CommonParams {
  @Param(Array("16", "1000", "100000"))
  var size: Int = 1000

  var arrA: Array[Int] = _
  var arrB: Array[Int] = _
  var arrC: Array[Int] = _ // a third overlapping window, for the 3-way-union benchmark
  var hit: Int = _
  var miss: Int = _

  var fsetA: FSetMaterialized[Int] = _; var fsetB: FSetMaterialized[Int] = _; var fsetC: FSetMaterialized[Int] = _ // FSet
  var sSetA: Set[Int] = _; var sSetB: Set[Int] = _; var sSetC: Set[Int] = _ // scala immutable HashSet (boxed CHAMP)
  var immBitA: BitSet = _; var immBitB: BitSet = _; var immBitC: BitSet = _ // scala immutable BitSet (unboxed)
  var juBitA: java.util.BitSet = _; var juBitB: java.util.BitSet = _; var juBitC: java.util.BitSet = _ // java.util.BitSet
  var fuA: FuIntSet = _; var fuB: FuIntSet = _; var fuC: FuIntSet = _ // fastutil IntOpenHashSet
  var hppcA: HppcIntSet = _; var hppcB: HppcIntSet = _; var hppcC: HppcIntSet = _ // HPPC IntHashSet
  var ecMutA: EcIntSet = _; var ecMutB: EcIntSet = _; var ecMutC: EcIntSet = _ // Eclipse mutable IntHashSet
  var ecImmA: ImmutableIntSet = _; var ecImmB: ImmutableIntSet = _ // Eclipse IMMUTABLE int set
  var roarA: RoaringBitmap = _; var roarB: RoaringBitmap = _; var roarC: RoaringBitmap = _ // RoaringBitmap
  // a structurally-equal COPY of A (for equals) and a HALF of A ⊆ A (for subsetOf)
  var fsetACopy: FSetMaterialized[Int] = _; var sSetACopy: Set[Int] = _; var fuACopy: FuIntSet = _; var immBitACopy: BitSet = _
  var fsetHalf: FSetMaterialized[Int] = _; var sSetHalf: Set[Int] = _; var fuHalf: FuIntSet = _; var immBitHalf: BitSet = _
  var probes: Array[Int] = _ // 1024 scrambled probes (~50% hit / 50% miss) — varied lookups defeat branch prediction

  @Setup
  def setup(): Unit = {
    probes = Array.tabulate(1024)(i => (((i.toLong * 2654435761L) & 0x7fffffffL).toInt) % (2 * size))
    arrA = Array.tabulate(size)(i => i)
    arrB = Array.tabulate(size)(i => i + size / 2)
    arrC = Array.tabulate(size)(i => i + size)
    hit = size / 2; miss = 3 * size // hit ∈ A∩B; miss ∉ A∪B∪C and non-negative (BitSets reject negatives)
    fsetA = FSet.fromArray(arrA); fsetB = FSet.fromArray(arrB); fsetC = FSet.fromArray(arrC)
    sSetA = arrA.toSet; sSetB = arrB.toSet; sSetC = arrC.toSet
    immBitA = BitSet(arrA*); immBitB = BitSet(arrB*); immBitC = BitSet(arrC*)
    juBitA = new java.util.BitSet(); arrA.foreach(juBitA.set)
    juBitB = new java.util.BitSet(); arrB.foreach(juBitB.set)
    juBitC = new java.util.BitSet(); arrC.foreach(juBitC.set)
    fuA = new FuIntSet(arrA); fuB = new FuIntSet(arrB); fuC = new FuIntSet(arrC)
    hppcA = HppcIntSet.from(arrA*); hppcB = HppcIntSet.from(arrB*); hppcC = HppcIntSet.from(arrC*)
    ecMutA = new EcIntSet(); ecMutA.addAll(arrA*); ecMutB = new EcIntSet(); ecMutB.addAll(arrB*); ecMutC = new EcIntSet(); ecMutC.addAll(arrC*)
    ecImmA = IntSets.immutable.`with`(arrA*); ecImmB = IntSets.immutable.`with`(arrB*)
    roarA = RoaringBitmap.bitmapOf(arrA*); roarB = RoaringBitmap.bitmapOf(arrB*); roarC = RoaringBitmap.bitmapOf(arrC*)
    fsetACopy = FSet.fromArray(arrA); sSetACopy = arrA.toSet; fuACopy = new FuIntSet(arrA); immBitACopy = BitSet(arrA*)
    val half = arrA.take(scala.math.max(1, size / 2))
    fsetHalf = FSet.fromArray(half); sSetHalf = half.toSet; fuHalf = new FuIntSet(half); immBitHalf = BitSet(half*)
  }
}
