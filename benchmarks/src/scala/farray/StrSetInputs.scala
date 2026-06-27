package farray

import org.openjdk.jmh.annotations.{Param, Setup}
import scala.collection.mutable.{HashSet => SMHashSet}
import it.unimi.dsi.fastutil.objects.{ObjectOpenHashSet => FuObjSet}
import com.carrotsearch.hppc.{ObjectHashSet => HppcObjSet}
import com.google.common.collect.ImmutableSet
import org.eclipse.collections.impl.set.mutable.UnifiedSet

/** String-set benchmark inputs: two overlapping sets `a`,`b` per competitor, the raw String[] for build, and
  * hit/miss probes. The reference (String) suite — every entry must beat scala immutable.HashSet (CHAMP). */
abstract class StrSetInputs extends CommonParams {
  @Param(Array("16", "1000", "100000"))
  var size: Int = 1000

  var arrA: Array[String] = _
  var arrB: Array[String] = _
  var arrC: Array[String] = _ // a third overlapping window, for the 3-way-union benchmark
  var hit: String = _
  var miss: String = _

  var fsetA: FSetMaterialized[String] = _; var fsetB: FSetMaterialized[String] = _; var fsetC: FSetMaterialized[String] = _ // FSet
  var sSetA: Set[String] = _; var sSetB: Set[String] = _; var sSetC: Set[String] = _ // scala immutable HashSet (CHAMP)
  var smSetA: SMHashSet[String] = _; var smSetB: SMHashSet[String] = _; var smSetC: SMHashSet[String] = _ // scala mutable
  var juA: java.util.HashSet[String] = _; var juB: java.util.HashSet[String] = _; var juC: java.util.HashSet[String] = _ // java.util.HashSet
  var juOfA: java.util.Set[String] = _ // java.util.Set.of (immutable)
  var guavaA: ImmutableSet[String] = _; var guavaB: ImmutableSet[String] = _; var guavaC: ImmutableSet[String] = _ // Guava
  var ecA: UnifiedSet[String] = _; var ecB: UnifiedSet[String] = _; var ecC: UnifiedSet[String] = _ // Eclipse UnifiedSet
  var fuA: FuObjSet[String] = _; var fuB: FuObjSet[String] = _; var fuC: FuObjSet[String] = _ // fastutil
  var hppcA: HppcObjSet[String] = _; var hppcB: HppcObjSet[String] = _; var hppcC: HppcObjSet[String] = _ // HPPC

  @Setup
  def setup(): Unit = {
    arrA = Array.tabulate(size)(i => "k" + i)
    arrB = Array.tabulate(size)(i => "k" + (i + size / 2))
    arrC = Array.tabulate(size)(i => "k" + (i + size))
    hit = "k" + (size / 2); miss = "z" + (2 * size)
    fsetA = FSet.fromArray(arrA); fsetB = FSet.fromArray(arrB); fsetC = FSet.fromArray(arrC)
    sSetA = arrA.toSet; sSetB = arrB.toSet; sSetC = arrC.toSet
    smSetA = SMHashSet(arrA*); smSetB = SMHashSet(arrB*); smSetC = SMHashSet(arrC*)
    juA = new java.util.HashSet[String](); arrA.foreach(juA.add)
    juB = new java.util.HashSet[String](); arrB.foreach(juB.add)
    juC = new java.util.HashSet[String](); arrC.foreach(juC.add)
    juOfA = java.util.Set.of(arrA*)
    guavaA = ImmutableSet.copyOf(arrA); guavaB = ImmutableSet.copyOf(arrB); guavaC = ImmutableSet.copyOf(arrC)
    ecA = UnifiedSet.newSetWith(arrA*); ecB = UnifiedSet.newSetWith(arrB*); ecC = UnifiedSet.newSetWith(arrC*)
    fuA = new FuObjSet[String](arrA); fuB = new FuObjSet[String](arrB); fuC = new FuObjSet[String](arrC)
    hppcA = HppcObjSet.from(arrA*); hppcB = HppcObjSet.from(arrB*); hppcC = HppcObjSet.from(arrC*)
  }
}
