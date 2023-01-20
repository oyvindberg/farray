package farray

final class AsIndexedSeq[A <: AnyRef](toFarray: FArray[A]) extends IndexedSeq[A] {
  def apply(v1: Int): A = toFarray.apply(v1)
  def length: Int = toFarray.length
}
