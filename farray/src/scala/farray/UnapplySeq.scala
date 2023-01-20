package farray

final class UnapplySeq[A <: AnyRef](private val as: FArray[A]) extends AnyVal {
  def lengthCompare(len: Int): Int = as.lengthCompare(len)

  def apply(i: Int): A = as.apply(i)

  def drop(n: Int): AsIndexedSeq[A] = new AsIndexedSeq(as.drop(n))

  def toSeq: AsIndexedSeq[A] = new AsIndexedSeq(as)
}
