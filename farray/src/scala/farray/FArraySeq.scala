package farray

/** Zero-copy `IndexedSeq` view over an `FArray`, so `toSeq`/`toIndexedSeq` are O(1) — like `List`/`Vector`, which already *are* Seqs — instead of materialising
  * into a fresh collection. A `Seq` hands back boxed `A` regardless, so element reads go through the (non-specialised) boxed `applyBoxed`; length is direct.
  */
final class FArraySeq[A] private[farray] (private val under: FBase) extends scala.collection.immutable.IndexedSeq[A]:
  // IndexedSeq contract: out-of-range index throws IndexOutOfBoundsException (parity with List). `applyBoxed`
  // arms for singleton/structural nodes ignore `i`, and a slack-backed leaf has array capacity beyond its
  // logical length, so the bounds check must be here, against the logical length.
  def apply(i: Int): A =
    if i < 0 || i >= under.length then throw new IndexOutOfBoundsException(java.lang.Integer.toString(i))
    else under.applyBoxed(i).asInstanceOf[A]
  def length: Int = under.length
  override def knownSize: Int = under.length

  /** the wrapped core — lets `toFArray` unwrap an `FArraySeq` back to its `FArray` in O(1). */
  private[farray] def fbase: FBase = under
