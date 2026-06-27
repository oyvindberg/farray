package farray

/** Zero-copy `IndexedSeq` view over an `FArray`, so `toSeq`/`toIndexedSeq` are O(1) — like `List`/`Vector`, which already *are* Seqs — instead of materialising
  * into a fresh collection. A `Seq` hands back boxed `A` regardless, so element reads go through the (non-specialised) boxed `applyBoxed`; length is direct.
  */
final class FArraySeq[A] private[farray] (private val under: FBase) extends scala.collection.immutable.IndexedSeq[A]:
  def apply(i: Int): A = under.applyBoxed(i).asInstanceOf[A]
  def length: Int = under.length
  override def knownSize: Int = under.length
