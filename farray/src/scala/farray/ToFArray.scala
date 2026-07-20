package farray

/** `import farray.toFArray` — build an `FArray[A]` from any `IterableOnce[A]`.
  *
  * `inline` so the kind dispatch inside `FArray.fromIterableOnce` resolves at the call site: a primitive element type lands in a primitive leaf (an
  * `FArray[Int]` really is an `IntArr`, so the Int-specialized ops that later run on it work); references build unboxed into a `RefArr`.
  *
  * Fast paths (see `FArray.fromIterableOnce`): an `FArraySeq` (the O(1) `toSeq` view) unwraps straight back to its backing core with no copy; array-backed
  * `IndexedSeq`s (`ArraySeq`, `ArrayBuffer`, `Vector`, …) are sized then indexed; `List`/`ListBuffer`/`Iterator`/views drain their iterator into the
  * kind-specialized builder.
  */
extension [A](it: IterableOnce[A]) inline def toFArray: FArray[A] = FArray.fromIterableOnce(it)
