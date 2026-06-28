package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}

/** Quantifies the cost of UNION-TYPED / covariant access on FArray.
  *
  * A statically-typed `FArray[Int | String]` cannot use the specialized, unboxed inline API (`apply`/`map`/`foldLeft` all `summonFrom` a `Repr[A]` that does
  * not exist for a non-`AnyRef` union) — the only working element-read path is the boxed `IndexedSeq` view (`toIndexedSeq`, backed by `applyBoxed`). These
  * benchmarks compare that boxed union path against the homogeneous, specialized `FArray[Int]` (unboxed `int[]`) and `FArray[String]` (`Object[]`) paths, so
  * the boxing tax of union access is visible per-op.
  *
  *   - `unionBoxed` : FArray[Int | String] read through `toIndexedSeq` (the usable covariant path)
  *   - `homInt` : FArray[Int] via the specialized unboxed API (baseline, no boxing)
  *   - `homString` : FArray[String] via the specialized Ref API (reference baseline)
  *
  * The union array is the literal Concat(IntArr int[], RefArr Object[]) you get from `ints ++ strs`.
  */
abstract class CovInputs extends CommonParams {
  @Param(Array("100", "1000", "10000"))
  var size: Int = 1000

  // homogeneous, specialized
  var homIntInput: FArray[Int] = _
  var homStringInput: FArray[String] = _

  // heterogeneous union, read via the boxed Seq view (the only covariant path for a PRIMITIVE union)
  var unionInput: FArray[Int | String] = _
  var unionSeq: IndexedSeq[Int | String] = _

  // `intArray.map(box) ++ stringArray`: box the int side to a reference once, so BOTH halves are
  // <: AnyRef -> the result is FArray[Integer | String], a Ref-union the SPECIALIZED inline API
  // accepts directly (RefRepr resolves). This is the recommended way to merge int+string content.
  var boxedUnionInput: FArray[java.lang.Integer | String] = _

  @Setup
  def setup(): Unit = {
    val half = size / 2
    homIntInput = FArray.tabulate(size)(i => i)
    homStringInput = FArray.tabulate(size)(i => i.toString)
    val ints: FArray[Int] = FArray.tabulate(half)(i => i)
    val strs: FArray[String] = FArray.tabulate(size - half)(i => i.toString)
    unionInput = ints ++ strs // Concat(IntArr, RefArr)
    unionSeq = unionInput.toIndexedSeq // FArraySeq -> applyBoxed
    boxedUnionInput = ints.map(i => java.lang.Integer.valueOf(i)) ++ strs // Concat(RefArr, RefArr)
  }
}

/** foldLeft summing a derived Int from each element. */
class FoldLeftCovarianceBenchmark extends CovInputs {
  // boxed union path: every read boxes the int half to Integer; String half read directly
  @Benchmark def unionBoxed(): Int =
    unionSeq.foldLeft(0)((n, x) => n + (x match { case i: Int => i; case s: String => s.length }))
  // pre-boxed Ref-union via map(box) ++ strs: uses the SPECIALIZED Ref foldLeft (no per-read boxing)
  @Benchmark def boxedUnion(): Int =
    boxedUnionInput.foldLeft(0)((n, x) => n + (x match { case i: java.lang.Integer => i.intValue; case s: String => s.length }))
  // homogeneous specialized (unboxed int[])
  @Benchmark def homInt(): Int = homIntInput.foldLeft(0)((n, x) => n + x)
  // homogeneous Ref
  @Benchmark def homString(): Int = homStringInput.foldLeft(0)((n, x) => n + x.length)
}

/** map producing a String per element. */
class MapCovarianceBenchmark extends CovInputs {
  @Benchmark def unionBoxed(): IndexedSeq[String] =
    unionSeq.map(x => x match { case i: Int => "i" + i; case s: String => "s" + s })
  @Benchmark def boxedUnion(): FArray[String] =
    boxedUnionInput.map(x => x match { case i: java.lang.Integer => "i" + i; case s: String => "s" + s })
  @Benchmark def homInt(): FArray[String] = homIntInput.map(i => "i" + i)
  @Benchmark def homString(): FArray[String] = homStringInput.map(s => "s" + s)
}

/** apply: indexed random-access read at a non-constant index. */
class ApplyCovarianceBenchmark extends CovInputs {
  @Benchmark def unionBoxed(): Int | String = unionSeq(size / 2 + 1) // crosses into the String half
  @Benchmark def boxedUnion(): java.lang.Integer | String = boxedUnionInput(size / 2 + 1)
  @Benchmark def homInt(): Int = homIntInput(size / 2 + 1)
  @Benchmark def homString(): String = homStringInput(size / 2 + 1)
}
