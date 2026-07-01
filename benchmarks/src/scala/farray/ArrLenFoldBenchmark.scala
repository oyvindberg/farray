package farray

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

// ============================================================================================
// Faithful emulation of the REAL generated fold method shape, to test the leaf-loop bound choice
// inside a realistic method rather than an isolated micro-loop. The real `reduceLeaf*` method is:
//   xs match { case Empty => ...; case One => ...; case Arr => <leaf loop>; case Slice => ...; case _ => tree }
// with the body calling a realized SAM (`f.apply(acc, a(i))`) — NOT a raw `+`. The surrounding match +
// megamorphic dispatch + the SAM interface call are exactly the context in which HotSpot's loop
// predication (which makes a `while (i < leaf.length)` field-bounded loop drop its bounds check) might
// behave differently than in a bare micro-loop. The project's current fold loops on `a.length` (the
// ARRAY length) on the stated belief that a `leaf.length` FIELD bound "leaks a bounds check that blocks
// vectorization". This benchmark measures whether that belief holds once leaves may carry slack
// (length < arr.length), across bound shapes / directions / element kinds.
//
// FAIRNESS: every variant folds EXACTLY `size` elements. `arrLen*` runs over an exactly-sized array
// (arr.length == size) — the ideal, JIT-recognized baseline. `fieldLen*` / `compound*` / `min*` run over
// a 2x-slack array (arr.length == 2*size, logical length == size) — the actual "avoid the copy" use case.
// If a slack variant ties the exact `arrLen` baseline, that bound shape is safe to adopt everywhere.
// ============================================================================================

/** Realized SAM matching the generated fold's `f.apply(acc, x)` — Int accumulator, Int element. */
abstract class NdIntFold { def apply(acc: Int, x: Int): Int }

/** Realized SAM for the Ref path — Int accumulator, Object element (leaf stores Object[], like RefArr). */
abstract class NdRefFold { def apply(acc: Int, x: Object): Int }

/** FBase-shaped node hierarchy. Only `IntLeaf`/`RefLeaf` are exercised at runtime; the other cases exist so the fold methods have the real method's breadth
  * (match arms + code size).
  */
sealed abstract class Nd { def length: Int }
final class IntLeaf(val arr: Array[Int], val len: Int) extends Nd { def length: Int = len }
final class RefLeaf(val arr: Array[Object], val len: Int) extends Nd { def length: Int = len }
final class NdOne(val elem: Int) extends Nd { def length: Int = 1 }
final class NdConcat(val l: Nd, val r: Nd) extends Nd { def length: Int = l.length + r.length }
final class NdSlice(val base: Nd, val off: Int, val len: Int) extends Nd { def length: Int = len }
final class NdReverse(val base: Nd) extends Nd { def length: Int = base.length }
final object NdEmpty extends Nd { def length: Int = 0 }

object NdFold {
  // ---- Int, forward ----
  def intArrLenFwd(xs: Nd, z: Int, f: NdIntFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => f.apply(z, o.elem)
    case leaf: IntLeaf =>
      val a = leaf.arr; val n = a.length; var acc = z; var i = 0
      while (i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => var acc = z; var i = 0; while (i < s.len) { acc = f.apply(acc, i); i += 1 }; acc
    case c: NdConcat  => intArrLenFwd(c.r, intArrLenFwd(c.l, z, f), f)
    case r: NdReverse => intArrLenFwd(r.base, z, f)
    case _            => z
  }
  def intFieldLenFwd(xs: Nd, z: Int, f: NdIntFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => f.apply(z, o.elem)
    case leaf: IntLeaf =>
      val a = leaf.arr; val n = leaf.len; var acc = z; var i = 0
      while (i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => var acc = z; var i = 0; while (i < s.len) { acc = f.apply(acc, i); i += 1 }; acc
    case c: NdConcat  => intFieldLenFwd(c.r, intFieldLenFwd(c.l, z, f), f)
    case r: NdReverse => intFieldLenFwd(r.base, z, f)
    case _            => z
  }
  def intCompoundFwd(xs: Nd, z: Int, f: NdIntFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => f.apply(z, o.elem)
    case leaf: IntLeaf =>
      val a = leaf.arr; val n = leaf.len; var acc = z; var i = 0
      while (i < a.length && i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => var acc = z; var i = 0; while (i < s.len) { acc = f.apply(acc, i); i += 1 }; acc
    case c: NdConcat  => intCompoundFwd(c.r, intCompoundFwd(c.l, z, f), f)
    case r: NdReverse => intCompoundFwd(r.base, z, f)
    case _            => z
  }
  def intMinFwd(xs: Nd, z: Int, f: NdIntFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => f.apply(z, o.elem)
    case leaf: IntLeaf =>
      val a = leaf.arr; val n = if (a.length < leaf.len) a.length else leaf.len; var acc = z; var i = 0
      while (i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => var acc = z; var i = 0; while (i < s.len) { acc = f.apply(acc, i); i += 1 }; acc
    case c: NdConcat  => intMinFwd(c.r, intMinFwd(c.l, z, f), f)
    case r: NdReverse => intMinFwd(r.base, z, f)
    case _            => z
  }

  // ---- Int, backward ----
  def intArrLenBwd(xs: Nd, z: Int, f: NdIntFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => f.apply(z, o.elem)
    case leaf: IntLeaf =>
      val a = leaf.arr; val n = a.length; var acc = z; var i = n - 1
      while (i >= 0) { acc = f.apply(acc, a(i)); i -= 1 }; acc
    case s: NdSlice   => var acc = z; var i = s.len - 1; while (i >= 0) { acc = f.apply(acc, i); i -= 1 }; acc
    case c: NdConcat  => intArrLenBwd(c.l, intArrLenBwd(c.r, z, f), f)
    case r: NdReverse => intArrLenBwd(r.base, z, f)
    case _            => z
  }
  def intFieldLenBwd(xs: Nd, z: Int, f: NdIntFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => f.apply(z, o.elem)
    case leaf: IntLeaf =>
      val a = leaf.arr; val n = leaf.len; var acc = z; var i = n - 1
      while (i >= 0) { acc = f.apply(acc, a(i)); i -= 1 }; acc
    case s: NdSlice   => var acc = z; var i = s.len - 1; while (i >= 0) { acc = f.apply(acc, i); i -= 1 }; acc
    case c: NdConcat  => intFieldLenBwd(c.l, intFieldLenBwd(c.r, z, f), f)
    case r: NdReverse => intFieldLenBwd(r.base, z, f)
    case _            => z
  }

  // ---- Ref (Object[]), forward — sums String lengths, like a real Ref fold ----
  def refArrLenFwd(xs: Nd, z: Int, f: NdRefFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => z
    case leaf: RefLeaf =>
      val a = leaf.arr; val n = a.length; var acc = z; var i = 0
      while (i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => z
    case c: NdConcat  => refArrLenFwd(c.r, refArrLenFwd(c.l, z, f), f)
    case r: NdReverse => refArrLenFwd(r.base, z, f)
    case _            => z
  }
  def refFieldLenFwd(xs: Nd, z: Int, f: NdRefFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => z
    case leaf: RefLeaf =>
      val a = leaf.arr; val n = leaf.len; var acc = z; var i = 0
      while (i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => z
    case c: NdConcat  => refFieldLenFwd(c.r, refFieldLenFwd(c.l, z, f), f)
    case r: NdReverse => refFieldLenFwd(r.base, z, f)
    case _            => z
  }
  def refCompoundFwd(xs: Nd, z: Int, f: NdRefFold): Int = xs match {
    case NdEmpty       => z
    case o: NdOne      => z
    case leaf: RefLeaf =>
      val a = leaf.arr; val n = leaf.len; var acc = z; var i = 0
      while (i < a.length && i < n) { acc = f.apply(acc, a(i)); i += 1 }; acc
    case s: NdSlice   => z
    case c: NdConcat  => refCompoundFwd(c.r, refCompoundFwd(c.l, z, f), f)
    case r: NdReverse => refCompoundFwd(r.base, z, f)
    case _            => z
  }
}

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array(
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC",
    "-XX:-UseAdaptiveSizePolicy",
    "-XX:MaxInlineLevel=20",
    "-XX:InlineSmallCode=1500",
    "-XX:+AlwaysPreTouch"
  )
)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ArrLenFoldBenchmark {
  @Param(Array("100", "1000", "10000", "100000"))
  var size: Int = 1000

  var exactInt: IntLeaf = _ // arr.length == size
  var slackInt: IntLeaf = _ // arr.length == 2*size, len == size
  var exactRef: RefLeaf = _
  var slackRef: RefLeaf = _

  val sumInt: NdIntFold = new NdIntFold { def apply(acc: Int, x: Int): Int = acc + x }
  val sumRefLen: NdRefFold = new NdRefFold { def apply(acc: Int, x: Object): Int = acc + x.asInstanceOf[String].length }

  @Setup def setup(): Unit = {
    exactInt = new IntLeaf(Array.tabulate(size)(i => i), size)
    slackInt = new IntLeaf(Array.tabulate(size * 2)(i => i), size)
    val refExact = Array.tabulate[Object](size)(i => ("s" + i): Object)
    val refSlack = Array.tabulate[Object](size * 2)(i => ("s" + i): Object)
    exactRef = new RefLeaf(refExact, size)
    slackRef = new RefLeaf(refSlack, size)
  }

  // Int forward: exact/arrLen is the ideal baseline; the three slack variants must tie it.
  @Benchmark def intArrLenExact(): Int = NdFold.intArrLenFwd(exactInt, 0, sumInt)
  @Benchmark def intFieldLenSlack(): Int = NdFold.intFieldLenFwd(slackInt, 0, sumInt)
  @Benchmark def intCompoundSlack(): Int = NdFold.intCompoundFwd(slackInt, 0, sumInt)
  @Benchmark def intMinSlack(): Int = NdFold.intMinFwd(slackInt, 0, sumInt)

  // Int backward.
  @Benchmark def intArrLenBwdExact(): Int = NdFold.intArrLenBwd(exactInt, 0, sumInt)
  @Benchmark def intFieldLenBwdSlack(): Int = NdFold.intFieldLenBwd(slackInt, 0, sumInt)

  // Ref (String) forward.
  @Benchmark def refArrLenExact(): Int = NdFold.refArrLenFwd(exactRef, 0, sumRefLen)
  @Benchmark def refFieldLenSlack(): Int = NdFold.refFieldLenFwd(slackRef, 0, sumRefLen)
  @Benchmark def refCompoundSlack(): Int = NdFold.refCompoundFwd(slackRef, 0, sumRefLen)
}
