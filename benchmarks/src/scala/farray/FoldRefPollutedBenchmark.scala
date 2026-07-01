package farray

import org.openjdk.jmh.annotations.{Benchmark, Param, Setup, State, Scope}
import org.openjdk.jmh.infra.Blackhole

// ── Megamorphic / multi-fold reference fold — a regression guard for the shared-leaf-method design ──
//
// FArray's reference `foldLeft(0)(_ + extract(_))` reads each element via a SHARED non-inline leaf
// method (`reduceLeafFwdRefInt`), whose body does a per-element `v.asInstanceOf[A]` checkcast. That
// design is deliberate: the surface stays a tiny CALL, so MANY folds in one method degrade gracefully
// instead of blowing the JIT's method-size / inline budget (the HugeMethodLimit cliff documented in the
// project memory — "8 folds @100k → 0.29× iarray"). This benchmark exists to keep that property honest.
//
// Two shapes, both running 6 folds in one measured method (JMH forks one JVM per @Benchmark, so the
// interesting profile must be built up INSIDE the method):
//   `polluted` — 6 DISTINCT reference element types (String/Integer/Long/Char + two case classes) funnel
//                through the same generated read site, so its `v.asInstanceOf[A]` checkcast goes
//                MEGAMORPHIC (the real-application profile, not the artificial single-type benchmark).
//   `strMono`  — 6 folds of the SAME String FArray: identical work, but the read site stays MONOMORPHIC.
// The gap between them isolates the megamorphic-checkcast cost of the Object[]-backed read.
//
// History / why this is here: a typed-`Array[A]` reference leaf (String[] etc., read with a cast-free
// `aaload`) was prototyped to kill that per-element checkcast. It can ONLY read typed by INLINING the
// leaf loop at the concrete-A surface (the shared method is erased, element arrives as Object) — and
// inlining the loop reintroduces exactly the multi-fold cliff: on this benchmark the typed build
// collapsed to ~6.7k ops/s vs ~18.6k for the committed Object[] build @10k (a single isolated fold was
// fine either way). The checkcast it saved was dwarfed by the compile-budget blowup. Net loss; the
// experiment was stashed. Keep this guard so any future "typed read" attempt is measured against it.
final case class Wid(w: Int)
final case class Tall(h: Long, label: String)

@State(Scope.Thread)
class FoldRefPollutedBenchmark extends CommonParams {
  @Param(Array("1000", "10000", "100000"))
  var size: Int = 10000

  // farray inputs (6 distinct reference element types)
  var fStr: FArray[String] = _
  var fInt: FArray[Integer] = _
  var fLong: FArray[java.lang.Long] = _
  var fWid: FArray[Wid] = _
  var fTall: FArray[Tall] = _
  var fChar: FArray[Character] = _

  // Competitor inputs of the SAME 6 types. List/Vector/Chunk fold via a generic boxed Function2 (no
  // per-kind codegen); IArray ≈ raw Array folds inline per call site. A fair megamorphic contrast.
  var lStr: List[String] = _; var lInt: List[Integer] = _; var lLong: List[java.lang.Long] = _
  var lWid: List[Wid] = _; var lTall: List[Tall] = _; var lChar: List[Character] = _
  var vStr: Vector[String] = _; var vInt: Vector[Integer] = _; var vLong: Vector[java.lang.Long] = _
  var vWid: Vector[Wid] = _; var vTall: Vector[Tall] = _; var vChar: Vector[Character] = _
  var iStr: IArray[String] = _; var iInt: IArray[Integer] = _; var iLong: IArray[java.lang.Long] = _
  var iWid: IArray[Wid] = _; var iTall: IArray[Tall] = _; var iChar: IArray[Character] = _
  var cStr: fs2.Chunk[String] = _; var cInt: fs2.Chunk[Integer] = _; var cLong: fs2.Chunk[java.lang.Long] = _
  var cWid: fs2.Chunk[Wid] = _; var cTall: fs2.Chunk[Tall] = _; var cChar: fs2.Chunk[Character] = _
  var zStr: zio.Chunk[String] = _; var zInt: zio.Chunk[Integer] = _; var zLong: zio.Chunk[java.lang.Long] = _
  var zWid: zio.Chunk[Wid] = _; var zTall: zio.Chunk[Tall] = _; var zChar: zio.Chunk[Character] = _

  @Setup def setup(): Unit = {
    fStr = FArray.tabulate(size)(i => i.toString)
    fInt = FArray.tabulate(size)(i => Integer.valueOf(i))
    fLong = FArray.tabulate(size)(i => java.lang.Long.valueOf(i.toLong))
    fWid = FArray.tabulate(size)(i => Wid(i))
    fTall = FArray.tabulate(size)(i => Tall(i.toLong, i.toString))
    fChar = FArray.tabulate(size)(i => Character.valueOf((i & 0xffff).toChar))
    lStr = List.tabulate(size)(i => i.toString)
    lInt = List.tabulate(size)(i => Integer.valueOf(i))
    lLong = List.tabulate(size)(i => java.lang.Long.valueOf(i.toLong))
    lWid = List.tabulate(size)(i => Wid(i))
    lTall = List.tabulate(size)(i => Tall(i.toLong, i.toString))
    lChar = List.tabulate(size)(i => Character.valueOf((i & 0xffff).toChar))
    vStr = lStr.toVector; vInt = lInt.toVector; vLong = lLong.toVector; vWid = lWid.toVector; vTall = lTall.toVector; vChar = lChar.toVector
    iStr = IArray.from(lStr); iInt = IArray.from(lInt); iLong = IArray.from(lLong); iWid = IArray.from(lWid); iTall = IArray.from(lTall);
    iChar = IArray.from(lChar)
    cStr = fs2.Chunk.from(lStr); cInt = fs2.Chunk.from(lInt); cLong = fs2.Chunk.from(lLong); cWid = fs2.Chunk.from(lWid); cTall = fs2.Chunk.from(lTall);
    cChar = fs2.Chunk.from(lChar)
    zStr = zio.Chunk.fromIterable(lStr); zInt = zio.Chunk.fromIterable(lInt); zLong = zio.Chunk.fromIterable(lLong); zWid = zio.Chunk.fromIterable(lWid);
    zTall = zio.Chunk.fromIterable(lTall); zChar = zio.Chunk.fromIterable(lChar)
  }

  // MEGAMORPHIC: 6 distinct element types funnel through the SAME generated foldLeft read site.
  @Benchmark def farray(bh: Blackhole): Unit = {
    bh.consume(fStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(fInt.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(fLong.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(fWid.foldLeft(0)((acc, w) => acc + w.w))
    bh.consume(fTall.foldLeft(0)((acc, t) => acc + t.h.toInt))
    bh.consume(fChar.foldLeft(0)((acc, c) => acc + c.charValue.toInt))
  }

  @Benchmark def list(bh: Blackhole): Unit = {
    bh.consume(lStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(lInt.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(lLong.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(lWid.foldLeft(0)((acc, w) => acc + w.w))
    bh.consume(lTall.foldLeft(0)((acc, t) => acc + t.h.toInt))
    bh.consume(lChar.foldLeft(0)((acc, c) => acc + c.charValue.toInt))
  }

  @Benchmark def vector(bh: Blackhole): Unit = {
    bh.consume(vStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(vInt.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(vLong.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(vWid.foldLeft(0)((acc, w) => acc + w.w))
    bh.consume(vTall.foldLeft(0)((acc, t) => acc + t.h.toInt))
    bh.consume(vChar.foldLeft(0)((acc, c) => acc + c.charValue.toInt))
  }

  @Benchmark def iarray(bh: Blackhole): Unit = {
    bh.consume(iStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(iInt.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(iLong.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(iWid.foldLeft(0)((acc, w) => acc + w.w))
    bh.consume(iTall.foldLeft(0)((acc, t) => acc + t.h.toInt))
    bh.consume(iChar.foldLeft(0)((acc, c) => acc + c.charValue.toInt))
  }

  @Benchmark def fs2chunk(bh: Blackhole): Unit = {
    bh.consume(cStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(cInt.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(cLong.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(cWid.foldLeft(0)((acc, w) => acc + w.w))
    bh.consume(cTall.foldLeft(0)((acc, t) => acc + t.h.toInt))
    bh.consume(cChar.foldLeft(0)((acc, c) => acc + c.charValue.toInt))
  }

  @Benchmark def ziochunk(bh: Blackhole): Unit = {
    bh.consume(zStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(zInt.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(zLong.foldLeft(0)((acc, n) => acc + n.intValue))
    bh.consume(zWid.foldLeft(0)((acc, w) => acc + w.w))
    bh.consume(zTall.foldLeft(0)((acc, t) => acc + t.h.toInt))
    bh.consume(zChar.foldLeft(0)((acc, c) => acc + c.charValue.toInt))
  }

  // MONOMORPHIC control: identical total work, but only String flows through the read site.
  @Benchmark def farrayMono(bh: Blackhole): Unit = {
    bh.consume(fStr.foldLeft(0)((acc, s) => acc + s.length))
    bh.consume(fStr.foldLeft(1)((acc, s) => acc + s.length))
    bh.consume(fStr.foldLeft(2)((acc, s) => acc + s.length))
    bh.consume(fStr.foldLeft(3)((acc, s) => acc + s.length))
    bh.consume(fStr.foldLeft(4)((acc, s) => acc + s.length))
    bh.consume(fStr.foldLeft(5)((acc, s) => acc + s.length))
  }
}
