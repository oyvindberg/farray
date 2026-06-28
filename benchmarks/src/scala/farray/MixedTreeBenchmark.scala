package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Replicates ZIO's MixedChunkBenchmarks: operate over a gnarly, unbalanced tree
// vs the same elements after flattening/materializing. THE apples-to-apples for
// "cost of operating on a Slice+Concat tree vs a flat structure".
//
// The tree is built by slicing a flat `0 until size` into uneven pieces via
// take/drop and `++`-ing them back in an uneven order. For farray/ziochunk this
// produces a Slice+Concat tree; we also keep a flattened twin (ziochunk via
// `.materialize`, farray via `FArray.tabulate`). The flat impls (iarray/list/vector)
// just hold the flat structure.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class MixedTreeIntBenchmark:
  @Param(Array("1000")) var size: Int = 1000

  // tree-shaped impls: the unbalanced tree + its flattened twin
  var farrayTree: FArray[Int] = _
  var farrayMat: FArray[Int] = _
  var ziochunkTree: zio.Chunk[Int] = _
  var ziochunkMat: zio.Chunk[Int] = _

  // flat impls
  var iarrayInput: IArray[Int] = _
  var listInput: List[Int] = _
  var vectorInput: Vector[Int] = _

  // Uneven piece sizes that tile `size`. For 1000: 150+300+50+250+100+150 = 1000.
  // Reordered offsets to make the concat order itself uneven.
  private def pieces(n: Int): Seq[(Int, Int)] =
    // (offset, length) pairs, recombined in an uneven order
    val base = Seq(150, 300, 50, 250, 100, 150)
    // scale/clamp to actual size so the pieces always tile [0, n)
    val total = base.sum
    var off = 0
    val withOffsets = base.map { len =>
      val l = math.min(len, math.max(0, n - off))
      val o = off
      off += l
      (o, l)
    }
    // if anything is left over (n not exactly the base total), append the tail
    val tail = if off < n then Seq((off, n - off)) else Seq.empty
    // recombine in an uneven order: 2,0,4,1,5,3 then tail
    val all = withOffsets ++ tail
    val order = Seq(2, 0, 4, 1, 5, 3).filter(_ < all.length) ++ all.indices.drop(6)
    order.map(all)

  @Setup def setup(): Unit =
    val flat = (0 until size).toArray

    // farray tree: slice via take/drop, concat back in uneven order
    val farrayFlat = FArray.tabulate(size)(i => i)
    farrayTree = pieces(size)
      .map { case (o, l) => farrayFlat.drop(o).take(l) }
      .reduce(_ ++ _)
    farrayMat = FArray.tabulate(size)(i => i)

    // ziochunk tree
    val zioFlat = zio.Chunk.fromArray(flat)
    ziochunkTree = pieces(size)
      .map { case (o, l) => zioFlat.drop(o).take(l) }
      .reduce(_ ++ _)
    ziochunkMat = ziochunkTree.materialize

    iarrayInput = IArray.unsafeFromArray(flat.clone())
    listInput = flat.toList
    vectorInput = flat.toVector

  // ---- map(_*2) ----
  @Benchmark def farrayTree_map(): FArray[Int] = farrayTree.map(_ * 2)
  @Benchmark def farrayMat_map(): FArray[Int] = farrayMat.map(_ * 2)
  @Benchmark def ziochunkTree_map(): zio.Chunk[Int] = ziochunkTree.map(_ * 2)
  @Benchmark def ziochunkMat_map(): zio.Chunk[Int] = ziochunkMat.map(_ * 2)
  @Benchmark def iarray_map(): IArray[Int] = iarrayInput.map(_ * 2)
  @Benchmark def list_map(): List[Int] = listInput.map(_ * 2)
  @Benchmark def vector_map(): Vector[Int] = vectorInput.map(_ * 2)

  // ---- filter(_%2==0) ----
  @Benchmark def farrayTree_filter(): FArray[Int] = farrayTree.filter(_ % 2 == 0)
  @Benchmark def farrayMat_filter(): FArray[Int] = farrayMat.filter(_ % 2 == 0)
  @Benchmark def ziochunkTree_filter(): zio.Chunk[Int] = ziochunkTree.filter(_ % 2 == 0)
  @Benchmark def ziochunkMat_filter(): zio.Chunk[Int] = ziochunkMat.filter(_ % 2 == 0)
  @Benchmark def iarray_filter(): IArray[Int] = iarrayInput.filter(_ % 2 == 0)
  @Benchmark def list_filter(): List[Int] = listInput.filter(_ % 2 == 0)
  @Benchmark def vector_filter(): Vector[Int] = vectorInput.filter(_ % 2 == 0)

  // ---- foldLeft(0)(_+_) ----
  @Benchmark def farrayTree_foldLeft(): Int = farrayTree.foldLeft(0)(_ + _)
  @Benchmark def farrayMat_foldLeft(): Int = farrayMat.foldLeft(0)(_ + _)
  @Benchmark def ziochunkTree_foldLeft(): Int = ziochunkTree.foldLeft(0)(_ + _)
  @Benchmark def ziochunkMat_foldLeft(): Int = ziochunkMat.foldLeft(0)(_ + _)
  @Benchmark def iarray_foldLeft(): Int = iarrayInput.foldLeft(0)(_ + _)
  @Benchmark def list_foldLeft(): Int = listInput.foldLeft(0)(_ + _)
  @Benchmark def vector_foldLeft(): Int = vectorInput.foldLeft(0)(_ + _)

  // ---- find(_>2) ----
  @Benchmark def farrayTree_find(): Option[Int] = farrayTree.find(_ > 2)
  @Benchmark def farrayMat_find(): Option[Int] = farrayMat.find(_ > 2)
  @Benchmark def ziochunkTree_find(): Option[Int] = ziochunkTree.find(_ > 2)
  @Benchmark def ziochunkMat_find(): Option[Int] = ziochunkMat.find(_ > 2)
  @Benchmark def iarray_find(): Option[Int] = iarrayInput.find(_ > 2)
  @Benchmark def list_find(): Option[Int] = listInput.find(_ > 2)
  @Benchmark def vector_find(): Option[Int] = vectorInput.find(_ > 2)
