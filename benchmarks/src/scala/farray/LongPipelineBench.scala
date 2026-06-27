package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** A LONG, mixed-stage pipeline — run the SAME logical transform across FArray (fused vs eager) and the standard immutable collections (List, Vector, IArray).
  * This is the headline comparison: one fused unboxed pass vs N intermediate collections.
  *
  * The chains deliberately use a rich mix of stages fusion supports AND every collection has — `map`, `filter`, `collect` (a PartialFunction; fusion inlines
  * the match into one filter+map), `take`/`takeWhile`, `zipWithIndex`, `flatMap` — so it's apples-to-apples.
  *
  *   - FArray FUSED: one pass, no intermediate collections, no `Function1`, unboxed (Int).
  *   - FArray EAGER / List / Vector / IArray: each stage allocates a new collection; List/Vector box every Int.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntLongPipelineBench extends IntInputs:
  // map · filter · collect · map · zipWithIndex(use idx) · takeWhile · filter · map · sum
  @Benchmark def farrayFused(): Int =
    farrayInput.fuse
      .map(_ + 1)
      .filter(_ % 2 == 0)
      .collect { case x if x % 3 == 0 => x * 3 }
      .map(_ - 1)
      .zipWithIndex
      .map((x, i) => x + i)
      .takeWhile(_ < 1_000_000)
      .filter(_ % 5 != 0)
      .map(_ * 2)
      .sum

  @Benchmark def farrayEager(): Int =
    farrayInput
      .map(_ + 1)
      .filter(_ % 2 == 0)
      .collect { case x if x % 3 == 0 => x * 3 }
      .map(_ - 1)
      .zipWithIndex
      .map((x, i) => x + i)
      .takeWhile(_ < 1_000_000)
      .filter(_ % 5 != 0)
      .map(_ * 2)
      .sum

  @Benchmark def list(): Int =
    listInput
      .map(_ + 1)
      .filter(_ % 2 == 0)
      .collect { case x if x % 3 == 0 => x * 3 }
      .map(_ - 1)
      .zipWithIndex
      .map((x, i) => x + i)
      .takeWhile(_ < 1_000_000)
      .filter(_ % 5 != 0)
      .map(_ * 2)
      .sum

  @Benchmark def vector(): Int =
    vectorInput
      .map(_ + 1)
      .filter(_ % 2 == 0)
      .collect { case x if x % 3 == 0 => x * 3 }
      .map(_ - 1)
      .zipWithIndex
      .map((x, i) => x + i)
      .takeWhile(_ < 1_000_000)
      .filter(_ % 5 != 0)
      .map(_ * 2)
      .sum

  @Benchmark def iarray(): Int =
    iarrayInput
      .map(_ + 1)
      .filter(_ % 2 == 0)
      .collect { case x if x % 3 == 0 => x * 3 }
      .map(_ - 1)
      .zipWithIndex
      .map((x, i) => x + i)
      .takeWhile(_ < 1_000_000)
      .filter(_ % 5 != 0)
      .map(_ * 2)
      .sum

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StrLongPipelineBench extends Inputs:
  // filter · map · collect · map · zipWithIndex · filter · map · sum  (a reference-element chain)
  @Benchmark def farrayFused(): Int =
    farrayInput.fuse
      .filter(_.length <= 4)
      .map(_.reverse)
      .collect { case s if !s.startsWith("0") => s.toUpperCase }
      .map(_ + "!")
      .zipWithIndex
      .map((s, i) => s + i)
      .filter(_.length > 2)
      .map(_.length)
      .sum

  @Benchmark def farrayEager(): Int =
    farrayInput
      .filter(_.length <= 4)
      .map(_.reverse)
      .collect { case s if !s.startsWith("0") => s.toUpperCase }
      .map(_ + "!")
      .zipWithIndex
      .map((s, i) => s + i)
      .filter(_.length > 2)
      .map(_.length)
      .sum

  @Benchmark def list(): Int =
    listInput
      .filter(_.length <= 4)
      .map(_.reverse)
      .collect { case s if !s.startsWith("0") => s.toUpperCase }
      .map(_ + "!")
      .zipWithIndex
      .map((s, i) => s + i)
      .filter(_.length > 2)
      .map(_.length)
      .sum

  @Benchmark def vector(): Int =
    vectorInput
      .filter(_.length <= 4)
      .map(_.reverse)
      .collect { case s if !s.startsWith("0") => s.toUpperCase }
      .map(_ + "!")
      .zipWithIndex
      .map((s, i) => s + i)
      .filter(_.length > 2)
      .map(_.length)
      .sum

  @Benchmark def iarray(): Int =
    iarrayInput
      .filter(_.length <= 4)
      .map(_.reverse)
      .collect { case s if !s.startsWith("0") => s.toUpperCase }
      .map(_ + "!")
      .zipWithIndex
      .map((s, i) => s + i)
      .filter(_.length > 2)
      .map(_.length)
      .sum

/** DEAD-CODE ELIMINATION showcase: `map` each element into a multi-field case class, then `filter` on one field and project ONE other. The two unused fields —
  * one of them DELIBERATELY EXPENSIVE — are never read downstream.
  *
  *   - FArray FUSED: the case class is a set of independent COLUMNS. `gauss(x)` (the expensive dead field) is never computed, `tag` is never computed, and NO
  *     `Rec` is ever allocated — the loop computes only `key` (for the filter) and `score` (the projection, only for survivors). Dead-column elimination +
  *     compute-for-survivors.
  *   - List / Vector / IArray / FArray-EAGER: build the FULL `Rec` for EVERY element — `gauss(x)` runs on all N, the object is allocated on all N — then throw
  *     3 of 4 fields away. This is where fusion laps the field.
  */
object DceBench:
  final case class Rec(key: Int, score: Int, tag: String, gauss: Int)
  // a deliberately expensive "feature" — a few dozen LCG rounds. In the eager collections this runs for EVERY
  // element; fused, it's a dead column and runs zero times.
  inline def gauss(x: Int): Int =
    var s = x; var k = 0
    while k < 40 do { s = s * 1103515245 + 12345; k += 1 }
    s
  inline def mkRec(x: Int): Rec = Rec(key = x % 100, score = x * 7, tag = "t" + (x % 8), gauss = gauss(x))

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class IntDceBench extends IntInputs:
  import DceBench.*
  // map -> Rec, filter on key, project score : fused never builds Rec, never computes gauss/tag
  @Benchmark def farrayFused(): Int =
    farrayInput.fuse.map(mkRec).filter(_.key % 5 == 0).map(_.score).sum
  @Benchmark def farrayEager(): Int =
    farrayInput.map(mkRec).filter(_.key % 5 == 0).map(_.score).sum
  @Benchmark def list(): Int =
    listInput.map(mkRec).filter(_.key % 5 == 0).map(_.score).sum
  @Benchmark def vector(): Int =
    vectorInput.map(mkRec).filter(_.key % 5 == 0).map(_.score).sum
  @Benchmark def iarray(): Int =
    iarrayInput.map(mkRec).filter(_.key % 5 == 0).map(_.score).sum
