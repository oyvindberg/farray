package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// Round-8 item 2 probe: native identity-preserving filterConserve.
//  - noDrop:  predicate keeps EVERY element -> filterConserve must return the receiver with ZERO allocation
//             (the dominant dotc shape — 40+ Decorators.filterConserve call sites, most keep all). farray_filter
//             (the always-allocating filter) is the baseline that rebuilds even when nothing is dropped.
//  - someDrop / allDrop: predicate drops one / all elements -> a rebuild is unavoidable; measure time parity.
// Vector has no filterConserve; `vector_filter` is the generic always-allocating baseline.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class FilterConserveRefBenchmark:
  @Param(Array("1", "3", "8", "32", "256")) var size: Int = 8

  var farrayInput: FArray[String] = _
  var vectorInput: Vector[String] = _
  var dropVal: String = _

  @Setup def setup(): Unit =
    farrayInput = FArray.tabulate(size)(i => "s" + i)
    vectorInput = Vector.tabulate(size)(i => "s" + i)
    dropVal = "s" + (size / 2) // one element value that exists in both inputs

  // --- no drop: keep all (the conserve win — identity, zero alloc) ---
  @Benchmark def farray_conserve_noDrop(): FArray[String] = farrayInput.filterConserve(_ => true)
  @Benchmark def farray_filter_noDrop(): FArray[String] = farrayInput.filter(_ => true)
  @Benchmark def vector_filter_noDrop(): Vector[String] = vectorInput.filter(_ => true)

  // --- some drop: remove exactly one element (by value, so both structures drop it) ---
  @Benchmark def farray_conserve_someDrop(): FArray[String] = farrayInput.filterConserve(_ != dropVal)
  @Benchmark def farray_filter_someDrop(): FArray[String] = farrayInput.filter(_ != dropVal)
  @Benchmark def vector_filter_someDrop(): Vector[String] = vectorInput.filter(_ != dropVal)

  // --- all drop: remove everything ---
  @Benchmark def farray_conserve_allDrop(): FArray[String] = farrayInput.filterConserve(_ => false)
  @Benchmark def farray_filter_allDrop(): FArray[String] = farrayInput.filter(_ => false)
