package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** STREAMING showcase: process a lazily-GENERATED sequence of N ints through `filter(even).map(*3).sum` in CONSTANT memory — no backing array is ever held, the
  * elements are produced on demand. This is the unique capability this PR adds: `fuse` off the end of an array and over an incremental `Source`.
  *
  * All contenders are constant-memory lazy drivers (apples-to-apples — none materializes the N elements):
  *   - farraySource : `Source.range(...).fuse.filter.map.sum` — the fused chunk driver. The macro emits ONE unboxed `int` loop over each pulled `int[]` chunk;
  *     `filter` is an `if`, `map` is `iadd`, the fold is a `var`. Zero `Function1`, zero per-element boxing, zero intermediate collection.
  *   - iterator : `Iterator.range(...).filter(...).map(...).sum` — the JVM-idiomatic lazy chain. Each stage is a stored `Function1` called through an interface
  *     per element; every Int is boxed (`Iterator[Int]` erases to `Iterator[Object]`).
  *   - lazyList : `LazyList.range(...)…` — Scala's lazy list; additionally memoizes every cell (a cons node + a boxed Int per element retained until GC).
  *   - fs2Stream : `fs2.Stream.range(...)…compile` — the reference functional-streaming library, driven through cats-effect. Constant memory, but a boxed
  *     pull-based interpreter per element.
  *
  * The point isn't just throughput — it's that the fused source is a tight primitive loop while every lazy alternative is a boxed, virtual-dispatch pull chain.
  * Same O(1) memory; very different constant factor.
  */
@State(Scope.Thread)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2g", "-Xmx2g", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch"))
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StreamPipelineBenchmark:

  @Param(Array("100000", "1000000"))
  var n: Int = 1000000

  @Benchmark def farraySource(): Int =
    Source.range(0, n).fuse.filter(_ % 2 == 0).map(_ * 3).sum

  @Benchmark def iterator(): Int =
    Iterator.range(0, n).filter(_ % 2 == 0).map(_ * 3).sum

  @Benchmark def lazyList(): Int =
    LazyList.range(0, n).filter(_ % 2 == 0).map(_ * 3).sum

  @Benchmark def fs2Stream(): Int =
    // a PURE fs2 stream (no effect type) — compiled to a single Int via the constant-memory pull interpreter.
    fs2.Stream
      .range(0, n)
      .filter(_ % 2 == 0)
      .map(_ * 3)
      .compile
      .fold(0)(_ + _)

/** STREAMING over an InputStream of lines: `lines.filter(...).map(_.length).sum` in constant memory — the headline "fold a file larger than heap, line by line,
  * and it closes itself" case, here over an in-memory byte stream so the benchmark is deterministic (the cost model is identical — bytes are pulled, never all
  * held).
  *
  *   - farrayLines : `Source.lines(in).fuse.filter(...).map(_.length).sum` — fused chunk driver over decoded lines.
  *   - scalaSource : `scala.io.Source.fromInputStream(in).getLines.filter(...).map(_.length).sum` — the idiomatic Scala way (a boxed `Iterator[String]` chain).
  *   - bufferedReader : a hand-written `BufferedReader.readLine()` while-loop — the manual constant-memory baseline.
  */
@State(Scope.Thread)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2g", "-Xmx2g", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch"))
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StreamLinesBenchmark:

  @Param(Array("100000"))
  var n: Int = 100000

  private var bytes: Array[Byte] = null

  @Setup
  def setup(): Unit =
    val sb = new java.lang.StringBuilder(n * 12)
    var i = 0
    while i < n do { sb.append("line number ").append(i).append('\n'); i += 1 }
    bytes = sb.toString.getBytes("UTF-8")

  private def in(): java.io.InputStream = new java.io.ByteArrayInputStream(bytes)

  @Benchmark def farrayLines(): Int =
    Source.lines(in()).fuse.filter(_.nonEmpty).map(_.length).sum

  @Benchmark def scalaSource(): Int =
    scala.io.Source.fromInputStream(in()).getLines().filter(_.nonEmpty).map(_.length).sum

  @Benchmark def bufferedReader(): Int =
    val r = new java.io.BufferedReader(new java.io.InputStreamReader(in(), java.nio.charset.StandardCharsets.UTF_8))
    try
      var acc = 0
      var line = r.readLine()
      while line ne null do
        if line.nonEmpty then acc += line.length
        line = r.readLine()
      acc
    finally r.close()
