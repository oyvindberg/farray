package farray

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

// Mixed-kind construction: ONE method instantiates a 4-element collection of FOUR different
// element kinds (Int, Long, Double, String) per invocation — the megamorphic-map experiment
// applied to creation. An impl whose literal construction funnels through a shared generic
// varargs path pays a Seq + boxing per kind once the site sees several element types; FArray's
// apply macro emits four independent typed constructions, so there is no shared site to poison.
// Fields are set in @Setup so the JIT can't constant-fold the literals away.
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class CreationMixedBenchmark:
  var i1, i2, i3, i4 = 0
  var l1, l2, l3, l4 = 0L
  var d1, d2, d3, d4 = 0.0
  var s1, s2, s3, s4: String = null

  @Setup def setup(): Unit =
    i1 = 1; i2 = 2; i3 = 3; i4 = 4
    l1 = 1L; l2 = 2L; l3 = 3L; l4 = 4L
    d1 = 1.0; d2 = 2.0; d3 = 3.0; d4 = 4.0
    s1 = "a"; s2 = "b"; s3 = "c"; s4 = "d"

  @Benchmark def farray(bh: Blackhole): Unit =
    bh.consume(FArray(i1, i2, i3, i4))
    bh.consume(FArray(l1, l2, l3, l4))
    bh.consume(FArray(d1, d2, d3, d4))
    bh.consume(FArray(s1, s2, s3, s4))

  @Benchmark def iarray(bh: Blackhole): Unit =
    bh.consume(IArray(i1, i2, i3, i4))
    bh.consume(IArray(l1, l2, l3, l4))
    bh.consume(IArray(d1, d2, d3, d4))
    bh.consume(IArray(s1, s2, s3, s4))

  @Benchmark def list(bh: Blackhole): Unit =
    bh.consume(List(i1, i2, i3, i4))
    bh.consume(List(l1, l2, l3, l4))
    bh.consume(List(d1, d2, d3, d4))
    bh.consume(List(s1, s2, s3, s4))

  @Benchmark def vector(bh: Blackhole): Unit =
    bh.consume(Vector(i1, i2, i3, i4))
    bh.consume(Vector(l1, l2, l3, l4))
    bh.consume(Vector(d1, d2, d3, d4))
    bh.consume(Vector(s1, s2, s3, s4))

  @Benchmark def fs2chunk(bh: Blackhole): Unit =
    bh.consume(fs2.Chunk(i1, i2, i3, i4))
    bh.consume(fs2.Chunk(l1, l2, l3, l4))
    bh.consume(fs2.Chunk(d1, d2, d3, d4))
    bh.consume(fs2.Chunk(s1, s2, s3, s4))

  @Benchmark def ziochunk(bh: Blackhole): Unit =
    bh.consume(zio.Chunk(i1, i2, i3, i4))
    bh.consume(zio.Chunk(l1, l2, l3, l4))
    bh.consume(zio.Chunk(d1, d2, d3, d4))
    bh.consume(zio.Chunk(s1, s2, s3, s4))
