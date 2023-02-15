package farray

import org.openjdk.jmh.annotations._
import scala.collection.immutable.BitSet
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.Benchmark
import scala.collection.immutable.BitSet

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array(
    "-server",
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
    "-XX:+AlwaysPreTouch",
    "-XX:+UseNUMA",
    "-XX:-UseAdaptiveNUMAChunkSizing"
  )
)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
abstract class CommonParams
