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
    // NOTE: dropped -server / -XX:+UseNUMA / -XX:-UseAdaptiveNUMAChunkSizing — server-tuned,
    // skew results on a non-NUMA macOS dev box. Re-add for the final publishable run on a server.
    // Quick low-N iteration: override warmup/iters/size from the CLI, e.g.
    //   bleep run benchmarks-runner <regex> -p size=4 -wi 0 -i 2 -f 1 -r 300ms
  )
)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
abstract class CommonParams
