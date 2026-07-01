package farray

import org.openjdk.jmh.annotations.{BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Warmup}
import java.util.concurrent.TimeUnit

// The SAME 14-stage pipeline as LongMixedPipelineIntBenchmark, but measured COLD: SingleShotTime, ZERO
// warmup, so each fork times a single uncompiled invocation while the JVM is still interpreting. This
// backs the site's cold-start claim — FArray is ahead before the JIT devirtualizes or escape-analyzes
// anything; the lead is there from the first call and only widens with warmup. Run at size=100000 (single
// -shot at tiny sizes is dominated by noise / dead-code elimination). Reuses the parent's @Benchmark
// methods and inputs verbatim; only the JMH mode changes.
@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 0)
@Measurement(iterations = 1)
@Fork(12)
class ColdPipelineIntBenchmark extends LongMixedPipelineIntBenchmark
