package farray
import org.openjdk.jmh.annotations.Benchmark
class P2Benchmark extends IntInputs:
  @Benchmark def farray_startsWith(): Boolean = farrayInput.startsWith(farrayInput)
  @Benchmark def array_startsWith(): Boolean   = arrayInput.startsWith(arrayInput)
  @Benchmark def list_startsWith(): Boolean    = listInput.startsWith(listInput)
  @Benchmark def farray_corresponds(): Boolean = farrayInput.corresponds(farrayInput)(_ == _)
