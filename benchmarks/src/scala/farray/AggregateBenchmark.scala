package farray

import org.openjdk.jmh.annotations.Benchmark

// Aggregations: max / min / maxBy / minBy / sum / product.
// fs2.Chunk has none of these. zio.Chunk is an IndexedSeq -> full API.
class IntAggregateBenchmark extends IntInputs {
  @Benchmark def farray_max(): Int = farrayInput.max
  @Benchmark def list_max(): Int = listInput.max
  @Benchmark def vector_max(): Int = vectorInput.max
  @Benchmark def iarray_max(): Int = iarrayInput.max
  @Benchmark def ziochunk_max(): Int = zioChunkInput.max

  @Benchmark def farray_min(): Int = farrayInput.min
  @Benchmark def list_min(): Int = listInput.min
  @Benchmark def vector_min(): Int = vectorInput.min
  @Benchmark def iarray_min(): Int = iarrayInput.min
  @Benchmark def ziochunk_min(): Int = zioChunkInput.min

  @Benchmark def farray_maxBy(): Int = farrayInput.maxBy(x => -x)
  @Benchmark def list_maxBy(): Int = listInput.maxBy(x => -x)
  @Benchmark def vector_maxBy(): Int = vectorInput.maxBy(x => -x)
  @Benchmark def iarray_maxBy(): Int = iarrayInput.maxBy(x => -x)
  @Benchmark def ziochunk_maxBy(): Int = zioChunkInput.maxBy(x => -x)

  @Benchmark def farray_minBy(): Int = farrayInput.minBy(x => -x)
  @Benchmark def list_minBy(): Int = listInput.minBy(x => -x)
  @Benchmark def vector_minBy(): Int = vectorInput.minBy(x => -x)
  @Benchmark def iarray_minBy(): Int = iarrayInput.minBy(x => -x)
  @Benchmark def ziochunk_minBy(): Int = zioChunkInput.minBy(x => -x)

  @Benchmark def farray_sum(): Int = farrayInput.sum
  @Benchmark def list_sum(): Int = listInput.sum
  @Benchmark def vector_sum(): Int = vectorInput.sum
  @Benchmark def iarray_sum(): Int = iarrayInput.sum
  @Benchmark def ziochunk_sum(): Int = zioChunkInput.sum

  @Benchmark def farray_product(): Int = farrayInput.product
  @Benchmark def list_product(): Int = listInput.product
  @Benchmark def vector_product(): Int = vectorInput.product
  @Benchmark def iarray_product(): Int = iarrayInput.product
  @Benchmark def ziochunk_product(): Int = zioChunkInput.product
  // fs2.Chunk has no sum/product/max/min/maxBy/minBy
}
