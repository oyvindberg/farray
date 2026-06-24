package farray

import org.openjdk.jmh.annotations.Benchmark

// Aggregations. FArray exposes max / min / maxBy / minBy but NOT sum / product (API gap).
// fs2.Chunk has none of these. zio.Chunk is an IndexedSeq -> full API.
class IntAggregateBenchmark extends IntInputs {
  @Benchmark def farray_max(): Int = farrayInput.max
  @Benchmark def list_max(): Int = listInput.max
  @Benchmark def vector_max(): Int = vectorInput.max
  @Benchmark def iarray_max(): Int = iarrayInput.max
  @Benchmark def array_max(): Int = arrayInput.max
  @Benchmark def ziochunk_max(): Int = zioChunkInput.max

  @Benchmark def farray_min(): Int = farrayInput.min
  @Benchmark def list_min(): Int = listInput.min
  @Benchmark def vector_min(): Int = vectorInput.min
  @Benchmark def iarray_min(): Int = iarrayInput.min
  @Benchmark def array_min(): Int = arrayInput.min
  @Benchmark def ziochunk_min(): Int = zioChunkInput.min

  @Benchmark def farray_maxBy(): Int = farrayInput.maxBy(x => -x)
  @Benchmark def list_maxBy(): Int = listInput.maxBy(x => -x)
  @Benchmark def vector_maxBy(): Int = vectorInput.maxBy(x => -x)
  @Benchmark def iarray_maxBy(): Int = iarrayInput.maxBy(x => -x)
  @Benchmark def array_maxBy(): Int = arrayInput.maxBy(x => -x)
  @Benchmark def ziochunk_maxBy(): Int = zioChunkInput.maxBy(x => -x)

  @Benchmark def farray_minBy(): Int = farrayInput.minBy(x => -x)
  @Benchmark def list_minBy(): Int = listInput.minBy(x => -x)
  @Benchmark def vector_minBy(): Int = vectorInput.minBy(x => -x)
  @Benchmark def iarray_minBy(): Int = iarrayInput.minBy(x => -x)
  @Benchmark def array_minBy(): Int = arrayInput.minBy(x => -x)
  @Benchmark def ziochunk_minBy(): Int = zioChunkInput.minBy(x => -x)

  // sum / product: FArray has neither (API gap)
  @Benchmark def list_sum(): Int = listInput.sum
  @Benchmark def vector_sum(): Int = vectorInput.sum
  @Benchmark def iarray_sum(): Int = iarrayInput.sum
  @Benchmark def array_sum(): Int = arrayInput.sum
  @Benchmark def ziochunk_sum(): Int = zioChunkInput.sum

  @Benchmark def list_product(): Int = listInput.product
  @Benchmark def vector_product(): Int = vectorInput.product
  @Benchmark def iarray_product(): Int = iarrayInput.product
  @Benchmark def array_product(): Int = arrayInput.product
  @Benchmark def ziochunk_product(): Int = zioChunkInput.product
  // fs2.Chunk has no sum/product/max/min/maxBy/minBy
}
