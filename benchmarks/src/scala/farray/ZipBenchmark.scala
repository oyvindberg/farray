package farray

import org.openjdk.jmh.annotations.Benchmark

class ZipIntBenchmark extends IntInputs:
  @Benchmark def farray_zip(): FArray[(Int, Int)] = farrayInput.zip(farrayInput)
  @Benchmark def array_zip(): Array[(Int, Int)] = arrayInput.zip(arrayInput)
  @Benchmark def list_zip(): List[(Int, Int)] = listInput.zip(listInput)
  @Benchmark def vector_zip(): Vector[(Int, Int)] = vectorInput.zip(vectorInput)
  @Benchmark def farray_zipWithIndex(): FArray[(Int, Int)] = farrayInput.zipWithIndex
  @Benchmark def array_zipWithIndex(): Array[(Int, Int)] = arrayInput.zipWithIndex
  @Benchmark def list_zipWithIndex(): List[(Int, Int)] = listInput.zipWithIndex

class ZipStringBenchmark extends Inputs:
  @Benchmark def farray_zip(): FArray[(String, String)] = farrayInput.zip(farrayInput)
  @Benchmark def list_zip(): List[(String, String)] = listInput.zip(listInput)
  @Benchmark def vector_zip(): Vector[(String, String)] = vectorInput.zip(vectorInput)
  @Benchmark def farray_zipWithIndex(): FArray[(String, Int)] = farrayInput.zipWithIndex
  @Benchmark def list_zipWithIndex(): List[(String, Int)] = listInput.zipWithIndex
