package farray

import org.openjdk.jmh.annotations.Benchmark

// A/B: the shared-Java-traverse PROTOTYPE (Proto.java + Prototype.scala) vs the committed inlined-walk
// map/foldLeft vs iarray. `proto` = the new design; `farray` = committed (e7bf468); `iarray` = baseline.
class IntProtoBenchmark extends IntInputs {
  @Benchmark def mapIarray(): IArray[Int] = iarrayInput.map(_ + 1)
  @Benchmark def mapFarray(): FArray[Int] = farrayInput.map(_ + 1)
  @Benchmark def mapProto(): FBase = Prototype.mapProtoInt(farrayInput.asInstanceOf[FBase])(_ + 1)

  @Benchmark def foldIarray(): Int = iarrayInput.foldLeft(0)(_ + _)
  @Benchmark def foldFarray(): Int = farrayInput.foldLeft(0)(_ + _)
  @Benchmark def foldProto(): Int = Prototype.foldLeftProtoInt(farrayInput.asInstanceOf[FBase], 0)(_ + _)
}
