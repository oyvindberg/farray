package farray

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.reflect.ClassTag

// Can a generic "create a typed String[]" be reflection-free? Compare:
//  - new Array[Object]            (anewarray Object — what we do now)
//  - new Array[String] (static)   (anewarray String — only possible when the type is a literal)
//  - ClassTag.newArray            (java.lang.reflect.Array.newInstance — reflection)
//  - inline new Array[A], A=String (does inlining a concrete type avoid the ClassTag/reflection?)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 6, time = 400, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
class ArrMkBench {
  val ctStr: ClassTag[String] = summon[ClassTag[String]]
  inline def mkInline[A](using ct: ClassTag[A]): Array[A] = new Array[A](2)

  @Benchmark def a_objectArr(): Int = { var i = 0; var s = 0; while (i < 1000) { s += new Array[Object](2).length; i += 1 }; s }
  @Benchmark def b_staticStr(): Int = { var i = 0; var s = 0; while (i < 1000) { s += new Array[String](2).length; i += 1 }; s }
  @Benchmark def c_reflectStr(): Int = { var i = 0; var s = 0; while (i < 1000) { s += ctStr.newArray(2).length; i += 1 }; s }
  @Benchmark def d_inlineStr(): Int = { var i = 0; var s = 0; while (i < 1000) { s += mkInline[String].length; i += 1 }; s }
}
