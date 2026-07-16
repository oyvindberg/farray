package farray

import scala.annotation.compileTimeOnly

/** The REIFIED terminal — what a shape's lowering hook receives. Every terminal method a capability trait provides (see [[ShapeLowering]]) constructs one of
  * these markers and hands it, together with the pipeline, to the shape's single `lower` hook; the hook's macro forwards both to [[FuseMacro.lower]], which
  * parses the marker off the tree and emits the fused loop. Fully typed end-to-end: `Terminal.find(p): Terminal[A, Option[A]]` ties the result type at the
  * surface to the type the engine produces.
  *
  * Markers only: the constructors are never evaluated (the consuming macro erases them), and `@compileTimeOnly` guarantees none can survive into runtime code.
  */
sealed trait Terminal[-A, +R]

object Terminal:
  private inline def stub[A, R]: Terminal[A, R] = null.asInstanceOf[Terminal[A, R]]
  private inline val msg = "Terminal.* are markers consumed by the shape's lowering — call the terminal method on the pipeline instead"

  @compileTimeOnly(msg) def run[A]: Terminal[A, FArray[A]] = stub
  @compileTimeOnly(msg) def head[A]: Terminal[A, A] = stub
  @compileTimeOnly(msg) def headOption[A]: Terminal[A, Option[A]] = stub

  @compileTimeOnly(msg) def find[A](p: A => Boolean): Terminal[A, Option[A]] = stub
  @compileTimeOnly(msg) def exists[A](p: A => Boolean): Terminal[A, Boolean] = stub
  @compileTimeOnly(msg) def forall[A](p: A => Boolean): Terminal[A, Boolean] = stub
  @compileTimeOnly(msg) def indexWhere[A](p: A => Boolean): Terminal[A, Int] = stub

  @compileTimeOnly(msg) def plan[A]: Terminal[A, String] = stub
  @compileTimeOnly(msg) def planFold[A, Z](op: (Z, A) => Z): Terminal[A, String] = stub
  @compileTimeOnly(msg) def planAgg[A](aggs: Seq[Agg[A, Any]]): Terminal[A, String] = stub

  @compileTimeOnly(msg) def agg1[A, R1](a1: Agg[A, R1]): Terminal[A, R1] = stub
  @compileTimeOnly(msg) def agg2[A, R1, R2](a1: Agg[A, R1], a2: Agg[A, R2]): Terminal[A, (R1, R2)] = stub
  @compileTimeOnly(msg) def agg3[A, R1, R2, R3](a1: Agg[A, R1], a2: Agg[A, R2], a3: Agg[A, R3]): Terminal[A, (R1, R2, R3)] = stub
  @compileTimeOnly(msg) def agg4[A, R1, R2, R3, R4](a1: Agg[A, R1], a2: Agg[A, R2], a3: Agg[A, R3], a4: Agg[A, R4]): Terminal[A, (R1, R2, R3, R4)] = stub

  @compileTimeOnly(msg) def aggTo2[A, R1, R2, R](make: (R1, R2) => R, a1: Agg[A, R1], a2: Agg[A, R2]): Terminal[A, R] = stub
  @compileTimeOnly(msg) def aggTo3[A, R1, R2, R3, R](make: (R1, R2, R3) => R, a1: Agg[A, R1], a2: Agg[A, R2], a3: Agg[A, R3]): Terminal[A, R] = stub
  @compileTimeOnly(msg) def aggTo4[A, R1, R2, R3, R4, R](
      make: (R1, R2, R3, R4) => R,
      a1: Agg[A, R1],
      a2: Agg[A, R2],
      a3: Agg[A, R3],
      a4: Agg[A, R4]
  ): Terminal[A, R] = stub

  @compileTimeOnly(msg) def groupReduce[A, K, B](key: A => K, value: A => B, reduce: (B, B) => B): Terminal[A, Map[K, B]] = stub
