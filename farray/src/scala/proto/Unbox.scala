package proto

import scala.compiletime.summonFrom

/** Evidence that A is represented by an unboxed primitive `Prim`. inline wrap/unwrap so no boxing. */
trait Unbox[A]:
  type Prim
  inline def unwrap(a: A): Prim
  inline def wrap(p: Prim): A

object Unbox:
  given Unbox[Int] with
    type Prim = Int
    inline def unwrap(a: Int): Int = a
    inline def wrap(p: Int): Int = p

  given Unbox[Double] with
    type Prim = Double
    inline def unwrap(a: Double): Double = a
    inline def wrap(p: Double): Double = p

/** Construction + a fold, dispatching on the Unbox Prim refinement via summonFrom. */
object UnboxFA:
  inline def tabulate[A](n: Int)(inline f: Int => A): FBase =
    summonFrom {
      case u: (Unbox[A] { type Prim = Int }) =>
        val out = new Array[Int](n); var i = 0
        while i < n do { out(i) = u.unwrap(f(i)); i += 1 }
        new IntArr(out, n)
      case u: (Unbox[A] { type Prim = Double }) =>
        val out = new Array[Double](n); var i = 0
        while i < n do { out(i) = u.unwrap(f(i)); i += 1 }
        new DoubleArr(out, n)
      case _ =>
        val out = new Array[Object](n); var i = 0
        while i < n do { out(i) = f(i).asInstanceOf[Object]; i += 1 }
        new RefArr(out, n)
    }

  inline def sumDouble[A](xs: FBase)(inline op: (Double, A) => Double): Double =
    summonFrom {
      case u: (Unbox[A] { type Prim = Double }) =>
        val a = xs.asInstanceOf[DoubleArr].arr; val n = xs.length
        var acc = 0.0; var i = 0
        while i < n do { acc = op(acc, u.wrap(a(i))); i += 1 }
        acc
      case _ => 0.0
    }
