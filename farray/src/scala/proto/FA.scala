package proto

import scala.compiletime.erasedValue

/**
 * Proof of compile-time per-primitive specialization with NO ClassTag.
 *
 * `inline erasedValue[A] match` reduces at the concrete call site and (we are testing) type-checks
 * only the selected branch — so the Int branch can read `int[]` and do unboxed int arithmetic that
 * would not type-check for an abstract A. The goal: `FA[Int].foldLeft` compiles to a tight `int[]`
 * loop with NO Integer boxing and NO checkcast, while `FA[String]` uses the Object[] path.
 */
opaque type FA[+A] = FBase

object FA:

  inline def tabulate[A](n: Int)(inline f: Int => A): FA[A] =
    inline erasedValue[A] match
      case _: Int =>
        val arr = new Array[Int](n)
        var i = 0
        while i < n do
          arr(i) = f(i).asInstanceOf[Int]
          i += 1
        new IntArr(arr, n)
      case _ =>
        val arr = new Array[Object](n)
        var i = 0
        while i < n do
          arr(i) = f(i).asInstanceOf[Object]
          i += 1
        new RefArr(arr, n)

  extension [A](xs: FA[A])
    inline def foldLeft[Z](z: Z)(inline op: (Z, A) => Z): Z =
      inline erasedValue[A] match
        case _: Int =>
          val core = xs.asInstanceOf[IntArr]
          val a = core.a
          val n = core.length
          var acc = z
          var i = 0
          while i < n do
            acc = op(acc, a(i).asInstanceOf[A])
            i += 1
          acc
        case _ =>
          val core = xs.asInstanceOf[RefArr]
          val a = core.a
          val n = core.length
          var acc = z
          var i = 0
          while i < n do
            acc = op(acc, a(i).asInstanceOf[A])
            i += 1
          acc
