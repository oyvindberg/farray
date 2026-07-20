package farray

/** The wrapper behind `xs.withFilter(p)`, so a guarded for-comprehension (`for x <- xs if p(x) yield e`, which desugars to `xs.withFilter(p).map(x => e)`)
  * compiles and runs the predicate inside the single result-building loop — NO intermediate filtered `FArray`.
  *
  * The predicate is stored as a plain function (one indirect call per element, exactly like stdlib `WithFilter`); the `map`/`flatMap`/`foreach` body is inlined
  * and the result is built in one pass. `withFilter` again ANDs the predicates so a second guard fuses too.
  */
final class FWithFilter[A](private[farray] val xs: FArray[A], private[farray] val p: A => Boolean):
  inline def foreach(inline f: A => Unit): Unit =
    xs.foreach(a => if p(a) then f(a))

  inline def map[B](inline f: A => B): FArray[B] =
    FArray.filterMapImpl[A, B](xs, p)(f)

  inline def flatMap[B](inline f: A => FArray[B]): FArray[B] =
    var acc: FArray[B] = FArray.empty[B]
    xs.foreach(a => if p(a) then acc = acc ++ f(a))
    acc

  inline def withFilter(q: A => Boolean): FWithFilter[A] =
    new FWithFilter[A](xs, a => p(a) && q(a))
