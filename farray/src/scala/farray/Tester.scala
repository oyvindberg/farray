package farray

object Tester:
//  case class Foo(value: Int)
//  case class Bar(value: Int)

  def main(args: Array[String]): Unit =
    println(FArray())
    val foo = Option.empty[String]
    val first = FArray.fromOptions(foo, Some("1"), None, Some("2"), Some("3"))
    println(first)
    FArray("1", "2", "3", "4", "flaff") match {
      case FArray()     => println("0")
      case FArray(_, _) => println("2")
      case FArray(_, _, many*) =>
        many match {
          case x: AsIndexedSeq[?] => println("many wrapper: " + x)
          case _             => println("many: " + many)
        }
    }

//    FArray(Foo(1), Foo(2), Foo(3)).map(x => Bar(x.value)).foreach(println)
//    FArray.fromArray(Array(Foo(1), Foo(2), Foo(3))).map(x => Bar(x.value)).foreach(println)

//  inline def flaff[A <: AnyVal](a: A)(implicit inline ev: A => AnyRef): AnyRef = ev(a)

//  case class A(value: Int) extends AnyVal

//  private val value: AnyRef = flaff(1)
//  flaff(A(1))
