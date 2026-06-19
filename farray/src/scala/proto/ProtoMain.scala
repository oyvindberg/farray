package proto

/** Call sites we will `javap -c` to confirm: sumInt has no boxing (iaload/iadd), sumStr uses Object[]. */
object ProtoMain:
  def sumInt(xs: FA[Int]): Int =
    xs.foldLeft(0)((acc, x) => acc + x)

  def sumStrLen(xs: FA[String]): Int =
    xs.foldLeft(0)((acc, s) => acc + s.length)

  // Construction with no ClassTag in sight:
  def buildInt(n: Int): FA[Int] = FA.tabulate(n)(i => i * 2)
  def buildStr(n: Int): FA[String] = FA.tabulate(n)(i => i.toString)
