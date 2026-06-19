package proto

// A value class over Double, with a manual Unbox (the macro auto-derive comes next).
class Meters(val d: Double) extends AnyVal
given Unbox[Meters] with
  type Prim = Double
  inline def unwrap(a: Meters): Double = a.d
  inline def wrap(p: Double): Meters = Meters(p)

// An opaque newtype over Double, with a manual Unbox declared in its scope (where Celsius = Double).
opaque type Celsius = Double
object Celsius:
  def apply(d: Double): Celsius = d
  extension (c: Celsius) def toDouble: Double = c
  given Unbox[Celsius] with
    type Prim = Double
    inline def unwrap(a: Celsius): Double = a
    inline def wrap(p: Double): Celsius = p

object UnboxMain:
  // Want: buildMeters -> DoubleArr (double[]); sumMeters -> unboxed double loop (no Double boxing).
  def buildMeters(n: Int): FBase = UnboxFA.tabulate[Meters](n)(i => Meters(i.toDouble))
  def sumMeters(xs: FBase): Double = UnboxFA.sumDouble[Meters](xs)((acc, m) => acc + m.d)

  def buildCelsius(n: Int): FBase = UnboxFA.tabulate[Celsius](n)(i => Celsius(i.toDouble))
  def buildInt(n: Int): FBase = UnboxFA.tabulate[Int](n)(i => i)
