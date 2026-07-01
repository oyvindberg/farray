package farray

import org.junit.Test

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable

/** EXHAUSTIVE small-scope correctness harness for FArray's traversal interpreter.
  *
  * Strategy: a recipe ADT enumerates EVERY FArray shape reachable from the constructors up to a bounded depth, each paired with its
  * `scala.collection.immutable.List` oracle (the identical sequence of constructor ops applied to a List). For each (FArray, List) pair we assert
  * `farray.<op> == list.<op>` across a broad op set. Every per-case evaluation runs inside a bounded-time worker thread (interrupt + join(timeoutMs)) so a
  * non-terminating / pathologically slow shape is RECORDED as a HANG and the sweep CONTINUES rather than wedging the whole build.
  *
  * The harness is generic over the element kind `E` so the same recipe tree is replayed once for Int leaves (primitive `int[]` cores) and once for String
  * leaves (`Object[]` cores).
  */
object FExhaustive:

  // ---------------------------------------------------------------------------
  // Recipe ADT — a replayable description of how a shape was built. `toString`
  // yields a recipe string like  empty.:+(1).++(leaf(2,3)).reverse.slice(1,3)
  // ---------------------------------------------------------------------------
  sealed trait Recipe
  case object Empty extends Recipe:
    override def toString = "empty"
  final case class Leaf(elems: List[Int]) extends Recipe:
    override def toString = s"leaf(${elems.mkString(",")})"
  final case class Concat(a: Recipe, b: Recipe) extends Recipe:
    override def toString = s"$a.++($b)"
  final case class Append(a: Recipe, e: Int) extends Recipe:
    override def toString = s"$a.:+($e)"
  final case class Prepend(e: Int, a: Recipe) extends Recipe:
    override def toString = s"$a.+:($e)" // shown post-fix style for replayability of the chain
  final case class Take(a: Recipe, k: Int) extends Recipe:
    override def toString = s"$a.take($k)"
  final case class Drop(a: Recipe, k: Int) extends Recipe:
    override def toString = s"$a.drop($k)"
  final case class Slice(a: Recipe, i: Int, j: Int) extends Recipe:
    override def toString = s"$a.slice($i,$j)"
  case class Reverse(a: Recipe) extends Recipe:
    override def toString = s"$a.reverse"
  final case class PadTo(a: Recipe, n: Int, f: Int) extends Recipe:
    override def toString = s"$a.padTo($n,$f)"
  final case class Updated(a: Recipe, i: Int, e: Int) extends Recipe:
    override def toString = s"$a.updated($i,$e)"
  // flatMap producing a ${K}FlatMap node: Dup = e => [e,e] (size-2 leaf inners, the fast path);
  // Single = e => [e] (size-1 => One-inner materialize + total-canonicalisation). Both are element-value
  // independent so the List oracle and the FArray stay in parity. Wrapping these in the composite ops above
  // is what exercises the flatMap node as an INPUT to take/drop/slice/padTo/updated/reverse/scan.
  final case class FlatMapDup(a: Recipe) extends Recipe:
    override def toString = s"$a.flatMap(e=>[e,e])"
  final case class FlatMapSingle(a: Recipe) extends Recipe:
    override def toString = s"$a.flatMap(e=>[e])"
  // Wide = e => [e]*8 (>= the node threshold): exercises the ${K}FlatMap NODE path (Dup/Single stay < 8 => flatten).
  final case class FlatMapWide(a: Recipe) extends Recipe:
    override def toString = s"$a.flatMap(e=>[e]*8)"

  // ---------------------------------------------------------------------------
  // Element kind abstraction: Int (primitive leaves) and String (Object[] leaves).
  // We carry one builder + the List oracle materialization through a typeclass so
  // the recipe enumeration is written once.
  // ---------------------------------------------------------------------------
  trait Kind[E]:
    def name: String
    def elem(i: Int): E // map the small int alphabet 0..3 to a concrete element
    def leaf(ints: List[Int]): FArray[E]
    def empty: FArray[E]
    def concat(a: FArray[E], b: FArray[E]): FArray[E]
    def append(a: FArray[E], e: Int): FArray[E]
    def prepend(e: Int, a: FArray[E]): FArray[E]
    def padTo(a: FArray[E], n: Int, f: Int): FArray[E]
    def updated(a: FArray[E], i: Int, e: Int): FArray[E]
    def flatMapDup(a: FArray[E]): FArray[E]
    def flatMapSingle(a: FArray[E]): FArray[E]
    def flatMapWide(a: FArray[E]): FArray[E]

  given Kind[Int] with
    def name = "Int"
    def elem(i: Int): Int = i
    def leaf(ints: List[Int]): FArray[Int] = FArray.fromIterable(ints)
    def empty: FArray[Int] = FArray.empty[Int]
    def concat(a: FArray[Int], b: FArray[Int]): FArray[Int] = a ++ b
    def append(a: FArray[Int], e: Int): FArray[Int] = a :+ e
    def prepend(e: Int, a: FArray[Int]): FArray[Int] = e +: a
    def padTo(a: FArray[Int], n: Int, f: Int): FArray[Int] = a.padTo(n, f)
    def updated(a: FArray[Int], i: Int, e: Int): FArray[Int] = a.updated(i, e)
    def flatMapDup(a: FArray[Int]): FArray[Int] = a.flatMap(e => FArray(e, e))
    def flatMapSingle(a: FArray[Int]): FArray[Int] = a.flatMap(e => FArray(e))
    def flatMapWide(a: FArray[Int]): FArray[Int] = a.flatMap(e => FArray.tabulate(8)(_ => e))

  given Kind[String] with
    def name = "String"
    def elem(i: Int): String = "s" + i
    def leaf(ints: List[Int]): FArray[String] = FArray.fromIterable(ints.map(elem))
    def empty: FArray[String] = FArray.empty[String]
    def concat(a: FArray[String], b: FArray[String]): FArray[String] = a ++ b
    def append(a: FArray[String], e: Int): FArray[String] = a :+ elem(e)
    def prepend(e: Int, a: FArray[String]): FArray[String] = elem(e) +: a
    def padTo(a: FArray[String], n: Int, f: Int): FArray[String] = a.padTo(n, elem(f))
    def updated(a: FArray[String], i: Int, e: Int): FArray[String] = a.updated(i, elem(e))
    def flatMapDup(a: FArray[String]): FArray[String] = a.flatMap(e => FArray(e, e))
    def flatMapSingle(a: FArray[String]): FArray[String] = a.flatMap(e => FArray(e))
    def flatMapWide(a: FArray[String]): FArray[String] = a.flatMap(e => FArray.tabulate(8)(_ => e))

  // ---------------------------------------------------------------------------
  // Interpret a recipe into both the FArray and the List oracle (same op chain).
  // Returns None when the recipe is structurally invalid for the current length
  // (e.g. updated index out of range) — such recipes are never enumerated, but
  // we guard anyway. List.padTo never truncates; List.slice/take/drop clamp.
  // ---------------------------------------------------------------------------
  def buildList(r: Recipe, K: Kind[?]): List[Int] = r match
    case Empty            => Nil
    case Leaf(es)         => es
    case Concat(a, b)     => buildList(a, K) ++ buildList(b, K)
    case Append(a, e)     => buildList(a, K) :+ e
    case Prepend(e, a)    => e :: buildList(a, K)
    case Take(a, k)       => buildList(a, K).take(k)
    case Drop(a, k)       => buildList(a, K).drop(k)
    case Slice(a, i, j)   => buildList(a, K).slice(i, j)
    case Reverse(a)       => buildList(a, K).reverse
    case PadTo(a, n, f)   => buildList(a, K).padTo(n, f)
    case Updated(a, i, e) =>
      val l = buildList(a, K)
      if i >= 0 && i < l.length then l.updated(i, e) else l // mirror FArray's valid-range only
    case FlatMapDup(a)    => buildList(a, K).flatMap(e => List(e, e))
    case FlatMapSingle(a) => buildList(a, K).flatMap(e => List(e))
    case FlatMapWide(a)   => buildList(a, K).flatMap(e => List.fill(8)(e))

  def buildF[E](r: Recipe)(using K: Kind[E]): FArray[E] = r match
    case Empty            => K.empty
    case Leaf(es)         => K.leaf(es)
    case Concat(a, b)     => K.concat(buildF[E](a), buildF[E](b))
    case Append(a, e)     => K.append(buildF[E](a), e)
    case Prepend(e, a)    => K.prepend(e, buildF[E](a))
    case Take(a, k)       => buildF[E](a).take(k)
    case Drop(a, k)       => buildF[E](a).drop(k)
    case Slice(a, i, j)   => buildF[E](a).slice(i, j)
    case Reverse(a)       => buildF[E](a).reverse
    case PadTo(a, n, f)   => K.padTo(buildF[E](a), n, f)
    case Updated(a, i, e) => K.updated(buildF[E](a), i, e)
    case FlatMapDup(a)    => K.flatMapDup(buildF[E](a))
    case FlatMapSingle(a) => K.flatMapSingle(buildF[E](a))
    case FlatMapWide(a)   => K.flatMapWide(buildF[E](a))

  // ---------------------------------------------------------------------------
  // Generator. Enumerate every shape up to depth D. base shapes = empty + leaves
  // of small int arrays (sizes 0..4 over alphabet 0..3, but we cap the leaf
  // population to keep the explosion bounded). Each composite op wraps sub-shapes
  // one level shallower. The structural ks/indices are swept over the small valid
  // range derived from the sub-shape's KNOWN length.
  // ---------------------------------------------------------------------------
  val Alphabet: List[Int] = List(0, 1, 2, 3)
  val LeafElem: List[Int] = List(0, 1, 2) // smaller per-leaf alphabet keeps leaf count sane

  // a representative, deduped set of small leaves of sizes 0..4
  val leaves: List[Recipe] =
    val sized: List[List[Int]] =
      (0 to 4).toList.flatMap { n =>
        if n == 0 then List(Nil)
        else
          // a handful of distinct contents per size, not the full cartesian product (3^4 = 81 is too many at depth)
          val plain = List.tabulate(n)(i => LeafElem(i % LeafElem.length))
          val rev = plain.reverse
          val const = List.fill(n)(1)
          val asc = List.tabulate(n)(i => i % 4)
          List(plain, rev, const, asc).distinct
      }
    Empty :: sized.distinct.map(Leaf.apply)

  /** all shapes of EXACTLY depth d (d=0 => the base leaves). bounded so the population stays tractable. */
  def shapesAt(d: Int, prev: List[Recipe]): List[Recipe] =
    if d == 0 then leaves
    else
      val sub = prev // shapes one level shallower (depth d-1)
      val buf = mutable.ListBuffer.empty[Recipe]
      // For combinators that take TWO sub-shapes (Concat) we'd blow up if we crossed all-with-all at every depth.
      // Cross a modest representative slice of `sub` for the binary op; unary ops sweep all of `sub`.
      val subForBinary = sub.take(18)
      for a <- sub do
        val la = buildList(a, summon[Kind[Int]])
        val n = la.length
        // ++ : concat with a representative set of sub-shapes
        for b <- subForBinary do buf += Concat(a, b)
        // :+ / +: over the small alphabet
        for e <- Alphabet.take(2) do { buf += Append(a, e); buf += Prepend(e, a) }
        // take / drop over the full valid range plus out-of-range probes
        for k <- (-1 to n + 1) do { buf += Take(a, k); buf += Drop(a, k) }
        // slice over a small grid of (i,j) including out-of-range and inverted
        for i <- (-1 to n + 1); j <- (i to n + 1) do buf += Slice(a, i, j)
        // reverse
        buf += Reverse(a)
        // flatMap -> ${K}FlatMap node (as a sub-shape, so the composite ops above wrap it next depth)
        buf += FlatMapDup(a)
        buf += FlatMapSingle(a)
        buf += FlatMapWide(a)
        // padTo to n-1 .. n+2 (truncation probe + growth) with two filler values
        for len <- (n - 1 to n + 2); f <- Alphabet.take(1) do buf += PadTo(a, len, f)
        // updated only at valid indices
        for i <- 0 until n; e <- Alphabet.take(1) do buf += Updated(a, i, e)
      buf.toList

  /** the full deduped population up to depth D, deduped by Int-oracle .toList so equivalent shapes collapse. */
  def allShapes(maxDepth: Int): List[Recipe] =
    val perDepth = mutable.ListBuffer.empty[List[Recipe]]
    var prev = leaves
    perDepth += leaves
    var d = 1
    while d <= maxDepth do
      val here = shapesAt(d, prev)
      perDepth += here
      prev = here
      d += 1
    // dedup the WHOLE population to bound the assert count. Key = (top-constructor-class, Int-oracle signature):
    // keep one representative per (signature, top-constructor) so we still exercise every node SHAPE even when two
    // shapes flatten to the same list (a Concat-of-leaves vs a plain leaf both flatten the same yet are worth testing).
    val seen = mutable.HashSet.empty[(String, List[Int])]
    val out = mutable.ListBuffer.empty[Recipe]
    val Kint = summon[Kind[Int]]
    for layer <- perDepth; r <- layer do
      val sig = buildList(r, Kint)
      val ctorTag = r.getClass.getSimpleName
      if seen.add((ctorTag, sig)) then out += r
    out.toList

  // ---------------------------------------------------------------------------
  // Findings sink.
  // ---------------------------------------------------------------------------
  sealed trait Finding
  final case class Wrong(recipe: String, op: String, far: String, lst: String) extends Finding:
    override def toString = s"WRONG  [$op]  recipe=$recipe\n         farray=$far\n         list  =$lst"
  final case class Hang(recipe: String, op: String) extends Finding:
    override def toString = s"HANG   [$op]  recipe=$recipe"

class FExhaustiveTest:
  import FExhaustive.*

  // depth/alphabet bound. D=3 with leaf sizes 0..4 over a 0..3 alphabet, structural ks swept over the small
  // valid range. Dedup by (signature, top-constructor) keeps the assert count in the tens of thousands.
  val Depth = 3

  private val timeoutMs = 5000L // per-SHAPE watchdog. A correct shape's whole op battery finishes in well under a ms;
  //                               this only fires on a non-terminating / pathologically-slow op.

  // ---------------------------------------------------------------------------
  // Per-shape watchdog state. The ENTIRE op battery for one (FArray, List) pair runs on a single worker thread
  // (not one thread per op — that was millions of spawns and blew the suite timeout). `currentOp` is updated by
  // the worker before each op so that, if the shape hangs, the main thread can name the exact op in flight.
  // ---------------------------------------------------------------------------
  private val currentOp = new AtomicReference[String]("") // op the worker is currently evaluating (for HANG attribution)
  private val currentRecipe = new AtomicReference[String]("")
  private val opCount = new java.util.concurrent.atomic.AtomicLong(0L) // total op-comparisons performed

  /** Compare one op's farray result vs its List oracle; record a Wrong on mismatch. Runs ON THE WORKER THREAD. Sets `currentOp` first so a hang inside `far` is
    * attributable. The List oracle (`lst`) never hangs.
    */
  private def cmp[R](op: String, sink: mutable.Buffer[Finding], far: => R, lst: => R): Unit =
    currentOp.set(op)
    opCount.incrementAndGet()
    val fv = far // may hang; the watchdog on the main thread will fire and the worker is abandoned (daemon)
    val lr = lst
    if fv != lr then synchronized { sink += Wrong(currentRecipe.get(), op, String.valueOf(fv), String.valueOf(lr)); () }

  /** Run the whole op battery for one shape on a worker thread; if it does not finish within `timeoutMs`, record a HANG naming the op in flight and CONTINUE
    * (the worker is a daemon and is abandoned, so a true infinite loop never wedges the sweep). Exceptions thrown by the battery are rethrown on the main
    * thread.
    */
  private def runShape(recipe: String, sink: mutable.Buffer[Finding])(battery: => Unit): Unit =
    // backstop per the brief: print+flush each shape's recipe before testing it (only the FIRST few + periodic, to
    // avoid drowning the log) — the per-op `currentOp` volatile is the precise attribution.
    currentRecipe.set(recipe)
    currentOp.set("build")
    val errRef = new AtomicReference[Throwable](null)
    val done = new java.util.concurrent.CountDownLatch(1)
    val t = new Thread(() =>
      try battery
      catch case e: Throwable => errRef.set(e)
      finally done.countDown()
    )
    t.setDaemon(true)
    t.start()
    val finished = done.await(timeoutMs, java.util.concurrent.TimeUnit.MILLISECONDS)
    if !finished then
      val op = currentOp.get()
      System.out.println(s"[HANG] op=$op recipe=$recipe")
      System.out.flush()
      sink += Hang(recipe, op)
      t.interrupt() // pure-CPU loops ignore this, but it's harmless; the daemon is then abandoned
    else
      val err = errRef.get()
      if err != null then throw err

  // ---------------------------------------------------------------------------
  // The op battery. Runs every op on one (FArray, List) pair; records findings.
  // Construction (`buildF`) runs INSIDE the worker too, so even construction-time
  // non-termination is caught and attributed to op="build".
  // ---------------------------------------------------------------------------
  // INLINE so the concrete element type (Int / String) is substituted for `E` at each call site — FArray's
  // lambda-taking ops dispatch on `${Kind}Repr[E]` via `summonFrom`, which only resolves when `E` is concrete.
  private inline def checkPair[E](r: Recipe, sink: mutable.Buffer[Finding])(using K: Kind[E]): Unit =
    val recipe = s"[${K.name}] $r"
    runShape(recipe, sink) {
      currentOp.set("build")
      val fa = buildF[E](r) // construction itself can hang on a pathological shape — attributed to "build"
      val la: List[Int] = buildList(r, K)
      val n = la.length

      // --- length / emptiness ---
      cmp("length", sink, fa.length, n)
      cmp("isEmpty", sink, fa.isEmpty, la.isEmpty)

      // --- whole-sequence materialization (the core traversal) ---
      cmp("toList", sink, fa.toList.map(toInt), la)
      cmp("iterator", sink, fa.iterator.toList.map(toInt), la)
      cmp("reverseIterator", sink, fa.reverseIterator.toList.map(toInt), la.reverse)
      cmp("foreach", sink, { val b = mutable.ListBuffer.empty[Int]; fa.foreach(e => b += toInt(e)); b.toList }, la)

      // --- folds / aggregates ---
      cmp("foldLeft", sink, fa.foldLeft(0)((z, e) => z * 31 + toInt(e) + 1), la.foldLeft(0)((z, e) => z * 31 + e + 1))
      cmp("foldRight", sink, fa.foldRight(0)((e, z) => z * 31 + toInt(e) + 1), la.foldRight(0)((e, z) => z * 31 + e + 1))
      cmp("count", sink, fa.count(e => toInt(e) % 2 == 0), la.count(_ % 2 == 0))
      cmp("mkString", sink, fa.mkString("[", ",", "]"), la.map(K.elem).mkString("[", ",", "]"))
      if K.name == "Int" then
        val fi = fa.asInstanceOf[FArray[Int]]
        val li = la
        cmp("sum", sink, fi.sum, li.sum)
        if n > 0 then
          cmp("min", sink, fi.min, li.min)
          cmp("max", sink, fi.max, li.max)

      // --- map / scan ---
      cmp("map(+1)", sink, fa.map(e => toInt(e) + 1).toList, la.map(_ + 1))
      cmp("map(toString)", sink, fa.map(e => String.valueOf(e)).toList, la.map(K.elem).map(String.valueOf))
      cmp("scanLeft", sink, fa.scanLeft(0)((z, e) => z + toInt(e)).toList, la.scanLeft(0)(_ + _))
      cmp("scanRight", sink, fa.scanRight(0)((e, z) => toInt(e) + z).toList, la.scanRight(0)(_ + _))

      // --- filtering / partition / collect ---
      cmp("filter", sink, fa.filter(e => toInt(e) % 2 == 0).toList.map(toInt), la.filter(_ % 2 == 0))
      cmp("filterNot", sink, fa.filterNot(e => toInt(e) % 2 == 0).toList.map(toInt), la.filterNot(_ % 2 == 0))
      cmp("collect", sink, fa.collect { case e if toInt(e) % 2 == 1 => toInt(e) }.toList, la.collect { case e if e % 2 == 1 => e })
      cmp("partition", sink, { val (a, b) = fa.partition(e => toInt(e) % 2 == 0); (a.toList.map(toInt), b.toList.map(toInt)) }, la.partition(_ % 2 == 0))
      cmp("takeWhile", sink, fa.takeWhile(e => toInt(e) < 2).toList.map(toInt), la.takeWhile(_ < 2))
      cmp("dropWhile", sink, fa.dropWhile(e => toInt(e) < 2).toList.map(toInt), la.dropWhile(_ < 2))
      cmp("span", sink, { val (a, b) = fa.span(e => toInt(e) < 2); (a.toList.map(toInt), b.toList.map(toInt)) }, la.span(_ < 2))

      // --- predicates / search ---
      cmp("exists", sink, fa.exists(e => toInt(e) == 2), la.exists(_ == 2))
      cmp("forall", sink, fa.forall(e => toInt(e) >= 0), la.forall(_ >= 0))
      cmp("find", sink, fa.find(e => toInt(e) == 2).map(toInt), la.find(_ == 2))
      cmp("indexWhere", sink, fa.indexWhere(e => toInt(e) == 2), la.indexWhere(_ == 2))
      cmp("indexOf", sink, fa.indexOf(K.elem(1)), la.indexOf(1))
      cmp("segmentLength", sink, fa.segmentLength(e => toInt(e) < 2), la.segmentLength(_ < 2, 0))
      cmp("lastIndexWhere", sink, fa.lastIndexWhere(e => toInt(e) == 1), la.lastIndexWhere(_ == 1))

      // --- structural ops (compare .toList) ---
      cmp("reverse", sink, fa.reverse.toList.map(toInt), la.reverse)
      cmp("++self", sink, (fa ++ fa).toList.map(toInt), la ++ la)
      cmp(":+", sink, (fa :+ K.elem(9)).toList.map(toInt), la :+ 9)
      cmp("+:", sink, (K.elem(9) +: fa).toList.map(toInt), 9 :: la)
      cmp("padTo+2", sink, K.padTo(fa, n + 2, 7).toList.map(toInt), la.padTo(n + 2, 7))
      // take/drop/slice sweep over a small index grid (incl. out-of-range)
      for k <- List(0, 1, n / 2, n - 1, n, n + 1) do
        cmp(s"take($k)", sink, fa.take(k).toList.map(toInt), la.take(k))
        cmp(s"drop($k)", sink, fa.drop(k).toList.map(toInt), la.drop(k))
      for i <- List(0, 1, n / 2); j <- List(i, i + 1, n, n + 1) do cmp(s"slice($i,$j)", sink, fa.slice(i, j).toList.map(toInt), la.slice(i, j))
      if n > 0 then cmp("updated(mid)", sink, K.updated(fa, n / 2, 5).toList.map(toInt), la.updated(n / 2, 5))

      // --- apply(i) for every i, plus head/last/tail/init where defined ---
      var i = 0
      while i < n do
        cmp(s"apply($i)", sink, toInt(fa.apply(i)), la(i))
        i += 1
      if n > 0 then
        cmp("head", sink, toInt(fa.head), la.head)
        cmp("last", sink, toInt(fa.last), la.last)
        cmp("tail", sink, fa.tail.toList.map(toInt), la.tail)
        cmp("init", sink, fa.init.toList.map(toInt), la.init)
    }

  // map an element of either kind back to its int alphabet code for oracle comparison
  private def toInt(e: Any): Int = e match
    case i: Int    => i
    case s: String => s.substring(1).toInt
    case other     => throw new MatchError(other)

  // Wall-clock budget so the test method always returns (and prints its summary) BEFORE bleep's ~120s suite
  // timeout would kill it mid-run with no report. A clean sweep at D=3 finishes well inside this.
  private val budgetMs = 100000L

  private def runSweep(): (Int, Int, List[Finding]) =
    val recipes = allShapes(Depth)
    val sink = mutable.ListBuffer.empty[Finding]
    val total = recipes.length
    val start = System.nanoTime()
    var idx = 0
    var aborted = false
    val it = recipes.iterator
    while it.hasNext && !aborted do
      val r = it.next()
      idx += 1
      if idx % 500 == 0 then
        val secs = (System.nanoTime() - start) / 1000000000.0
        System.out.println(f"  ... $idx/$total shapes (${secs}%.1fs, findings so far: ${sink.length})"); System.out.flush()
      checkPair[Int](r, sink)
      checkPair[String](r, sink)
      if (System.nanoTime() - start) / 1000000L > budgetMs then aborted = true
    if aborted then
      System.out.println(s"  !! wall-clock budget (${budgetMs}ms) exhausted after $idx/$total shapes — reporting partial sweep")
      System.out.flush()
    (idx, total, sink.toList)

  @Test def exhaustive_interpreter_parity(): Unit =
    val (covered, total, findings) = runSweep()
    val wrongs = findings.filter(_.isInstanceOf[Wrong])
    val hangs = findings.filter(_.isInstanceOf[Hang])
    val report = new StringBuilder
    def out(s: String): Unit = { report.append(s).append('\n'); System.out.println(s) }
    out("=" * 80)
    out(s"FExhaustive sweep: depth=$Depth, shapes covered=$covered/$total distinct (x2 kinds Int+String)")
    out(s"  op-comparisons: ${opCount.get()}")
    out(s"  findings: ${wrongs.length} WRONG, ${hangs.length} HANG")
    if wrongs.nonEmpty then
      out("-" * 40 + " WRONG ANSWERS " + "-" * 40)
      wrongs.take(300).foreach(w => out(w.toString))
      if wrongs.length > 300 then out(s"  ... and ${wrongs.length - 300} more")
    if hangs.nonEmpty then
      out("-" * 45 + " HANGS " + "-" * 45)
      hangs.distinct.take(300).foreach(h => out(h.toString))
    out("=" * 80)
    System.out.flush()
    // JUnit swallows a passing test's stdout, so also persist the summary to a file that survives a green run.
    try java.nio.file.Files.writeString(java.nio.file.Path.of(System.getProperty("java.io.tmpdir"), "fexhaustive-report.txt"), report.toString)
    catch case _: Throwable => ()
    assert(
      findings.isEmpty,
      s"FExhaustive found ${wrongs.length} wrong-answer and ${hangs.length} hang cases (see stdout / ${System.getProperty("java.io.tmpdir")}/fexhaustive-report.txt)"
    )
