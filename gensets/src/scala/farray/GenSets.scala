package farray

import bleep.*
import bleep.internal.FileUtils

import java.nio.file.Files

/** Generates the sealed FSet core hierarchy `SBase` (sealed Java, leaves `final`, NO node tag — §1.6.10) + the
  * per-kind-specialized `FSetOps` (the shared NON-inline leaf methods + the build/membership helpers) + the Java
  * `SetTraversers` (the membership-oracle predicate SAM + the foreach consumer SAM).
  *
  * Mirrors `GenCores` verbatim where it can: the `Emit` helper (real indentation, one statement per line), the
  * `Kind`/`Box` model, `summonFrom` kind dispatch on the REUSED `farray.Repr[A]` (so there is no second `Repr`),
  * the typed-`Object[]` Ref trick, and the "small shared non-inline leaf method, JIT-inlinable AND shareable"
  * discipline (§1.6.4).
  *
  * SCAFFOLD/M1 SCOPE (the green vertical slice): `SBase` + `SView`/`SMaterialized` intermediates + `Empty` +
  * `${K}One` + `${K}Sorted` (sorted prim leaf) + the lazy `Union`/`Inter`/`Diff` algebra nodes, plus the ops
  * `empty`/`apply`/`fromArray`/`fromFArray`/`from`/`contains`/`size`/`isEmpty`/`incl`(`+`)/`excl`(`-`). The
  * remaining FSet surface (Hash/Dense leaves, the traversal/materialize ops, `&`/`diff`/`xor`/`map`/`filter`/
  * `iterator`) is intentionally NOT emitted yet — see the NEXT-STEPS in the design.
  *
  * Adding a primitive = one row in `prims`. Edit THIS generator, never the generated sources.
  */
object GenSets extends BleepCodegenScript("GenSets") {

  // ===== the kind model (mirrors GenCores) =====

  /** the primitive boxing facet of a Kind (absent ⇒ a reference): the boxed wrapper plus how to box / unbox. */
  final case class Box(wrapper: String, prim: String) {
    def to(v: String): String = s"$wrapper.valueOf($v)"
    def from(v: String): String = s"scala.runtime.BoxesRunTime.unboxTo$prim($v)"
  }

  /** name=core prefix (Int/Long/Double/Ref); arr=Scala array elem type; jt=Java element type; dflt=zero. */
  final case class Kind(name: String, arr: String, jt: String, dflt: String, prim: Option[Box]) {
    def lc: String = name.toLowerCase
    def isPrim: Boolean = prim.isDefined
  }

  // Int/Long/Double primitives + the Ref kind. Add a row to cover more primitives.
  val opKinds: List[Kind] = List(
    Kind("Int", "Int", "int", "0", Some(Box("java.lang.Integer", "Int"))),
    Kind("Long", "Long", "long", "0L", Some(Box("java.lang.Long", "Long"))),
    Kind("Double", "Double", "double", "0.0", Some(Box("java.lang.Double", "Double"))),
    Kind("Ref", "Object", "Object", "null", None)
  )

  val prims: List[Kind] = opKinds.filter(_.isPrim)

  override def run(started: Started, commands: Commands, targets: List[GenSets.Target], args: List[String]): Unit =
    targets.foreach { target =>
      val dir = target.sources.resolve("farray")
      FileUtils.deleteDirectory(target.sources)
      Files.createDirectories(dir)
      // --- the sealed Java SBase core hierarchy (no node tag — §1.6.10) ---
      Files.writeString(dir.resolve("SBase.java"), sbase)
      Files.writeString(dir.resolve("SView.java"), sview)
      Files.writeString(dir.resolve("SMaterialized.java"), smaterialized)
      Files.writeString(dir.resolve("SEmpty.java"), sEmpty)
      opKinds.foreach { k =>
        Files.writeString(dir.resolve(s"S${k.name}One.java"), oneNode(k))
        Files.writeString(dir.resolve(s"S${k.name}Sorted.java"), sortedNode(k))
        Files.writeString(dir.resolve(s"S${k.name}Hash.java"), hashNode(k))
      }
      Files.writeString(dir.resolve("SUnion.java"), algebraNode("SUnion"))
      Files.writeString(dir.resolve("SInter.java"), algebraNode("SInter"))
      Files.writeString(dir.resolve("SDiff.java"), algebraNode("SDiff"))
      Files.writeString(dir.resolve("SXor.java"), algebraNode("SXor"))
      // --- predicate leaves (§2.5): answer contains by COMPUTATION, no element storage — possibly infinite,
      // so they are SView (not materializable). SIntRange/SLongRange = a contiguous [lo,hi] (above/below/
      // universal fall out via the domain bounds); SComplement = !inner. ---
      Files.writeString(dir.resolve("SIntRange.java"), rangeNode("Int", "int"))
      Files.writeString(dir.resolve("SLongRange.java"), rangeNode("Long", "long"))
      Files.writeString(dir.resolve("SComplement.java"), complementNode)
      // --- the Java SAM interfaces (membership oracle + foreach consumer), specialized per kind ---
      Files.writeString(dir.resolve("SetTraversers.java"), setTraversers)
      // --- the per-kind-specialized Scala ops (reuse farray.Repr; NO second Repr emitted) ---
      Files.writeString(dir.resolve("FSetOps.scala"), fsetOps)
    }

  // ===== Emit: indent-aware one-statement-per-line emitter (copied from GenCores §6) =====
  final class Emit(indentStr: String = "    ") {
    private val sb = new StringBuilder
    private var level = 0
    private def pad: String = indentStr * level
    def line(s: String): Emit = { sb.append(pad).append(s).append('\n'); this }
    def lines(s: String): Emit = {
      s.stripLineEnd.split("\n", -1).foreach(l => if l.isEmpty then sb.append('\n') else sb.append(pad).append(l).append('\n'))
      this
    }
    def blank(): Emit = { sb.append('\n'); this }
    def open(head: String): Emit = { line(head + " {"); level += 1; this }
    def close(): Emit = { level -= 1; line("}"); this }
    def closeOpen(head: String): Emit = { level -= 1; line("} " + head + " {"); level += 1; this }
    def scope(body: => Unit): Emit = { line("{"); level += 1; body; level -= 1; line("}"); this }
    def result: String = sb.toString
  }

  // ====================================================================================================
  // The sealed Java SBase core (§1.4 / §1.6.1). Two sealed intermediates give each public opaque type's
  // invariant a physical home: SView (lazy algebra) backs FSetView; SMaterialized (dedup'd, enumerable)
  // backs FSet/FSortedSet. NO node tag (§1.6.10) — plain sealed instanceof dispatch.
  // ====================================================================================================

  private def sbase: String = {
    val permits = "SView, SMaterialized"
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit.
       |// The sealed FSet core. Every FSet/FSetView/FSortedSet is an SBase at runtime (opaque type, zero
       |// wrapper). `size` is the cardinality of the (possibly lazy) set — leaves know it as a field; lazy
       |// algebra nodes report an UPPER BOUND here (exact size forces a materialize, §1.3); `isEmpty` is
       |// answered structurally where possible. No node tag (§1.6.10).
       |public sealed abstract class SBase permits $permits {
       |    /** Upper bound on the cardinality (exact for a materialized leaf; l.size + r.size for a lazy Union;
       |      * a loose bound for Inter/Diff). NOT the exact size for a lazy node — see SMaterialized.size(). */
       |    public abstract int sizeHint();
       |}
       |""".stripMargin
  }

  private def sview: String =
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit.
       |// The lazy-algebra half of the core: PURELY the Union/Inter/Diff/Xor combinator nodes (the deferred
       |// algebra). Backs FSet/FSetInfinite when deferred. A whole-set op materializes an SView into an
       |// SMaterialized leaf (the merge, §3.2) and memoizes it. Empty/One are SMaterialized (trivially enumerable).
       |public sealed abstract class SView extends SBase
       |    permits SUnion, SInter, SDiff, SXor, SIntRange, SLongRange, SComplement {
       |}
       |""".stripMargin

  private def smaterialized: String = {
    val permits = ("SEmpty" :: opKinds.map(k => s"S${k.name}One") ::: opKinds.map(k => s"S${k.name}Sorted") ::: opKinds.map(k => s"S${k.name}Hash")).mkString(", ")
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit.
       |// The deduplicated / enumerable half of the core: the materialized leaf shapes (M1: ${"$"}{K}Sorted;
       |// later ${"$"}{K}Hash, IntDense). Backs the FSet / FSortedSet opaque types. `size()` is the exact,
       |// O(1) cardinality field — only an SMaterialized answers it cheaply, which is precisely why you must
       |// materialize an FSetView before you can ask exact size / iterate it.
       |public sealed abstract class SMaterialized extends SBase permits $permits {
       |    /** the exact, O(1) cardinality. */
       |    public abstract int size();
       |    @Override public final int sizeHint() { return size(); }
       |}
       |""".stripMargin
  }

  /** the single empty set — kind-agnostic, a shared instance (mirrors FArray's Empty). SMaterialized: it is
    * trivially deduped/enumerable, so FSet.empty answers size()/iterate with no materialize step. */
  private def sEmpty: String =
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit. The single empty set (no elements, no kind).
       |public final class SEmpty extends SMaterialized {
       |    public static final SEmpty INSTANCE = new SEmpty();
       |    private SEmpty() {}
       |    @Override public int size() { return 0; }
       |}
       |""".stripMargin

  /** the array-free singleton, one per kind (no boxing of the primitive payload). SMaterialized (size 1). */
  private def oneNode(k: Kind): String =
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit. Singleton set: one element, no backing array.
       |public final class S${k.name}One extends SMaterialized {
       |    public final ${k.jt} elem;
       |    public S${k.name}One(${k.jt} elem) { this.elem = elem; }
       |    @Override public int size() { return 1; }
       |}
       |""".stripMargin

  /** the sorted-prim-array materialized leaf — the default leaf shape (§1.1). `arr` is SORTED and DEDUPED;
    * `size` is exactly `arr.length` (we never over-allocate the final array), so the JIT can bound a scan on
    * `arr.length` and drop the bounds check (§1.6.5). */
  private def sortedNode(k: Kind): String =
    if k.isPrim then
      s"""package farray;
         |
         |// GENERATED by GenSets — do not edit. SORTED + DEDUPED ${k.jt}[]-backed leaf; binary-search contains.
         |public final class S${k.name}Sorted extends SMaterialized {
         |    public final ${k.jt}[] arr;
         |    public S${k.name}Sorted(${k.jt}[] arr) { this.arr = arr; }
         |    @Override public int size() { return arr.length; }
         |}
         |""".stripMargin
    else
      // Ref Sorted: no value Ordering, so the leaf is sorted by (signed) hashCode, with a parallel `hashes`
      // array holding that same monotone key. This makes contains a binary-search-on-hash (+ tie-group .equals)
      // and lets the merge core two-pointer over two leaves' hashes — the unboxed Ref union/intersect/diff.
      s"""package farray;
         |
         |// GENERATED by GenSets — do not edit. Reference leaf sorted by hashCode; `hashes` parallels `arr`.
         |public final class S${k.name}Sorted extends SMaterialized {
         |    public final Object[] arr;
         |    public final int[] hashes;
         |    public S${k.name}Sorted(Object[] arr, int[] hashes) { this.arr = arr; this.hashes = hashes; }
         |    @Override public int size() { return arr.length; }
         |}
         |""".stripMargin

  /** the frozen open-addressing HASH leaf — the O(1)-contains workhorse past the size threshold (§2.3). The
    * compact form: a dense `arr` (sorted+distinct for prims — so it doubles as the merge's sorted input — any
    * order for Ref) + a sparse `index` of POSITIONS into `arr` (-1 = empty). Built once, frozen. Ref adds a
    * parallel `hashes` so a probe rejects a non-matching slot with one int compare before any .equals/deref. */
  private def hashNode(k: Kind): String =
    if k.isPrim then
      s"""package farray;
         |
         |// GENERATED by GenSets — do not edit. Frozen open-addressing ${k.jt} set: index[(mix(key))&(len-1)]
         |// linear-probes to a position in the sorted `arr`, or -1. O(1) contains; `arr` is the merge's sorted input.
         |public final class S${k.name}Hash extends SMaterialized {
         |    public final ${k.jt}[] arr;
         |    public final int[] index;
         |    public S${k.name}Hash(${k.jt}[] arr, int[] index) { this.arr = arr; this.index = index; }
         |    @Override public int size() { return arr.length; }
         |}
         |""".stripMargin
    else
      s"""package farray;
         |
         |// GENERATED by GenSets — do not edit. Frozen open-addressing reference set: index -> position in `arr`,
         |// `hashes` caches each element's hashCode so a probe skips .equals/deref on a miss. O(1) contains.
         |public final class S${k.name}Hash extends SMaterialized {
         |    public final Object[] arr;
         |    public final int[] hashes;
         |    public final int[] index;
         |    public S${k.name}Hash(Object[] arr, int[] hashes, int[] index) { this.arr = arr; this.hashes = hashes; this.index = index; }
         |    @Override public int size() { return arr.length; }
         |}
         |""".stripMargin

  /** a contiguous range predicate leaf (§2.5): contains = lo<=x<=hi, O(1), ~24 bytes for any range incl. a
    * billion elements. SView (membership-only / non-materializable here). above(k)=[k+1,MAX], below(k)=
    * [MIN,k-1], universal=[MIN,MAX] all fall out of the bounds (canonicalized in the factory). */
  private def rangeNode(name: String, jt: String): String =
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit. Contiguous [lo,hi] $jt range; O(1) contains, no element storage.
       |public final class S${name}Range extends SView {
       |    public final $jt lo;
       |    public final $jt hi;
       |    public S${name}Range($jt lo, $jt hi) { this.lo = lo; this.hi = hi; }
       |    @Override public int sizeHint() { long n = (long) hi - (long) lo + 1L; return n > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) n; }
       |}
       |""".stripMargin

  /** the complement predicate leaf: contains = !inner.contains. Co-finite (≈ infinite) over a bounded domain,
    * so SView / membership-only. Kind-agnostic (inner carries its kind). */
  private def complementNode: String =
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit. Complement: contains(x) = !inner.contains(x). Membership-only.
       |public final class SComplement extends SView {
       |    public final SBase inner;
       |    public SComplement(SBase inner) { this.inner = inner; }
       |    @Override public int sizeHint() { return Integer.MAX_VALUE; }
       |}
       |""".stripMargin

  /** a lazy 2-field algebra node — O(1) construct, shares both operands (§1.3). Kind-agnostic (the operands
    * carry their own kind). `sizeHint` is a cheap upper bound; exact size forces a materialize. */
  private def algebraNode(name: String): String = {
    val hint = name match {
      // Union/Xor: at most |l| + |r|. Inter/Diff: at most |l| (Inter ⊆ l, Diff ⊆ l).
      case "SUnion" | "SXor" => "left.sizeHint() + right.sizeHint()"
      case _                 => "left.sizeHint()"
    }
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit. Lazy ${name} algebra node — O(1) construct, shares l & r (§1.3).
       |// `memo` caches the materialized (deduped) leaf on first whole-set access (§3.2); @volatile gives the
       |// benign-race safe publication of the leaf's array fields. Null until materialized.
       |public final class $name extends SView {
       |    public final SBase left;
       |    public final SBase right;
       |    public volatile SBase memo;
       |    public $name(SBase left, SBase right) { this.left = left; this.right = right; }
       |    @Override public int sizeHint() { return $hint; }
       |}
       |""".stripMargin
  }

  // ====================================================================================================
  // The Java SAM interfaces (§1.6.6) — specialized on the element kind so primitives stay unboxed.
  //   ${I}Pred   { boolean test(${jt} v); }    — the membership oracle / filter predicate
  //   ${I}Consumer { void accept(${jt} v); }    — foreach
  // M1 only needs these two families (no fold/map SAM until the materialize/transform ops land).
  // ====================================================================================================

  private def setTraversers: String = {
    val e = new Emit()
    e.line("package farray;")
    e.blank()
    e.line("// GENERATED by GenSets — do not edit. SAM function types specialized on the element kind, so a")
    e.line("// primitive element never boxes (§1.6.6). M1: a predicate (membership oracle / filter) and a")
    e.line("// consumer (foreach). The cold tree walk over the lazy SView nodes lives in FSetOps (Scala),")
    e.line("// which references these statics directly (the NON-inline-forwarder rule, §1.6.8).")
    e.open("public final class SetTraversers")
    e.line("private SetTraversers() {}")
    e.blank()
    opKinds.foreach { k =>
      e.open(s"public interface ${k.name}Pred")
      e.line(s"boolean test(${k.jt} v);")
      e.close()
      e.open(s"public interface ${k.name}Consumer")
      e.line(s"void accept(${k.jt} v);")
      e.close()
    }
    e.close()
    e.result
  }

  // ====================================================================================================
  // FSetOps.scala — the per-kind-specialized ops. REUSES farray.Repr (generated by GenCores) verbatim; we
  // do NOT emit a second Repr. Kind dispatch is `summonFrom` on `${K}Repr[A]` exactly as in FArray.
  //
  // M1 GREEN SLICE: construction (empty/apply/fromArray/fromFArray/from), contains (the membership-
  // distributing tree walk + the sorted-leaf binary search), size/isEmpty, incl(+)/excl(-).
  // The "shared NON-inline leaf method" discipline (§1.6.4) applies to the membership walk: `containsLeaf${K}`
  // is the ONE place the leaf binary-search + the Union/Inter/Diff distribution lives; the inline surface only
  // dispatches the kind and unwraps the element. Build helpers (sort+unique-compact) are non-inline too.
  // ====================================================================================================

  private def fsetOps: String = {
    def dispatchA(body: Kind => String): String =
      "summonFrom {\n" + opKinds.map(k => s"      case r: ${k.name}Repr[A] => ${body(k)}").mkString("\n") + "\n    }"

    // indent a pre-rendered block one level (2 spaces) so the shared NON-inline helper methods sit cleanly
    // INSIDE `object FSetOps { ... }` (keeps Scala 3's optional-braces indent checker happy — §1.6.8 emit quality).
    def indent1(block: String): String =
      block.stripLineEnd.split("\n", -1).map(l => if l.isEmpty then l else "  " + l).mkString("\n")

    // Double set-equality must be NaN-correct and treat -0.0 as DISTINCT from +0.0 — parity with
    // immutable.Set, whose boxed java.lang.Double.equals is bit-equality (NaN==NaN, -0.0≠+0.0). So compare
    // doubles via java.lang.Double.compare (compare==0 iff .equals; total order == Arrays.sort's order).
    // Int/Long keep raw ==/</>; Ref uses Scala == (value-equals).
    def isEq(k: Kind, a: String, b: String): String =
      if k.name == "Double" then s"(java.lang.Double.compare($a, $b) == 0)" else s"($a == $b)"
    def isLt(k: Kind, a: String, b: String): String =
      if k.name == "Double" then s"(java.lang.Double.compare($a, $b) < 0)" else s"($a < $b)"
    def notEq(k: Kind, a: String, b: String): String =
      if k.name == "Double" then s"(java.lang.Double.compare($a, $b) != 0)" else s"($a != $b)"

    // the slot hash for the frozen open-addressing leaf: an avalanche (mixInt/mixLong) over the raw bits, so a
    // power-of-two mask keeps full entropy. Double goes through doubleToLongBits (NaN canonical, ±0.0 distinct).
    def slotHash(k: Kind, e: String): String = k.name match
      case "Int"    => s"mixInt($e)"
      case "Long"   => s"mixLong($e)"
      case "Double" => s"mixLong(java.lang.Double.doubleToLongBits($e))"
      case _        => s"mixInt(($e).hashCode())"

    // unwrap an A-typed element to its raw kind value (prim → the primitive; Ref → cast to Object).
    def unwrapElem(k: Kind, e: String): String = if k.name == "Ref" then s"($e).asInstanceOf[Object]" else s"r.unwrap($e)"
    // WRAP a raw kind value back to A (for handing a leaf element to the user's inline lambda — unboxed).
    def wrapElem(k: Kind, v: String): String = if k.name == "Ref" then s"($v).asInstanceOf[A]" else s"r.wrap($v)"
    // unwrap a B-typed element (incl/excl dispatch on the element kind B, like FArray's appendImpl).
    def unwrapElemB(k: Kind, e: String): String = if k.name == "Ref" then s"($e).asInstanceOf[Object]" else s"r.unwrap($e)"
    // store an A into an Object[] (Ref) or the prim into its array — used by the build path.
    def storeRaw(k: Kind, e: String): String = if k.name == "Ref" then s"($e).asInstanceOf[Object]" else e

    // ---- the membership oracle: shared NON-inline leaf methods, ONE per kind (§1.6.4) ----------------
    // `containsLeaf${K}` answers membership for a primitive kind by distributing over the lazy algebra
    // (§1.3): Union short-circuits on the first hit (proving), Inter on the first miss (disproving), Diff is
    // left && !right. A materialized ${K}Sorted leaf is a BRANCHLESS binary search; One/Empty are trivial.
    // It is non-inline, lives in FSetOps' own class, and so references the SetTraversers SAM types directly
    // with no module ref (dodges the §1.6.8 NoClassDefFoundError gotcha when the inline surface splices it).
    val containsLeaves = {
      val ee = new Emit("  ")
      opKinds.foreach { k =>
        val K = k.name
        // ITERATIVE left-spine trampoline (§2.4): loop the unbounded LEFT spine of a Union/Inter/Diff chain
        // and recurse only on the (shallow) right children — so a deep +/++/- chain (which is LEFT-deep)
        // cannot StackOverflow. Sorted leaf = branchless binary search (Double via java.lang.Double.compare,
        // NaN-correct); One/Empty trivial. Union short-circuits on first hit, Inter/Diff on first miss.
        ee.open(s"def containsLeaf$K(node0: SBase, e: ${k.arr}): Boolean =")
        ee.line("var node: SBase = node0")
        ee.open("while (true)")
        ee.open("node match")
        ee.line("case _: SEmpty => return false")
        ee.line(s"case o: S${K}One => return ${isEq(k, "o.elem", "e")}")
        ee.open(s"case s: S${K}Sorted =>")
        ee.line("val a = s.arr")
        if k.isPrim then {
          // binary search over the sorted prim array; bound on a.length (§1.6.5).
          ee.line("var lo = 0")
          ee.line("var hi = a.length - 1")
          ee.line("var found = false")
          ee.open("while (lo <= hi && !found)")
          ee.line("val mid = (lo + hi) >>> 1")
          ee.line("val v = a(mid)")
          ee.line(s"if (${isEq(k, "v", "e")}) found = true")
          ee.line(s"else if (${isLt(k, "v", "e")}) lo = mid + 1")
          ee.line("else hi = mid - 1")
          ee.close()
          ee.line("return found")
        } else {
          // Ref leaf: hash-sorted, so binary-search the cached signed hash, then scan the equal-hash tie-group
          // (expected size ~1) with .equals. O(log n + tie-group) — replaces the old O(n) linear equals scan.
          ee.line("val hs = s.hashes")
          ee.line("val hc = e.hashCode()")
          ee.line("var lo = 0; var hi = a.length - 1; var found = false")
          ee.open("while (lo <= hi && !found)")
          ee.line("val mid = (lo + hi) >>> 1")
          ee.line("val hm = hs(mid)")
          ee.line("if (hm < hc) lo = mid + 1")
          ee.line("else if (hm > hc) hi = mid - 1")
          ee.open("else")
          // hash hit: scan left then right over the contiguous equal-hash run for an .equals match, then stop.
          ee.line("var t = mid")
          ee.line("while (t >= 0 && hs(t) == hc && !found) { if (a(t).equals(e)) found = true; t -= 1 }")
          ee.line("t = mid + 1")
          ee.line("while (t < a.length && hs(t) == hc && !found) { if (a(t).equals(e)) found = true; t += 1 }")
          ee.line("lo = hi + 1")
          ee.close()
          ee.close()
          ee.line("return found")
        }
        ee.close() // case Sorted
        ee.line(s"case u: SUnion => if (containsLeaf$K(u.right, e)) return true else node = u.left")
        ee.line(s"case n: SInter => if (!containsLeaf$K(n.right, e)) return false else node = n.left")
        ee.line(s"case d: SDiff => if (containsLeaf$K(d.right, e)) return false else node = d.left")
        // Xor needs BOTH children (no short-circuit, no left-spine) — both recurse (xor trees are not loop-built).
        ee.line(s"case x: SXor => return containsLeaf$K(x.left, e) ^ containsLeaf$K(x.right, e)")
        // frozen open-addressing leaf — O(1) probe (§2.3): mix → slot → linear-probe index → position → compare.
        ee.open(s"case h: S${K}Hash =>")
        ee.line("val idx = h.index")
        ee.line("val arr = h.arr")
        if k.isPrim then {
          ee.line(s"var slot = (${slotHash(k, "e")}) & (idx.length - 1)")
          ee.line("var res = false; var go = true")
          ee.open("while (go)")
          ee.line("val p = idx(slot)")
          ee.line("if (p == -1) go = false")
          ee.line(s"else if (${isEq(k, "arr(p)", "e")}) { res = true; go = false }")
          ee.line("else slot = (slot + 1) & (idx.length - 1)")
          ee.close()
          ee.line("return res")
        } else {
          ee.line("val hs = h.hashes")
          ee.line("val hc = e.hashCode()")
          ee.line("var slot = mixInt(hc) & (idx.length - 1)")
          ee.line("var res = false; var go = true")
          ee.open("while (go)")
          ee.line("val p = idx(slot)")
          ee.line("if (p == -1) go = false")
          ee.line("else if (hs(p) == hc && arr(p).equals(e)) { res = true; go = false }")
          ee.line("else slot = (slot + 1) & (idx.length - 1)")
          ee.close()
          ee.line("return res")
        }
        ee.close()
        // predicate leaves (§2.5): O(1)-by-computation, distribute through the algebra, never materialize.
        if k.name == "Int" then ee.line("case r: SIntRange => return r.lo <= e && e <= r.hi")
        if k.name == "Long" then ee.line("case r: SLongRange => return r.lo <= e && e <= r.hi")
        ee.line(s"case c: SComplement => return !containsLeaf$K(c.inner, e)")
        ee.line("case _ => return false")
        ee.close() // node match
        ee.close() // while
        ee.line("false")
        ee.close() // def
      }
      ee.result
    }

    // ---- the BUILD path: sort + unique-compact a raw kind array into a ${K}Sorted leaf (§3.1) ----------
    // Non-inline, one per kind. Takes an over-allocated raw array of `len` raw elements, sorts it (unboxed
    // dual-pivot for prims; Arrays.sort with the Ordering-derived comparator is a NEXT-STEP for Ref — M1 Ref
    // build does an equals-based linear dedup, no sort, so a Ref set has no order guarantee yet), drops equal
    // neighbours, and wraps the trimmed array into Empty/${K}One/${K}Sorted (the size-0/1 canonicalization).
    val buildLeaves = {
      val ee = new Emit("  ")
      opKinds.foreach { k =>
        val K = k.name
        val arrT = if k.name == "Ref" then "Object" else k.arr
        ee.open(s"def buildSorted$K(raw: Array[$arrT], len: Int): SBase =")
        ee.line("if (len == 0) return SEmpty.INSTANCE")
        if k.isPrim then {
          // sort the first `len` slots in place, then unique-compact dropping equal neighbours.
          ee.line("java.util.Arrays.sort(raw, 0, len)")
          ee.line("var w = 1")
          ee.line("var i = 1")
          ee.open("while (i < len)")
          ee.open(s"if (${notEq(k, "raw(i)", "raw(i - 1)")})")
          ee.line("raw(w) = raw(i)")
          ee.line("w += 1")
          ee.close()
          ee.line("i += 1")
          ee.close()
          ee.line(s"if (w == 1) new S${K}One(raw(0))")
          // small → Sorted (cache-local binary search, no table); large → frozen Hash (O(1) probe). §2.2/§2.3.
          ee.line(s"else { val trimmed = java.util.Arrays.copyOf(raw, w); if (w <= 16) new S${K}Sorted(trimmed) else buildHash$K(trimmed) }")
        } else {
          // Ref: dedup DURING an open-addressing insert (O(n) expected) — replaces the old O(n²) equals scan.
          // Build the index + parallel hashes inline; trim arr/hashes to the distinct count w. The index is
          // sized for `len` (slightly over for heavy-dup inputs) but only references positions 0..w-1.
          ee.line("var cap = 8")
          ee.line("while (cap < len * 2) cap <<= 1")
          ee.line("val index = new Array[Int](cap)")
          ee.line("java.util.Arrays.fill(index, -1)")
          ee.line("val arr = new Array[Object](len)")
          ee.line("val hashes = new Array[Int](len)")
          ee.line("var w = 0")
          ee.line("var i = 0")
          ee.open("while (i < len)")
          ee.line("val v = raw(i)")
          ee.line("val hc = v.hashCode()")
          ee.line("var slot = mixInt(hc) & (cap - 1)")
          ee.line("var dup = false")
          ee.open("while (index(slot) != -1 && !dup)")
          ee.line("val p = index(slot)")
          ee.line("if (hashes(p) == hc && arr(p).equals(v)) dup = true")
          ee.line("else slot = (slot + 1) & (cap - 1)")
          ee.close()
          ee.open("if (!dup)")
          ee.line("arr(w) = v; hashes(w) = hc; index(slot) = w; w += 1")
          ee.close()
          ee.line("i += 1")
          ee.close()
          ee.line("if (w == 0) SEmpty.INSTANCE")
          ee.line(s"else if (w == 1) new S${K}One(arr(0))")
          // sort the distinct elements by hashCode, then wrap (≤16 → Sorted, else frozen Hash). The dedup index
          // was over the unsorted arr, so wrapRef rebuilds it over the sorted leaf (build path, not hot).
          ee.open("else")
          ee.line("val ta = java.util.Arrays.copyOf(arr, w)")
          ee.line("val th = java.util.Arrays.copyOf(hashes, w)")
          ee.line(s"sortByHash$K(ta, th, w)")
          ee.line(s"wrap$K(ta, th)")
          ee.close()
        }
        ee.close()
      }
      ee.result
    }

    // ---- the frozen HASH leaf build (§2.3): shared avalanche mixes + per-kind `buildHash` (open-addressing
    // index over the dense `arr`, load ~0.5, linear probe). Prims index by the raw-bits mix; Ref caches each
    // element's hashCode in a parallel array so a probe rejects misses with one int compare.
    val hashHelpers = {
      val ee = new Emit("  ")
      ee.line("def mixInt(h0: Int): Int = { var h = h0; h ^= h >>> 16; h *= 0x85ebca6b; h ^= h >>> 13; h *= 0xc2b2ae35; h ^= h >>> 16; h }")
      ee.line("def mixLong(h0: Long): Int = { var h = h0; h ^= h >>> 33; h *= 0xff51afd7ed558ccdL.toLong; h ^= h >>> 33; h *= 0xc4ceb9fe1a85ec53L.toLong; h ^= h >>> 33; (h ^ (h >>> 32)).toInt }")
      opKinds.foreach { k =>
        val K = k.name
        if k.isPrim then {
          val P = k.arr
          ee.open(s"def buildHash$K(arr: Array[$P]): SBase =")
          ee.line("val n = arr.length")
          ee.line("var cap = 8")
          ee.line("while (cap < n * 2) cap <<= 1")
          ee.line("val index = new Array[Int](cap)")
          ee.line("java.util.Arrays.fill(index, -1)")
          ee.line("var p = 0")
          ee.open("while (p < n)")
          ee.line(s"var slot = (${slotHash(k, "arr(p)")}) & (cap - 1)")
          ee.line("while (index(slot) != -1) slot = (slot + 1) & (cap - 1)")
          ee.line("index(slot) = p")
          ee.line("p += 1")
          ee.close()
          ee.line(s"new S${K}Hash(arr, index)")
          ee.close()
        } else {
          ee.open(s"def buildHash$K(arr: Array[Object]): SBase =")
          ee.line("val n = arr.length")
          ee.line("val hashes = new Array[Int](n)")
          ee.line("var i = 0")
          ee.line("while (i < n) { hashes(i) = arr(i).hashCode(); i += 1 }")
          ee.line("var cap = 8")
          ee.line("while (cap < n * 2) cap <<= 1")
          ee.line("val index = new Array[Int](cap)")
          ee.line("java.util.Arrays.fill(index, -1)")
          ee.line("var p = 0")
          ee.open("while (p < n)")
          ee.line("var slot = mixInt(hashes(p)) & (cap - 1)")
          ee.line("while (index(slot) != -1) slot = (slot + 1) & (cap - 1)")
          ee.line("index(slot) = p")
          ee.line("p += 1")
          ee.close()
          ee.line(s"new S${K}Hash(arr, hashes, index)")
          ee.close()
        }
      }
      ee.result
    }

    // ---- the materialize / merge CORE (§3.2): fold a lazy algebra tree into ONE sorted leaf, memoized on the
    // node. PRIMS only — Ref has no Ordering threaded yet, so a Ref tree stays on the boxed collectElems walk.
    // Each merge is an O(m+n) unboxed two-pointer pass over two sorted+distinct arrays; cmp via ${W}.compare so
    // Double is NaN/±0.0-correct and consistent with the leaf's Arrays.sort order. Recursive fold = correct for
    // SHALLOW algebra (the common case); a deep union spine wants an iterative k-way merge (NEXT-STEP) — contains
    // already handles deep chains, so materialize/size on them is the only bounded gap.
    val mergeCore = {
      val ee = new Emit("  ")
      prims.foreach { k =>
        val K = k.name
        val P = k.arr
        val W = k.prim.get.wrapper
        // extract a materialized leaf's sorted+distinct array
        ee.open(s"def asArr$K(node: SBase): Array[$P] = node match")
        ee.line(s"case _: SEmpty => new Array[$P](0)")
        ee.line(s"case o: S${K}One => { val a = new Array[$P](1); a(0) = o.elem; a }")
        ee.line(s"case s: S${K}Sorted => s.arr")
        ee.line(s"case h: S${K}Hash => h.arr")
        ee.line(s"case _ => new Array[$P](0)")
        ee.close()
        // wrap a sorted+distinct array into the canonical leaf. Mirror buildSorted's threshold: ≤16 → Sorted
        // (cache-local binary search), else the frozen Hash (O(1) probe) — so a materialized merge result is
        // closed under the same leaf-kind choice as a fresh build (post-merge contains stays O(1), not O(log n)).
        ee.open(s"def wrap$K(arr: Array[$P]): SBase =")
        ee.line("if (arr.length == 0) SEmpty.INSTANCE")
        ee.line(s"else if (arr.length == 1) new S${K}One(arr(0))")
        ee.line(s"else if (arr.length <= 16) new S${K}Sorted(arr) else buildHash$K(arr)")
        ee.close()
        // the four unboxed sorted merges (a, b sorted+distinct; out trimmed to w).
        def merge(name: String, outLen: String, body: Emit => Unit, tailA: Boolean, tailB: Boolean): Unit = {
          ee.open(s"def $name$K(a: Array[$P], b: Array[$P]): Array[$P] =")
          ee.line(s"val out = new Array[$P]($outLen)")
          ee.line("var i = 0; var j = 0; var w = 0")
          ee.open("while (i < a.length && j < b.length)")
          ee.line(s"val c = $W.compare(a(i), b(j))")
          body(ee)
          ee.close()
          if tailA then ee.line("while (i < a.length) { out(w) = a(i); w += 1; i += 1 }")
          if tailB then ee.line("while (j < b.length) { out(w) = b(j); w += 1; j += 1 }")
          ee.line("java.util.Arrays.copyOf(out, w)")
          ee.close()
        }
        merge("mergeUnion", "a.length + b.length", e => {
          e.line("if (c < 0) { out(w) = a(i); w += 1; i += 1 }")
          e.line("else if (c > 0) { out(w) = b(j); w += 1; j += 1 }")
          e.line("else { out(w) = a(i); w += 1; i += 1; j += 1 }")
        }, tailA = true, tailB = true)
        merge("mergeInter", "scala.math.min(a.length, b.length)", e => {
          e.line("if (c < 0) i += 1")
          e.line("else if (c > 0) j += 1")
          e.line("else { out(w) = a(i); w += 1; i += 1; j += 1 }")
        }, tailA = false, tailB = false)
        merge("mergeDiff", "a.length", e => {
          e.line("if (c < 0) { out(w) = a(i); w += 1; i += 1 }")
          e.line("else if (c > 0) j += 1")
          e.line("else { i += 1; j += 1 }")
        }, tailA = true, tailB = false)
        merge("mergeXor", "a.length + b.length", e => {
          e.line("if (c < 0) { out(w) = a(i); w += 1; i += 1 }")
          e.line("else if (c > 0) { out(w) = b(j); w += 1; j += 1 }")
          e.line("else { i += 1; j += 1 }")
        }, tailA = true, tailB = true)
        // materialize: fold + memoize. A leaf returns itself; an algebra node merges its materialized children.
        ee.open(s"def materialize$K(node: SBase): SBase = node match")
        ee.line(s"case _: SMaterialized => node")
        def algebra(tpe: String, bind: String, merge: String): Unit = {
          ee.open(s"case $bind: $tpe =>")
          ee.line(s"val m = $bind.memo")
          ee.line(s"if (m != null) m else { val r = wrap$K($merge$K(asArr$K(materialize$K($bind.left)), asArr$K(materialize$K($bind.right)))); $bind.memo = r; r }")
          ee.close()
        }
        algebra("SUnion", "u", "mergeUnion")
        algebra("SInter", "n", "mergeInter")
        algebra("SDiff", "d", "mergeDiff")
        algebra("SXor", "x", "mergeXor")
        ee.line("""case _ => throw new UnsupportedOperationException("cannot materialize/enumerate an infinite or predicate set (range/above/below/universal/complement) — only contains is defined")""")
        ee.close()
      }
      // ---- the Ref merge CORE (§3.2 for references): leaves are hash-sorted (a parallel signed-hashCode key),
      // so union/intersect/diff/xor are a two-pointer merge over the cached hashes, with .equals confined to the
      // equal-hash tie-group (expected size ~1). Zero hashCode recompute; the output is itself hash-sorted, so
      // the algebra is CLOSED over the leaf — chained materialized Ref algebra stays unboxed.
      {
        // sort arr[0..w) + hashes[0..w) IN PLACE ascending by signed hash, via a packed (hash<<32 | index) sort.
        ee.open("def sortByHashRef(arr: Array[Object], hashes: Array[Int], w: Int): Unit =")
        ee.line("val packed = new Array[Long](w)")
        ee.line("var k = 0")
        ee.line("while (k < w) { packed(k) = (hashes(k).toLong << 32) | (k.toLong & 0xffffffffL); k += 1 }")
        ee.line("java.util.Arrays.sort(packed)")
        ee.line("val tmp = new Array[Object](w)")
        ee.line("k = 0")
        ee.open("while (k < w)")
        ee.line("val p = packed(k)")
        ee.line("tmp(k) = arr((p & 0xffffffffL).toInt)")
        ee.line("hashes(k) = (p >> 32).toInt")
        ee.line("k += 1")
        ee.close()
        ee.line("System.arraycopy(tmp, 0, arr, 0, w)")
        ee.close()
        // extract the hash-sorted element array / its parallel hash key from any materialized Ref node.
        ee.open("def asArrRef(node: SBase): Array[Object] = node match")
        ee.line("case _: SEmpty => new Array[Object](0)")
        ee.line("case o: SRefOne => { val a = new Array[Object](1); a(0) = o.elem; a }")
        ee.line("case s: SRefSorted => s.arr")
        ee.line("case h: SRefHash => h.arr")
        ee.line("case _ => new Array[Object](0)")
        ee.close()
        ee.open("def hashesOfRef(node: SBase): Array[Int] = node match")
        ee.line("case _: SEmpty => new Array[Int](0)")
        ee.line("case o: SRefOne => { val a = new Array[Int](1); a(0) = o.elem.hashCode(); a }")
        ee.line("case s: SRefSorted => s.hashes")
        ee.line("case h: SRefHash => h.hashes")
        ee.line("case _ => new Array[Int](0)")
        ee.close()
        // wrap a hash-sorted arr+hashes into the canonical leaf (≤16 → Sorted; else build the open-addressing index).
        ee.open("def wrapRef(arr: Array[Object], hashes: Array[Int]): SBase =")
        ee.line("val n = arr.length")
        ee.line("if (n == 0) SEmpty.INSTANCE")
        ee.line("else if (n == 1) new SRefOne(arr(0))")
        ee.line("else if (n <= 16) new SRefSorted(arr, hashes)")
        ee.open("else")
        ee.line("var cap = 8")
        ee.line("while (cap < n * 2) cap <<= 1")
        ee.line("val index = new Array[Int](cap)")
        ee.line("java.util.Arrays.fill(index, -1)")
        ee.line("var p = 0")
        ee.open("while (p < n)")
        ee.line("var slot = mixInt(hashes(p)) & (cap - 1)")
        ee.line("while (index(slot) != -1) slot = (slot + 1) & (cap - 1)")
        ee.line("index(slot) = p")
        ee.line("p += 1")
        ee.close()
        ee.line("new SRefHash(arr, hashes, index)")
        ee.close()
        ee.close()
        // the four tie-group-aware merges. aA/aH and bA/bH are hash-sorted parallel (element, signed-hash) arrays.
        // gather the equal-hash run on each side, then resolve membership inside it with .equals (g~1 expected).
        def refMerge(name: String, outLen: String, lt: String, gt: String, tieAllA: Boolean,
                     tieKeepIfFound: Boolean, emitBNotInA: Boolean, tailA: Boolean, tailB: Boolean): Unit = {
          ee.open(s"def $name(aA: Array[Object], aH: Array[Int], bA: Array[Object], bH: Array[Int]): SBase =")
          ee.line("val na = aA.length; val nb = bA.length")
          ee.line(s"val outA = new Array[Object]($outLen)")
          ee.line(s"val outH = new Array[Int]($outLen)")
          ee.line("var i = 0; var j = 0; var w = 0")
          ee.open("while (i < na && j < nb)")
          ee.line("val ha = aH(i); val hb = bH(j)")
          ee.line(s"if (ha < hb) { $lt }")
          ee.line(s"else if (ha > hb) { $gt }")
          ee.open("else")
          ee.line("var ie = i; while (ie < na && aH(ie) == ha) ie += 1")
          ee.line("var je = j; while (je < nb && bH(je) == hb) je += 1")
          if tieAllA then
            // union: keep the whole a-group unconditionally.
            ee.line("var k = i; while (k < ie) { outA(w) = aA(k); outH(w) = ha; w += 1; k += 1 }")
          else {
            // inter/diff/xor: keep aA(k) iff (found-in-b == tieKeepIfFound).
            ee.line("var k = i")
            ee.open("while (k < ie)")
            ee.line("val v = aA(k)")
            ee.line("var found = false; var m = j")
            ee.line("while (m < je && !found) { if (bA(m).equals(v)) found = true; m += 1 }")
            ee.line(s"if (found == ${tieKeepIfFound}) { outA(w) = v; outH(w) = ha; w += 1 }")
            ee.line("k += 1")
            ee.close()
          }
          if emitBNotInA then {
            // b-group: emit bA(k) iff NOT equal to any a-group element (union & xor).
            ee.line("k = j")
            ee.open("while (k < je)")
            ee.line("val v = bA(k)")
            ee.line("var found = false; var m = i")
            ee.line("while (m < ie && !found) { if (aA(m).equals(v)) found = true; m += 1 }")
            ee.line("if (!found) { outA(w) = v; outH(w) = hb; w += 1 }")
            ee.line("k += 1")
            ee.close()
          }
          ee.line("i = ie; j = je")
          ee.close()
          ee.close()
          if tailA then ee.line("while (i < na) { outA(w) = aA(i); outH(w) = aH(i); w += 1; i += 1 }")
          if tailB then ee.line("while (j < nb) { outA(w) = bA(j); outH(w) = bH(j); w += 1; j += 1 }")
          ee.line("wrapRef(java.util.Arrays.copyOf(outA, w), java.util.Arrays.copyOf(outH, w))")
          ee.close()
        }
        // union: keep all of a's group + b's group-not-in-a; carry both unique-hash tails.
        refMerge("mergeUnionRef", "na + nb", "outA(w) = aA(i); outH(w) = ha; w += 1; i += 1",
          "outA(w) = bA(j); outH(w) = hb; w += 1; j += 1", tieAllA = true, tieKeepIfFound = true, emitBNotInA = true, tailA = true, tailB = true)
        // intersect: keep a-group elements that ARE in b; drop unique-hash runs on both sides; no tails.
        refMerge("mergeInterRef", "if (na < nb) na else nb", "i += 1", "j += 1",
          tieAllA = false, tieKeepIfFound = true, emitBNotInA = false, tailA = false, tailB = false)
        // diff a∖b: keep a-group elements NOT in b; carry a's unique-hash run; tail a.
        refMerge("mergeDiffRef", "na", "outA(w) = aA(i); outH(w) = ha; w += 1; i += 1", "j += 1",
          tieAllA = false, tieKeepIfFound = false, emitBNotInA = false, tailA = true, tailB = false)
        // xor a△b: a-group not in b + b-group not in a; carry both unique-hash runs; both tails.
        refMerge("mergeXorRef", "na + nb", "outA(w) = aA(i); outH(w) = ha; w += 1; i += 1",
          "outA(w) = bA(j); outH(w) = hb; w += 1; j += 1", tieAllA = false, tieKeepIfFound = false, emitBNotInA = true, tailA = true, tailB = true)
        // materialize: fold + memoize (children materialized once, only when the memo is unset).
        ee.open("def materializeRef(node: SBase): SBase = node match")
        ee.line("case _: SMaterialized => node")
        def algebraRef(tpe: String, bind: String, merge: String): Unit = {
          ee.open(s"case $bind: $tpe =>")
          ee.line(s"val m = $bind.memo")
          ee.line(s"if (m != null) m else { val l = materializeRef($bind.left); val rr = materializeRef($bind.right); val r = $merge(asArrRef(l), hashesOfRef(l), asArrRef(rr), hashesOfRef(rr)); $bind.memo = r; r }")
          ee.close()
        }
        algebraRef("SUnion", "u", "mergeUnionRef")
        algebraRef("SInter", "n", "mergeInterRef")
        algebraRef("SDiff", "d", "mergeDiffRef")
        algebraRef("SXor", "x", "mergeXorRef")
        ee.line("""case _ => throw new UnsupportedOperationException("cannot materialize/enumerate an infinite or predicate Ref set — only contains is defined")""")
        ee.close()
      }
      ee.result
    }

    // ---- the surface impls (inline, dispatch the kind, route to the shared non-inline helpers) ----------

    // contains: dispatch the kind, unwrap the element, call the shared containsLeaf${K}.
    val containsV = dispatchA(k => s"containsLeaf${k.name}(xs, ${unwrapElem(k, "elem")})")

    // empty: the shared SEmpty (kind-agnostic).
    val emptyV = "(SEmpty.INSTANCE: SBase)"

    // fromValues{1,2,3}: small-arity build paths that avoid varargs boxing. Each unwraps into a raw scratch
    // array and routes through buildSorted${K} (dedup + sort). One element short-circuits to ${K}One.
    val fromValues1 = dispatchA(k => s"(new S${k.name}One(${unwrapElem(k, "a")})): SBase")
    def fromValuesN(n: Int): String = {
      val params = (1 to n).map(i => s"p$i")
      dispatchA { k =>
        val arrT = if k.name == "Ref" then "Object" else k.arr
        val stores = params.map(p => unwrapElem(k, p)).mkString(", ")
        s"buildSorted${k.name}(Array[$arrT]($stores), $n)"
      }
    }

    // fromArray: unwrap the source Array[A] into a raw scratch array, then build. The source is copied (we
    // sort in place and must not mutate the caller's array). Ref reuses a fresh Object[] (typed-ref read needs
    // no per-elem checkcast on the way IN; reads back out are a NEXT-STEP transform op).
    val fromArrayV = dispatchA { k =>
      val arrT = if k.name == "Ref" then "Object" else k.arr
      if k.isPrim then
        s"{ val src = as.asInstanceOf[Array[${k.arr}]]; val n = src.length; buildSorted${k.name}(java.util.Arrays.copyOf(src, n), n) }"
      else
        s"{ val src = as.asInstanceOf[Array[Object]]; val n = src.length; buildSorted${k.name}(java.util.Arrays.copyOf(src, n), n) }"
    }

    // from(IterableOnce): generic build — drain into an ArrayBuffer of raw kind values, then copy into a raw
    // kind array and build. (fromFArray routes through here from the surface via `fa.iterator`, so there is no
    // separate FArray bridge impl — the opaque FArray type can only be widened to FBase in FArray.scala's own
    // scope, not here; a direct unboxed leaf read off the FArray's ${K}Arr is a NEXT-STEP zero-copy bridge.)
    val fromV = dispatchA { k =>
      val arrT = if k.name == "Ref" then "Object" else k.arr
      s"{ val b = scala.collection.mutable.ArrayBuffer.empty[$arrT]; it.iterator.foreach(a => b += ${storeRaw(k, unwrapElem(k, "a"))}); val n = b.length; val raw = new Array[$arrT](n); var i = 0; while (i < n) { raw(i) = b(i); i += 1 }; buildSorted${k.name}(raw, n) }"
    }

    // dispatch on the ELEMENT kind B (mirrors FArray's appendImpl[A, B], which dispatches `${K}Repr[B]`).
    def dispatchB(body: Kind => String): String =
      "summonFrom {\n" + opKinds.map(k => s"      case r: ${k.name}Repr[B] => ${body(k)}").mkString("\n") + "\n    }"

    // incl: Union(this, One(elem)) — O(1), shares the base; returns `this` (a sound alias by immutability,
    // §3.2) when a cheap contains proves the element already present. Shape MIRRORS FArray's appendImpl[A, B]
    // exactly (two type params, dispatch on the element kind B, build a node node from the `xs: SBase` param) —
    // the structure that lets the `SBase`-result up-coerce to the opaque `FSet[B]` at foreign call sites.
    val inclV = dispatchB(k =>
      s"{ val e = ${unwrapElemB(k, "elem")}; if (containsLeaf${k.name}(xs, e)) xs else new SUnion(xs, new S${k.name}One(e)) }"
    )
    // excl: Diff(this, One(elem)) — O(1); returns `this` if absent.
    val exclV = dispatchB(k =>
      s"{ val e = ${unwrapElemB(k, "elem")}; if (!containsLeaf${k.name}(xs, e)) xs else new SDiff(xs, new S${k.name}One(e)) }"
    )

    // size: M1 — SMaterialized leaves report the O(1) field; a lazy SView node materializes (folds the algebra
    // into a sorted leaf) then reads the field. For the green slice the only multi-element materialized shape
    // is ${K}Sorted, and lazy nodes are folded by sizeOfView (a structural element-count via the membership
    // walk). To keep M1 small and correct, size walks the structure counting DISTINCT elements through a
    // boxed HashSet — correct, not yet fast; the unboxed sorted-merge materialize is the NEXT-STEP that makes
    // it leaf-speed. isEmpty is structural and cheap (no materialize).
    val sizeV = dispatchA(k => s"sizeImpl${k.name}(xs)")

    // structural isEmpty: SEmpty is empty; a One/Sorted is non-empty; Union empty iff both empty; Inter/Diff
    // need a probe but M1 just asks size==0 via the (boxed) element collection — cheap enough for the slice.
    val isEmptyV = dispatchA(k => s"isEmptyImpl${k.name}(xs)")

    // iterator: materialize to a leaf (prim: unboxed sorted merge → ordered; Ref: boxed collectElems set) and
    // walk its elements. The Array[prim].iterator boxes on `next`; the cast to Iterator[A] is sound (A is the
    // concrete element kind at the call site). Ordered for prims; unordered for Ref (no Ordering threaded yet).
    val iteratorV = dispatchA(k => s"asArr${k.name}(materialize${k.name}(xs)).iterator.asInstanceOf[Iterator[A]]")

    // value equals / hashCode (order- and shape-independent) — but ONLY on a MATERIALIZED set: a still-lazy
    // SView throws (call .materialize first). This is the FSetMaterialized-only capability of the design,
    // enforced at runtime until the Option C type lattice makes it a compile error.
    val sameElementsV = dispatchA(k =>
      if k.isPrim then s"setEq${k.name}(a, b)" else "setEqRef(a, b)"
    )
    val hashV = dispatchA(k => s"setHash${k.name}(xs)")

    // subsetOf(b): every element of `a` is in `b`. Materializes `a` (the receiver) and streams it against
    // `b`'s membership (contains distributes over a lazy `b`, no materialize). A query op (no materialized guard).
    val subsetOfV = dispatchA(k =>
      s"{ val arr = asArr${k.name}(materialize${k.name}(a)); var i = 0; var ok = true; while (i < arr.length && ok) { if (!containsLeaf${k.name}(b, arr(i))) ok = false; i += 1 }; ok }"
    )

    // min / max — O(1) on the materialized sorted leaf (prim natural order). Ref needs an Ordering (FSortedSet),
    // so it throws for now. Empty throws NoSuchElementException. Materialized-ordered (the FSortedSet capability).
    val minV = dispatchA(k =>
      if k.isPrim then s"""{ val arr = asArr${k.name}(materialize${k.name}(xs)); if (arr.length == 0) throw new NoSuchElementException("min of empty FSet") else (arr(0)).asInstanceOf[A] }"""
      else """throw new UnsupportedOperationException("min requires an ordered set (Ordering / FSortedSet)")"""
    )
    val maxV = dispatchA(k =>
      if k.isPrim then s"""{ val arr = asArr${k.name}(materialize${k.name}(xs)); if (arr.length == 0) throw new NoSuchElementException("max of empty FSet") else (arr(arr.length - 1)).asInstanceOf[A] }"""
      else """throw new UnsupportedOperationException("max requires an ordered set (Ordering / FSortedSet)")"""
    )

    // ---- size / isEmpty helpers (non-inline; M1 boxed-HashSet element walk; NEXT-STEP: unboxed merge) ----
    val sizeHelpers = {
      val ee = new Emit("  ")
      opKinds.foreach { k =>
        val K = k.name
        // A MATERIALIZED leaf knows its exact distinct count in O(1) (and with the leaf's own — correct —
        // equality, e.g. Double -0.0≠+0.0 via Double.compare); only a lazy algebra node needs the walk. The
        // boxed-HashSet walk is an M1 scaffold (wrong -0.0 semantics + boxing) — the unboxed merge replaces it.
        ee.open(s"def sizeImpl$K(node: SBase): Int = node match")
        ee.line("case m: SMaterialized => m.size()")
        // a lazy algebra node: materialize (unboxed sorted/hash merge) to a leaf, read its O(1) size.
        ee.line(s"case _ => materialize$K(node).asInstanceOf[SMaterialized].size()")
        ee.close()
        ee.open(s"def isEmptyImpl$K(node: SBase): Boolean = node match")
        ee.line("case _: SEmpty => true")
        ee.line("case u: SUnion => isEmptyImpl" + K + "(u.left) && isEmptyImpl" + K + "(u.right)")
        // Inter/Diff and leaves: fall back to the size walk (M1). A One/Sorted is never empty.
        ee.line(s"case o: S${K}One => false")
        ee.line(s"case s: S${K}Sorted => s.arr.length == 0")
        ee.line(s"case _ => sizeImpl$K(node) == 0")
        ee.close()
        // collect distinct elements (boxed) — M1 correctness scaffold for size; the algebra is walked with
        // set semantics (Diff removes, Inter intersects) via temporary collections.
        ee.open(s"def collectElems$K(node: SBase, acc: scala.collection.mutable.HashSet[${k.arr}]): Unit = node match")
        ee.line("case _: SEmpty => ()")
        if k.name == "Ref" then ee.line(s"case o: S${K}One => acc += o.elem.asInstanceOf[${k.arr}]")
        else ee.line(s"case o: S${K}One => acc += o.elem")
        ee.open(s"case s: S${K}Sorted =>")
        ee.line("val a = s.arr")
        ee.line("val n = a.length")
        ee.line("var i = 0")
        ee.open("while (i < n)")
        if k.name == "Ref" then ee.line(s"acc += a(i).asInstanceOf[${k.arr}]")
        else ee.line("acc += a(i)")
        ee.line("i += 1")
        ee.close()
        ee.closeOpen(s"case h: S${K}Hash =>")
        ee.line("val ha = h.arr")
        ee.line("val hn = ha.length")
        ee.line("var hi = 0")
        ee.open("while (hi < hn)")
        if k.name == "Ref" then ee.line(s"acc += ha(hi).asInstanceOf[${k.arr}]")
        else ee.line("acc += ha(hi)")
        ee.line("hi += 1")
        ee.close()
        ee.closeOpen("case u: SUnion =>")
        ee.line(s"collectElems$K(u.left, acc)")
        ee.line(s"collectElems$K(u.right, acc)")
        ee.closeOpen("case n: SInter =>")
        ee.line(s"val l = scala.collection.mutable.HashSet.empty[${k.arr}]")
        ee.line(s"collectElems$K(n.left, l)")
        ee.line(s"val rr = scala.collection.mutable.HashSet.empty[${k.arr}]")
        ee.line(s"collectElems$K(n.right, rr)")
        ee.line("acc ++= l.intersect(rr)")
        ee.closeOpen("case d: SDiff =>")
        ee.line(s"val l = scala.collection.mutable.HashSet.empty[${k.arr}]")
        ee.line(s"collectElems$K(d.left, l)")
        ee.line(s"val rr = scala.collection.mutable.HashSet.empty[${k.arr}]")
        ee.line(s"collectElems$K(d.right, rr)")
        ee.line("acc ++= (l diff rr)")
        ee.closeOpen("case x: SXor =>")
        ee.line(s"val l = scala.collection.mutable.HashSet.empty[${k.arr}]")
        ee.line(s"collectElems$K(x.left, l)")
        ee.line(s"val rr = scala.collection.mutable.HashSet.empty[${k.arr}]")
        ee.line(s"collectElems$K(x.right, rr)")
        ee.line("acc ++= (l diff rr); acc ++= (rr diff l)")
        ee.closeOpen("case _ =>")
        ee.line("()")
        ee.close()
        ee.close() // node match
      }
      ee.result
    }

    // ---- traversal leaf-walkers (§1.6.4/1.6.7): the inline surface realizes the user lambda into a kind-
    // specialized SAM (SetTraversers.${K}Pred/Consumer, prim arg unboxed) and CALLS these shared non-inline
    // walkers. foreach/forall/exists auto-materialize a lazy node (each element once); short-circuit where they can.
    val traversalHelpers = {
      val ee = new Emit("  ")
      opKinds.foreach { k =>
        val K = k.name
        // every kind enumerates the SAME way: materialize to a leaf (prim → unboxed sorted merge; Ref →
        // cached-hash sort-merge) and walk its asArr — unboxed, each element once (hash-ordered for Ref).
        def setup(): Unit = ee.line(s"val arr = asArr$K(materialize$K(node))")
        ee.open(s"def foreachLeaf$K(node: SBase, f: SetTraversers.${K}Consumer): Unit =")
        setup(); ee.line("var i = 0")
        ee.open("while (i < arr.length)"); ee.line("f.accept(arr(i))"); ee.line("i += 1"); ee.close()
        ee.close()
        ee.open(s"def forallLeaf$K(node: SBase, p: SetTraversers.${K}Pred): Boolean =")
        setup(); ee.line("var i = 0; var ok = true")
        ee.open("while (i < arr.length && ok)"); ee.line("if (!p.test(arr(i))) ok = false"); ee.line("i += 1"); ee.close()
        ee.line("ok")
        ee.close()
        ee.open(s"def existsLeaf$K(node: SBase, p: SetTraversers.${K}Pred): Boolean =")
        setup(); ee.line("var i = 0; var found = false")
        ee.open("while (i < arr.length && !found)"); ee.line("if (p.test(arr(i))) found = true"); ee.line("i += 1"); ee.close()
        ee.line("found")
        ee.close()
        // count: number of elements satisfying p.
        ee.open(s"def countLeaf$K(node: SBase, p: SetTraversers.${K}Pred): Int =")
        setup(); ee.line("var i = 0; var c = 0")
        ee.open("while (i < arr.length)"); ee.line("if (p.test(arr(i))) c += 1"); ee.line("i += 1"); ee.close()
        ee.line("c")
        ee.close()
        // filter: keep elements satisfying p, build the same-kind leaf. Prim: the kept slice of the sorted arr
        // stays sorted+distinct → wrap directly (no re-sort). Ref: the kept subset is still distinct → re-hash+
        // sort+wrap via buildSortedRef (its dedup is a no-op on an already-distinct input).
        ee.open(s"def filterLeaf$K(node: SBase, p: SetTraversers.${K}Pred): SBase =")
        setup()
        if k.isPrim then {
          val P = k.arr
          ee.line(s"val out = new Array[$P](arr.length)")
          ee.line("var i = 0; var w = 0")
          ee.open("while (i < arr.length)")
          ee.line("val v = arr(i)")
          ee.line("if (p.test(v)) { out(w) = v; w += 1 }")
          ee.line("i += 1")
          ee.close()
          ee.line("if (w == 0) SEmpty.INSTANCE")
          ee.line(s"else if (w == 1) new S${K}One(out(0))")
          ee.line(s"else { val trimmed = java.util.Arrays.copyOf(out, w); if (w <= 16) new S${K}Sorted(trimmed) else buildHash$K(trimmed) }")
        } else {
          ee.line("val out = new Array[Object](arr.length)")
          ee.line("var i = 0; var w = 0")
          ee.open("while (i < arr.length)")
          ee.line("val v = arr(i)")
          ee.line("if (p.test(v)) { out(w) = v; w += 1 }")
          ee.line("i += 1")
          ee.close()
          ee.line(s"buildSorted$K(out, w)")
        }
        ee.close()
      }
      ee.result
    }
    val foreachV = dispatchA(k => s"foreachLeaf${k.name}(xs, (v) => f(${wrapElem(k, "v")}))")
    val forallV = dispatchA(k => s"forallLeaf${k.name}(xs, (v) => p(${wrapElem(k, "v")}))")
    val existsV = dispatchA(k => s"existsLeaf${k.name}(xs, (v) => p(${wrapElem(k, "v")}))")
    val countV = dispatchA(k => s"countLeaf${k.name}(xs, (v) => p(${wrapElem(k, "v")}))")
    val filterV = dispatchA(k => s"filterLeaf${k.name}(xs, (v) => p(${wrapElem(k, "v")}))")

    // map[B]: the N×N read-kind A × write-kind B matrix. Read each element unboxed (kind A), apply the inline
    // user `f` to get a B, store the raw B (kind B), then buildSorted${B} DEDUPS (map can collapse distinct A→B).
    // Both A and B are concrete at the call site, so summonFrom picks one (A,B) path — no megamorphism, no box.
    val mapV = {
      def srcOf(ka: Kind): String = s"asArr${ka.name}(materialize${ka.name}(xs))"
      def readA(ka: Kind): String = if ka.isPrim then "ra.wrap(src(i))" else "src(i).asInstanceOf[A]"
      def storeB(kb: Kind, b: String): String = if kb.isPrim then s"rb.unwrap($b)" else s"($b).asInstanceOf[Object]"
      "summonFrom {\n" + opKinds.map { ka =>
        s"      case ra: ${ka.name}Repr[A] => summonFrom {\n" +
          opKinds.map { kb =>
            val body = s"{ val src = ${srcOf(ka)}; val n = src.length; val out = new Array[${kb.arr}](n); var i = 0; while (i < n) { out(i) = ${storeB(kb, "f(" + readA(ka) + ")")}; i += 1 }; buildSorted${kb.name}(out, n) }"
            s"        case rb: ${kb.name}Repr[B] => $body"
          }.mkString("\n") +
          "\n      }"
      }.mkString("\n") + "\n    }"
    }

    // ---- value equals / hashCode helpers — MATERIALIZED-ONLY (throw on a lazy SView) ----
    val valueHelpers = {
      val ee = new Emit("  ")
      val throwMsg = "throw new UnsupportedOperationException(\"FSet equals/hashCode require a materialized set — call .materialize first\")"
      opKinds.foreach { k =>
        val K = k.name
        // order-independent hashCode: a commutative sum of per-element avalanche mixes + the size. Identical
        // across leaf shapes (Sorted vs Hash) because it folds the canonical element set.
        ee.open(s"def setHash$K(node: SBase): Int = node match")
        if k.isPrim then {
          ee.open("case _: SMaterialized =>")
          ee.line(s"val arr = asArr$K(node)")
          ee.line("var h = 0; var i = 0")
          ee.open("while (i < arr.length)")
          ee.line(s"h += ${slotHash(k, "arr(i)")}")
          ee.line("i += 1")
          ee.close()
          ee.line("h + arr.length")
          ee.close()
        } else {
          // unboxed: fold the leaf's parallel hash cache directly (commutative → order/shape-independent).
          ee.open("case _: SMaterialized =>")
          ee.line("val hs = hashesOfRef(node)")
          ee.line("var h = 0; var i = 0")
          ee.open("while (i < hs.length)")
          ee.line("h += mixInt(hs(i))")
          ee.line("i += 1")
          ee.close()
          ee.line("h + hs.length")
          ee.close()
        }
        ee.line(s"case _ => $throwMsg")
        ee.close()
        // value equality: both must be materialized. Prim = Arrays.equals of the two sorted leaf arrays
        // (asArr is sorted for prims); Ref = compare the two element HashSets.
        if k.isPrim then {
          ee.open(s"def setEq$K(a: SBase, b: SBase): Boolean = (a, b) match")
          ee.line(s"case (_: SMaterialized, _: SMaterialized) => java.util.Arrays.equals(asArr$K(a), asArr$K(b))")
          ee.line(s"case _ => $throwMsg")
          ee.close()
        } else {
          ee.open("def setEqRef(a: SBase, b: SBase): Boolean = (a, b) match")
          ee.open("case (_: SMaterialized, _: SMaterialized) =>")
          ee.line("val sa = scala.collection.mutable.HashSet.empty[Object]; collectElemsRef(a, sa)")
          ee.line("val sb = scala.collection.mutable.HashSet.empty[Object]; collectElemsRef(b, sb)")
          ee.line("sa == sb")
          ee.close()
          ee.line(s"case _ => $throwMsg")
          ee.close()
        }
      }
      ee.result
    }

    s"""package farray
       |
       |// GENERATED by GenSets — do not edit.
       |// Per-kind-specialized FSet ops. REUSES farray.Repr (generated by GenCores); no second Repr is emitted.
       |// The "small shared NON-inline leaf method" discipline (§1.6.4): the membership walk + the build/sort +
       |// the size walk live here, ONCE per kind; the inline surface (FSet.scala) only dispatches the kind via
       |// summonFrom on ${"$"}{K}Repr[A] and calls in. Primitives stay unboxed (Repr.unwrap, raw prim arrays).
       |import scala.compiletime.summonFrom
       |
       |object FSetOps {
       |
       |${indent1(containsLeaves)}
       |${indent1(buildLeaves)}
       |${indent1(hashHelpers)}
       |${indent1(mergeCore)}
       |${indent1(sizeHelpers)}
       |${indent1(traversalHelpers)}
       |${indent1(valueHelpers)}
       |
       |  inline def emptyImpl[A]: SBase = $emptyV
       |  inline def fromValues1[A](a: A): SBase = $fromValues1
       |  inline def fromValues2[A](p1: A, p2: A): SBase = ${fromValuesN(2)}
       |  inline def fromValues3[A](p1: A, p2: A, p3: A): SBase = ${fromValuesN(3)}
       |  inline def fromArrayImpl[A](as: Array[A]): SBase = $fromArrayV
       |  inline def fromImpl[A](it: IterableOnce[A]): SBase = $fromV
       |  inline def containsImpl[A](xs: SBase, elem: A): Boolean = $containsV
       |  inline def sizeImpl[A](xs: SBase): Int = $sizeV
       |  inline def isEmptyImpl[A](xs: SBase): Boolean = $isEmptyV
       |  inline def iteratorImpl[A](xs: SBase): Iterator[A] = $iteratorV
       |  inline def materializeImpl[A](xs: SBase): SBase = ${dispatchA(k => s"materialize${k.name}(xs)")}
       |  inline def sameElementsImpl[A](a: SBase, b: SBase): Boolean = $sameElementsV
       |  inline def hashCodeImpl[A](xs: SBase): Int = $hashV
       |  inline def subsetOfImpl[A](a: SBase, b: SBase): Boolean = $subsetOfV
       |  inline def minImpl[A](xs: SBase): A = $minV
       |  inline def maxImpl[A](xs: SBase): A = $maxV
       |  inline def foreachImpl[A](xs: SBase)(inline f: A => Unit): Unit = $foreachV
       |  inline def forallImpl[A](xs: SBase)(inline p: A => Boolean): Boolean = $forallV
       |  inline def existsImpl[A](xs: SBase)(inline p: A => Boolean): Boolean = $existsV
       |  inline def countImpl[A](xs: SBase)(inline p: A => Boolean): Int = $countV
       |  inline def filterImpl[A](xs: SBase)(inline p: A => Boolean): SBase = $filterV
       |  inline def mapImpl[A, B](xs: SBase)(inline f: A => B): SBase = $mapV
       |  inline def inclImpl[A, B](xs: SBase, elem: B): SBase = $inclV
       |  inline def exclImpl[A, B](xs: SBase, elem: B): SBase = $exclV
       |}
       |""".stripMargin
  }
}
