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
      Files.writeString(dir.resolve("SIntBitmap.java"), bitmapNode)
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
    val permits = ("SEmpty" :: opKinds.map(k => s"S${k.name}One") ::: opKinds.map(k => s"S${k.name}Sorted") ::: opKinds.map(k => s"S${k.name}Hash") ::: List("SIntBitmap")).mkString(", ")
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
         |// GENERATED by GenSets — do not edit. Frozen open-addressing reference set. `slots[slot]` packs the
         |// F14/abseil-style: `ctrl` is a dense 1-byte-per-slot table (0 = empty; else 0x80 | 7-bit hash
         |// fingerprint) that rejects a miss with one cache-resident byte; `keys` holds the element ref INLINE at
         |// the slot, so a hit dereferences it directly — no dependent hashes[p]/arr[p] random load. `arr`/`hashes`
         |// hold the dense element column (insertion order unless built sorted); the HASH-SORTED view the merge
         |// core needs is memoized LAZILY in `sortedArr`/`sortedHashes` (build-then-query never pays the sort).
         |// Benign race: the sorted view is deterministic, and `sortedHashes` is written before `sortedArr`, so a
         |// reader that sees sortedArr non-null also sees its pair. O(1) contains.
         |public final class S${k.name}Hash extends SMaterialized {
         |    public final Object[] arr;
         |    public final int[] hashes;
         |    public final byte[] ctrl;
         |    public final Object[] keys;
         |    public volatile Object[] sortedArr;
         |    public volatile int[] sortedHashes;
         |    public S${k.name}Hash(Object[] arr, int[] hashes, byte[] ctrl, Object[] keys, boolean sorted) {
         |        this.arr = arr; this.hashes = hashes; this.ctrl = ctrl; this.keys = keys;
         |        if (sorted) { this.sortedHashes = hashes; this.sortedArr = arr; }
         |    }
         |    @Override public int size() { return arr.length; }
         |}
         |""".stripMargin

  /** the dense-Int bitmap leaf (Step 2 / Roaring "bitmap container"): a `long[]` covering [base, base+64·words),
    * one bit per value. contains is an O(1) bit-test (no hash, no probe walk) — the answer to BitSet on dense
    * non-negative-span Int. `card` is the precomputed popcount (the exact size). Chosen by a density router
    * (span ≤ K·n and span ≤ cap); sparse/wide stays on Sorted/Hash. */
  private def bitmapNode: String =
    s"""package farray;
       |
       |// GENERATED by GenSets — do not edit. Dense Int bitmap: words[(x-base)>>>6] bit (x-base). O(1) contains.
       |public final class SIntBitmap extends SMaterialized {
       |    public final int base;     // value of bit 0 of words[0]
       |    public final int card;     // popcount == size
       |    public final long[] words;
       |    public SIntBitmap(int base, int card, long[] words) { this.base = base; this.card = card; this.words = words; }
       |    @Override public int size() { return card; }
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
      // Union/Xor: at most |l| + |r| (summed in long — two huge hints must clamp, not overflow negative).
      // Inter/Diff: at most |l| (Inter ⊆ l, Diff ⊆ l).
      case "SUnion" | "SXor" => "(int) Math.min((long) left.sizeHint() + (long) right.sizeHint(), (long) Integer.MAX_VALUE)"
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
        // ITERATIVE left-spine trampoline (§2.4): loop the unbounded LEFT spine and recurse only on the
        // (shallow) right children — so a deep +/++/-/^ chain (which is LEFT-deep) cannot StackOverflow.
        // `flip` accumulates the pending parity from SXor/SComplement frames (result = raw ^ flip), which is
        // what lets Xor and Complement ride the SAME loop instead of recursing on `left`/`inner`. Union
        // short-circuits on first hit (→ !flip), Inter/Diff on first miss (→ flip). On the hot leaf-only path
        // flip is provably false and folds away.
        ee.open(s"def containsLeaf$K(node0: SBase, e: ${k.arr}): Boolean =")
        ee.line("var node: SBase = node0")
        ee.line("var flip = false")
        ee.open("while (true)")
        ee.open("node match")
        ee.line("case _: SEmpty => return flip")
        ee.line(s"case o: S${K}One => return ${isEq(k, "o.elem", "e")} ^ flip")
        ee.open(s"case s: S${K}Sorted =>")
        ee.line("val a = s.arr")
        if k.isPrim then {
          // a Sorted leaf is ALWAYS ≤16 (the wrap threshold), so a LINEAR scan beats binary search: the loop
          // branch is perfectly predictable and only the equality branch can mispredict (once, on a hit),
          // whereas binary search's ~log n comparisons are all data-dependent → a mispredict each.
          ee.line("var i = 0")
          ee.open("while (i < a.length)")
          ee.line("val v = a(i)")
          ee.line(s"if (${isEq(k, "v", "e")}) return !flip")
          ee.line(s"if (${isLt(k, "e", "v")}) return flip") // sorted ascending → passed e → miss, stop early
          ee.line("i += 1")
          ee.close()
          ee.line("return flip")
        } else {
          // Ref Sorted leaf is ≤16 (the wrap threshold) and hash-sorted; a LINEAR scan over the cached hashes is
          // branch-predictable (vs binary search's data-dependent comparisons) and only calls .equals on a hash hit.
          ee.line("val hs = s.hashes")
          ee.line("val hc = e.hashCode()")
          ee.line("var i = 0")
          ee.open("while (i < a.length)")
          ee.line("val hm = hs(i)")
          ee.line("if (hm == hc && a(i).equals(e)) return !flip")
          ee.line("if (hm > hc) return flip") // hashes sorted ascending → passed hc → miss, stop early
          ee.line("i += 1")
          ee.close()
          ee.line("return flip")
        }
        ee.close() // case Sorted
        ee.line(s"case u: SUnion => if (containsLeaf$K(u.right, e)) return !flip else node = u.left")
        ee.line(s"case n: SInter => if (!containsLeaf$K(n.right, e)) return flip else node = n.left")
        ee.line(s"case d: SDiff => if (containsLeaf$K(d.right, e)) return flip else node = d.left")
        // Xor: a LEAF left child answers directly (the common single-^ case — MEASURED ~20% faster than
        // looping); a lazy left child folds the right into the parity and LOOPS — a ^-built chain is left-deep.
        ee.line(s"case x: SXor => x.left match { case _: SView => { flip ^= containsLeaf$K(x.right, e); node = x.left }; case l => return (containsLeaf$K(l, e) ^ containsLeaf$K(x.right, e)) ^ flip }")
        // frozen open-addressing leaf — O(1) probe (§2.3): mix → slot → linear-probe index → position → compare.
        ee.open(s"case h: S${K}Hash =>")
        ee.line("val arr = h.arr")
        if k.isPrim then {
          ee.line("val idx = h.index")
          ee.line(s"var slot = (${slotHash(k, "e")}) & (idx.length - 1)")
          ee.line("var res = false; var go = true")
          ee.open("while (go)")
          ee.line("val p = idx(slot)")
          ee.line("if (p == -1) go = false")
          ee.line(s"else if (${isEq(k, "arr(p)", "e")}) { res = true; go = false }")
          ee.line("else slot = (slot + 1) & (idx.length - 1)")
          ee.close()
          ee.line("return res ^ flip")
        } else {
          // F14 probe: a 1-byte ctrl fingerprint rejects a miss from a cache-resident byte; on a fingerprint
          // match the inline `keys[slot]` ref is dereferenced directly (no dependent arr[pos] random load).
          ee.line("val ctrl = h.ctrl")
          ee.line("val keys = h.keys")
          ee.line("val hc = e.hashCode()")
          // mixRef (cheap) only in the mid BAND where it both matters and distributes well: its high-bit fold is
          // tuned for ~16-bit masks, so it degrades below ~2^10 (tiny) and above ~2^15 (large) — mixInt there.
          ee.line("val cl = ctrl.length")
          ee.line("val m = if (cl >= 1024 && cl <= 32768) mixRef(hc) else mixInt(hc)")
          ee.line("val fp = (((m >>> 24) & 0x7f) | 0x80).toByte")
          ee.line("var slot = m & (ctrl.length - 1)")
          ee.line("var res = false; var go = true")
          ee.open("while (go)")
          ee.line("val c = ctrl(slot)")
          ee.line("if (c == 0) go = false")
          ee.line("else if (c == fp && keys(slot).equals(e)) { res = true; go = false }")
          ee.line("else slot = (slot + 1) & (ctrl.length - 1)")
          ee.close()
          ee.line("return res ^ flip")
        }
        ee.close()
        // predicate leaves (§2.5): O(1)-by-computation, distribute through the algebra, never materialize.
        // (a single unsigned long-compare bounds test was MEASURED SLOWER than these two int compares — the
        // branches predict perfectly; the long ops don't come free. Keep the two-compare form.)
        if k.name == "Int" then ee.line("case b: SIntBitmap => { val i = e - b.base; return (i >= 0 && (i >>> 6) < b.words.length && (b.words(i >>> 6) >>> i & 1L) != 0L) ^ flip }")
        if k.name == "Int" then ee.line("case r: SIntRange => return (r.lo <= e && e <= r.hi) ^ flip")
        if k.name == "Long" then ee.line("case r: SLongRange => return (r.lo <= e && e <= r.hi) ^ flip")
        // Complement folds into the parity and LOOPS on inner — a nested-complement chain cannot StackOverflow.
        ee.line("case c: SComplement => { flip = !flip; node = c.inner }")
        ee.line("case _ => return flip")
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
        // `owned = false` ⇒ the caller's array must not be mutated: the prim SORT branch copies before sorting
        // (the dense-Int bitmap branch and the whole Ref branch only READ raw, so they never copy — this is what
        // lets fromArray pass the user's array straight through with no defensive copy).
        ee.open(s"def buildSorted$K(raw: Array[$arrT], len: Int, owned: Boolean): SBase =")
        ee.line("if (len == 0) return SEmpty.INSTANCE")
        if k.name == "Int" then {
          // dense-Int FAST build: one min/max pass, then set bits DIRECTLY into a bitmap (O(n), no O(n log n)
          // sort — and the bits dedup for free). Sparse/wide falls through to the sort path → Sorted/Hash.
          ee.line("var mn = raw(0); var mx = raw(0); var di = 1")
          ee.line("while (di < len) { val v = raw(di); if (v < mn) mn = v; if (v > mx) mx = v; di += 1 }")
          ee.line("val dspan = mx.toLong - mn.toLong + 1L")
          // even SMALL dense sets become bitmaps (O(1) bit-test) — a binary-search Sorted leaf at ≤16 elements
          // loses contains to every O(1) competitor (the size-16 contains hole). len==1 falls through → SIntOne.
          ee.open("if (len >= 2 && dspan <= 64L * len && dspan <= (1L << 20))")
          ee.line("val dbase = (mn >> 6) << 6")
          ee.line("val dnw = ((mx - dbase) >>> 6) + 1")
          ee.line("val dwords = new Array[Long](dnw)")
          ee.line("var bi = 0; while (bi < len) { val bb = raw(bi) - dbase; dwords(bb >>> 6) |= (1L << bb); bi += 1 }")
          ee.line("var dcard = 0; var dwi = 0; while (dwi < dnw) { dcard += java.lang.Long.bitCount(dwords(dwi)); dwi += 1 }")
          ee.line("return new SIntBitmap(dbase, dcard, dwords)")
          ee.close()
        }
        if k.isPrim then {
          // sort the first `len` slots (copy first if the caller still owns the array), then unique-compact.
          ee.line("val a = if (owned) raw else java.util.Arrays.copyOf(raw, len)")
          ee.line("java.util.Arrays.sort(a, 0, len)")
          ee.line("var w = 1")
          ee.line("var i = 1")
          ee.open("while (i < len)")
          ee.open(s"if (${notEq(k, "a(i)", "a(i - 1)")})")
          ee.line("a(w) = a(i)")
          ee.line("w += 1")
          ee.close()
          ee.line("i += 1")
          ee.close()
          ee.line(s"if (w == 1) new S${K}One(a(0))")
          // route the sorted distinct array through wrap (Int → dense bitmap / Sorted / Hash; Long/Double → Sorted/Hash).
          ee.line(s"else wrap$K(java.util.Arrays.copyOf(a, w))")
        } else {
          // Ref: ONE PASS — the F14 ctrl+keys table doubles as the dedup structure AND the final probe index;
          // arr/hashes collect the distinct elements in insertion order. The hash-sort the merge core needs is
          // DEFERRED to the leaf's memoized sorted view (ensureSortedRef), so build-then-query never pays it.
          // Tiny results (≤4) eager-sort into SRefSorted (its contains scan needs sorted hashes); a heavy-dup
          // input (final load ≤ cap/8) rebuilds a tight table instead of keeping the oversized one.
          ee.line("var cap = 8")
          ee.line("while (cap < len * 2) cap <<= 1")
          ee.line("val ctrl = new Array[Byte](cap)")
          ee.line("val keys = new Array[Object](cap)")
          ee.line("val arr = new Array[Object](len)")
          ee.line("val hashes = new Array[Int](len)")
          ee.line("val useRef = cap >= 1024 && cap <= 32768")
          ee.line("var w = 0")
          ee.line("var i = 0")
          ee.open("while (i < len)")
          ee.line("val v = raw(i)")
          ee.line("val hc = v.hashCode()")
          ee.line("val m = if (useRef) mixRef(hc) else mixInt(hc)")
          ee.line("val fp = (((m >>> 24) & 0x7f) | 0x80).toByte")
          ee.line("var slot = m & (cap - 1)")
          ee.line("var dup = false; var go = true")
          ee.open("while (go)")
          ee.line("val c = ctrl(slot)")
          ee.line("if (c == 0) go = false")
          ee.line("else if (c == fp && keys(slot).equals(v)) { dup = true; go = false }")
          ee.line("else slot = (slot + 1) & (cap - 1)")
          ee.close()
          ee.open("if (!dup)")
          ee.line("ctrl(slot) = fp; keys(slot) = v; arr(w) = v; hashes(w) = hc; w += 1")
          ee.close()
          ee.line("i += 1")
          ee.close()
          ee.line("if (w == 0) SEmpty.INSTANCE")
          ee.line(s"else if (w == 1) new S${K}One(arr(0))")
          ee.open("else if (w <= 4)")
          ee.line("val ta = java.util.Arrays.copyOf(arr, w)")
          ee.line("val th = java.util.Arrays.copyOf(hashes, w)")
          ee.line(s"sortByHash$K(ta, th, w)")
          ee.line(s"new S${K}Sorted(ta, th)")
          ee.close()
          ee.line(s"else if (w * 8 <= cap) wrap${K}Raw(java.util.Arrays.copyOf(arr, w), java.util.Arrays.copyOf(hashes, w))")
          ee.open("else")
          ee.line("val ta = if (w == len) arr else java.util.Arrays.copyOf(arr, w)")
          ee.line("val th = if (w == len) hashes else java.util.Arrays.copyOf(hashes, w)")
          ee.line(s"new S${K}Hash(ta, th, ctrl, keys, false)")
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
      // ref-hash slot mix: the input is already String.hashCode (a decent polynomial hash), so a single
      // Fibonacci multiply + high-bit fold suffices (~3 cycles vs mixInt's ~9) — the murmur avalanche is redundant.
      ee.line("def mixRef(h0: Int): Int = { val x = h0 * 0x9e3779b1; x ^ (x >>> 16) }")
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
          // dead since Step 1 (Ref leaves are built via buildSortedRef/wrapRef) — route through wrapRef to stay
          // consistent with the hash-sorted, packed-slots representation rather than keep a stale builder.
          ee.open(s"def buildHash$K(arr: Array[Object]): SBase =")
          ee.line("val n = arr.length")
          ee.line("val hashes = new Array[Int](n)")
          ee.line("var i = 0")
          ee.line("while (i < n) { hashes(i) = arr(i).hashCode(); i += 1 }")
          ee.line("sortByHashRef(arr, hashes, n)")
          ee.line("wrapRef(arr, hashes)")
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
        if k.name == "Int" then ee.line("case b: SIntBitmap => bitmapToArr(b)")
        ee.line(s"case _ => new Array[$P](0)")
        ee.close()
        // wrap a sorted+distinct array into the canonical leaf. Int routes through the density-aware router
        // (dense → bitmap; else ≤16 Sorted / Hash); Long/Double mirror buildSorted's Sorted-or-Hash threshold so
        // a materialized merge is closed under the same leaf-kind choice as a fresh build.
        ee.open(s"def wrap$K(arr: Array[$P]): SBase =")
        if k.name == "Int" then ee.line("routeInt(arr)")
        else {
          ee.line("if (arr.length == 0) SEmpty.INSTANCE")
          ee.line(s"else if (arr.length == 1) new S${K}One(arr(0))")
          ee.line(s"else if (arr.length <= 16) new S${K}Sorted(arr) else buildHash$K(arr)")
        }
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
        // unionTwo: union of two already-materialized leaves (Int keeps the word-parallel bitmap fast-path).
        ee.open(s"def unionTwo$K(a: SBase, b: SBase): SBase =")
        if k.name == "Int" then {
          ee.open("(a, b) match")
          ee.line("case (lb: SIntBitmap, rb: SIntBitmap) => bitmapMergeInt(lb, rb, 0)")
          ee.line("case _ => wrapInt(mergeUnionInt(asArrInt(a), asArrInt(b)))")
          ee.close()
        } else ee.line(s"wrap$K(mergeUnion$K(asArr$K(a), asArr$K(b)))")
        ee.close()
        // unionEager: the smart `++` constructor — eagerly merge when the merge is CHEAP (both bitmaps with a
        // modest merged span = a word-OR; or both small ≤16 leaves), so e.g. a chain of dense bitmaps collapses
        // to ONE bitmap (contains = 1 bit-test). Stay LAZY for the O(n) cases (large Sorted/Hash) — deferring
        // those is the win on refs/large sets. Mixed operands → lazy.
        ee.open(s"def unionEager$K(a: SBase, b: SBase): SBase =")
        ee.open("(a, b) match")
        if k.name == "Int" then {
          ee.open("case (la: SIntBitmap, lb: SIntBitmap) =>")
          ee.line("val base = if (la.base < lb.base) la.base else lb.base")
          ee.line("val hi = scala.math.max(la.base.toLong + la.words.length.toLong * 64L, lb.base.toLong + lb.words.length.toLong * 64L)")
          ee.line("if (hi - base.toLong <= (1L << 20)) bitmapMergeInt(la, lb, 0) else new SUnion(a, b)")
          ee.close()
        }
        ee.line(s"case (_: SEmpty | _: S${K}One | _: S${K}Sorted, _: SEmpty | _: S${K}One | _: S${K}Sorted) => unionTwo$K(a, b)")
        ee.line("case _ => new SUnion(a, b)")
        ee.close()
        ee.close()
        // (the union-spine collapse now lives inside materializeTree$K's union-run batching)
        // enumerate a bounded range [lo,hi] into a sorted prim array (the caller / materialize cap-guards span).
        if k.name == "Int" || k.name == "Long" then {
          ee.open(s"def rangeArr$K(lo: $P, hi: $P): Array[$P] =")
          ee.line("val n = (hi.toLong - lo.toLong + 1L).toInt")
          ee.line(s"val a = new Array[$P](n)")
          ee.line("var i = 0")
          ee.line("while (i < n) { a(i) = lo + i; i += 1 }")
          ee.line("a")
          ee.close()
        }
        // materialize: fold + memoize, ITERATIVELY over the left spine. Any of the four algebra ops loop-built
        // (incl/excl/&/^/++ chains are all LEFT-deep) would StackOverflow a left-recursive fold — collect the
        // spine, materialize the bottom, then fold back UP. Consecutive-union runs batch through a balanced
        // pairwise merge (O(N log k), what materializeUnion did); Inter/Diff/Xor apply sequentially (each is
        // one merge). Right children recurse (shallow in loop-built chains). Memos: Inter/Diff/Xor nodes and
        // the TOP node of each union run get theirs set (interior run nodes stay unset, same as before).
        val interExpr = if k.name == "Int" then "mergeAlgebraInt(acc, n.right, 1)" else s"wrap$K(mergeInter$K(asArr$K(acc), asArr$K(materialize$K(n.right))))"
        val diffExpr = if k.name == "Int" then "mergeAlgebraInt(acc, d.right, 2)" else s"wrap$K(mergeDiff$K(asArr$K(acc), asArr$K(materialize$K(d.right))))"
        val xorExpr = if k.name == "Int" then "mergeAlgebraInt(acc, x.right, 3)" else s"wrap$K(mergeXor$K(asArr$K(acc), asArr$K(materialize$K(x.right))))"
        val rangeCase =
          if k.name == "Int" || k.name == "Long" then
            s"""  case r: S${K}Range => { val s = r.hi.toLong - r.lo.toLong; if (s < 0L || s >= (1L << 20)) throw new UnsupportedOperationException("range too large to enumerate (> 2^20) — membership-only") else wrap$K(rangeArr$K(r.lo, r.hi)) }
               |""".stripMargin
          else ""
        ee.lines(
          s"""def isUnsetAlg$K(node: SBase): Boolean = node match {
             |  case u: SUnion => u.memo == null
             |  case n: SInter => n.memo == null
             |  case d: SDiff => d.memo == null
             |  case x: SXor => x.memo == null
             |  case _ => false
             |}
             |def materialize$K(node: SBase): SBase = node match {
             |  case m: SMaterialized => m
             |  // SHALLOW fast paths (left already materialized/memoized — the common `a & b` shape): merge
             |  // directly, no spine list allocation. Only a left-deep UNMATERIALIZED chain walks the tree.
             |  case u: SUnion =>
             |    val m = u.memo
             |    if (m != null) m
             |    else if (!isUnsetAlg$K(u.left)) { val r = unionTwo$K(materialize$K(u.left), materialize$K(u.right)); u.memo = r; r }
             |    else materializeTree$K(node)
             |  case n: SInter =>
             |    val m = n.memo
             |    if (m != null) m
             |    else if (!isUnsetAlg$K(n.left)) { val acc = materialize$K(n.left); val r = $interExpr; n.memo = r; r }
             |    else materializeTree$K(node)
             |  case d: SDiff =>
             |    val m = d.memo
             |    if (m != null) m
             |    else if (!isUnsetAlg$K(d.left)) { val acc = materialize$K(d.left); val r = $diffExpr; d.memo = r; r }
             |    else materializeTree$K(node)
             |  case x: SXor =>
             |    val m = x.memo
             |    if (m != null) m
             |    else if (!isUnsetAlg$K(x.left)) { val acc = materialize$K(x.left); val r = $xorExpr; x.memo = r; r }
             |    else materializeTree$K(node)
             |  case _ => materializeTree$K(node)
             |}
             |def materializeBottom$K(node: SBase): SBase = node match {
             |  case m: SMaterialized => m
             |  case u: SUnion => u.memo
             |  case n: SInter => n.memo
             |  case d: SDiff => d.memo
             |  case x: SXor => x.memo
             |$rangeCase  case _ => throw new UnsupportedOperationException("cannot materialize/enumerate an infinite or predicate set (above/below/universal/complement) — only contains is defined")
             |}
             |def materializeTree$K(node0: SBase): SBase = {
             |  val spine = new java.util.ArrayList[SBase]()
             |  var cur: SBase = node0
             |  var go = true
             |  while (go) cur match {
             |    case u: SUnion if u.memo == null => spine.add(u); cur = u.left
             |    case n: SInter if n.memo == null => spine.add(n); cur = n.left
             |    case d: SDiff if d.memo == null => spine.add(d); cur = d.left
             |    case x: SXor if x.memo == null => spine.add(x); cur = x.left
             |    case _ => go = false
             |  }
             |  var acc = materializeBottom$K(cur)
             |  var idx = spine.size - 1
             |  while (idx >= 0) {
             |    spine.get(idx) match {
             |      case _: SUnion =>
             |        val parts = new java.util.ArrayList[SBase]()
             |        parts.add(acc)
             |        var top = idx
             |        var run = true
             |        while (run && idx >= 0) spine.get(idx) match {
             |          case uu: SUnion => parts.add(materialize$K(uu.right)); top = idx; idx -= 1
             |          case _ => run = false
             |        }
             |        var sz = parts.size
             |        while (sz > 1) {
             |          var w = 0; var i = 0
             |          while (i + 1 < sz) { parts.set(w, unionTwo$K(parts.get(i), parts.get(i + 1))); w += 1; i += 2 }
             |          if (i < sz) { parts.set(w, parts.get(i)); w += 1 }
             |          sz = w
             |        }
             |        acc = parts.get(0)
             |        spine.get(top).asInstanceOf[SUnion].memo = acc
             |      case n: SInter =>
             |        acc = $interExpr
             |        n.memo = acc
             |        idx -= 1
             |      case d: SDiff =>
             |        acc = $diffExpr
             |        d.memo = acc
             |        idx -= 1
             |      case x: SXor =>
             |        acc = $xorExpr
             |        x.memo = acc
             |        idx -= 1
             |      case _ =>
             |        idx -= 1
             |    }
             |  }
             |  acc
             |}""".stripMargin)
      }
      // ---- Int dense-bitmap helpers (Step 2): density router + bitmap build + bit-iteration extraction.
      {
        // arr is sorted ascending. Dense (span ≤ 64·n) and bounded (span ≤ 2^20 bits) → bitmap; else the usual
        // ≤16 Sorted / Hash. A sparse-wide span falls through, so we never allocate a giant mostly-empty bitmap.
        ee.open("def routeInt(arr: Array[Int]): SBase =")
        ee.line("val n = arr.length")
        ee.line("if (n == 0) SEmpty.INSTANCE")
        ee.line("else if (n == 1) new SIntOne(arr(0))")
        ee.open("else")
        // SAME density rule as buildSortedInt (n ≥ 2 here): a small dense merge result keeps its O(1) bitmap
        // contains instead of degrading to a linear-scan Sorted leaf that an identical fresh build wouldn't use.
        ee.line("val span = arr(n - 1).toLong - arr(0).toLong + 1L")
        ee.line("if (span <= 64L * n && span <= (1L << 20)) buildBitmapInt(arr)")
        ee.line("else if (n <= 16) new SIntSorted(arr)")
        ee.line("else buildHashInt(arr)")
        ee.close()
        ee.close()
        // build a bitmap from a sorted distinct Int array (the router already proved the span dense + bounded).
        ee.open("def buildBitmapInt(arr: Array[Int]): SBase =")
        ee.line("val base = (arr(0) >> 6) << 6 // round DOWN to a multiple of 64 so two bitmaps word-align for merging")
        ee.line("val nwords = ((arr(arr.length - 1) - base) >>> 6) + 1")
        ee.line("val words = new Array[Long](nwords)")
        ee.line("var i = 0")
        ee.line("while (i < arr.length) { val b = arr(i) - base; words(b >>> 6) |= (1L << b); i += 1 }")
        ee.line("new SIntBitmap(base, arr.length, words)")
        ee.close()
        // extract a SORTED Int array from a bitmap (iterate set bits low→high → ascending) for merge/iterate.
        ee.open("def bitmapToArr(b: SIntBitmap): Array[Int] =")
        ee.line("val out = new Array[Int](b.card)")
        ee.line("val words = b.words; val base = b.base")
        ee.line("var wi = 0; var w = 0")
        ee.open("while (wi < words.length)")
        ee.line("var bits = words(wi)")
        ee.open("while (bits != 0L)")
        ee.line("val t = java.lang.Long.numberOfTrailingZeros(bits)")
        ee.line("out(w) = base + (wi << 6) + t; w += 1")
        ee.line("bits &= bits - 1L")
        ee.close()
        ee.line("wi += 1")
        ee.close()
        ee.line("out")
        ee.close()
        // canonicalize a merge result: down-convert a sparse result (fill < 1/16) back through routeInt — but
        // never churn a tiny (≤4-word) bitmap through extract+rebuild, there is nothing to save there.
        ee.lines(
          """def finishBitmapInt(base: Int, card: Int, words: Array[Long]): SBase =
            |  if (words.length > 4 && card.toLong * 16L < words.length.toLong * 64L) routeInt(bitmapToArr(new SIntBitmap(base, card, words)))
            |  else new SIntBitmap(base, card, words)""".stripMargin)
        // word-parallel bitmap algebra (bases are multiples of 64 → word grids align). op 0=∪ 1=∩ 2=∖ 3=△.
        // REGION-SPLIT kernel: the exclusive regions are arraycopied (∪/△) or untouched (∖) and only the
        // overlap runs a (branch-free) combining loop; ∩ allocates just the overlap window and ∖ just the left
        // window, not the full merged span. Same-grid single-word pairs are ONE op (the whole @16 dense case),
        // with operand-aliasing returns (immutability makes returning lb/rb sound). Offsets are computed in
        // LONG and a ∪/△ whose merged span exceeds 2^20 bits falls back to the sorted-array merge instead of
        // allocating a giant mostly-empty word array (far-apart bases previously did exactly that).
        ee.lines(
          """def bitmapMergeInt(lb: SIntBitmap, rb: SIntBitmap, op: Int): SBase = {
            |  val lw = lb.words; val rw = rb.words
            |  if (lb.base == rb.base && lw.length == 1 && rw.length == 1) {
            |    val w1 = lw(0); val w2 = rw(0)
            |    val r = if (op == 0) w1 | w2 else if (op == 1) w1 & w2 else if (op == 2) w1 & ~w2 else w1 ^ w2
            |    if (r == 0L) return SEmpty.INSTANCE
            |    if (r == w1) return lb
            |    if (r == w2) return rb
            |    val ws = new Array[Long](1); ws(0) = r
            |    return new SIntBitmap(lb.base, java.lang.Long.bitCount(r), ws)
            |  }
            |  val base = if (lb.base < rb.base) lb.base else rb.base
            |  val o1L = (lb.base.toLong - base.toLong) >> 6; val o2L = (rb.base.toLong - base.toLong) >> 6
            |  val e1L = o1L + lw.length; val e2L = o2L + rw.length
            |  val ovSL = if (o1L > o2L) o1L else o2L
            |  val ovEL = if (e1L < e2L) e1L else e2L
            |  if (op == 1) {
            |    if (ovSL >= ovEL) return SEmpty.INSTANCE
            |    val o1 = o1L.toInt; val o2 = o2L.toInt; val ovS = ovSL.toInt; val ovE = ovEL.toInt
            |    val n = ovE - ovS
            |    val words = new Array[Long](n)
            |    var card = 0; var k = 0
            |    while (k < n) { val r = lw(k + ovS - o1) & rw(k + ovS - o2); words(k) = r; card += java.lang.Long.bitCount(r); k += 1 }
            |    if (card == 0) return SEmpty.INSTANCE
            |    return finishBitmapInt(base + (ovS << 6), card, words)
            |  }
            |  if (op == 2) {
            |    if (ovSL >= ovEL) return lb
            |    val o1 = o1L.toInt; val o2 = o2L.toInt; val ovS = ovSL.toInt; val ovE = ovEL.toInt
            |    val words = java.util.Arrays.copyOf(lw, lw.length)
            |    var card = lb.card
            |    var k = ovS
            |    while (k < ovE) {
            |      val before = words(k - o1); val after = before & ~rw(k - o2)
            |      if (after != before) { card -= java.lang.Long.bitCount(before ^ after); words(k - o1) = after }
            |      k += 1
            |    }
            |    if (card == lb.card) return lb
            |    if (card == 0) return SEmpty.INSTANCE
            |    return finishBitmapInt(lb.base, card, words)
            |  }
            |  val endL = if (e1L > e2L) e1L else e2L
            |  if (endL > (1L << 14)) {
            |    val a = bitmapToArr(lb); val b = bitmapToArr(rb)
            |    return wrapInt(if (op == 0) mergeUnionInt(a, b) else mergeXorInt(a, b))
            |  }
            |  val o1 = o1L.toInt; val o2 = o2L.toInt; val ovS = ovSL.toInt; val ovE = ovEL.toInt
            |  val end = endL.toInt
            |  val words = new Array[Long](end)
            |  System.arraycopy(lw, 0, words, o1, lw.length)
            |  if (ovS >= ovE) System.arraycopy(rw, 0, words, o2, rw.length)
            |  else {
            |    if (o2 < ovS) System.arraycopy(rw, 0, words, o2, ovS - o2)
            |    if (ovE < e2L.toInt) System.arraycopy(rw, ovE - o2, words, ovE, e2L.toInt - ovE)
            |    var k = ovS
            |    if (op == 0) while (k < ovE) { words(k) |= rw(k - o2); k += 1 }
            |    else while (k < ovE) { words(k) ^= rw(k - o2); k += 1 }
            |  }
            |  var card = 0; var i = 0
            |  while (i < end) { card += java.lang.Long.bitCount(words(i)); i += 1 }
            |  if (card == 0) SEmpty.INSTANCE
            |  else finishBitmapInt(base, card, words)
            |}""".stripMargin)
        // materialize an Int algebra node: bitmap fast-path when both children are bitmaps, else array sorted-merge.
        ee.open("def mergeAlgebraInt(left: SBase, right: SBase, op: Int): SBase =")
        ee.line("val l = materializeInt(left); val rr = materializeInt(right)")
        ee.open("(l, rr) match")
        ee.line("case (lb: SIntBitmap, rb: SIntBitmap) => bitmapMergeInt(lb, rb, op)")
        ee.open("case _ =>")
        ee.line("val a = asArrInt(l); val b = asArrInt(rr)")
        ee.line("val arr = if (op == 0) mergeUnionInt(a, b) else if (op == 1) mergeInterInt(a, b) else if (op == 2) mergeDiffInt(a, b) else mergeXorInt(a, b)")
        ee.line("wrapInt(arr)")
        ee.close()
        ee.close()
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
        // force + memoize the hash-sorted view of a hash leaf. Deterministic result → the racy double-compute
        // is benign; hashes written BEFORE arr so a reader seeing sortedArr non-null also sees its pair.
        ee.open("def ensureSortedRef(h: SRefHash): Unit =")
        ee.open("if (h.sortedArr == null)")
        ee.line("val n = h.arr.length")
        ee.line("val sa = java.util.Arrays.copyOf(h.arr, n)")
        ee.line("val sh = java.util.Arrays.copyOf(h.hashes, n)")
        ee.line("sortByHashRef(sa, sh, n)")
        ee.line("h.sortedHashes = sh")
        ee.line("h.sortedArr = sa")
        ee.close()
        ee.close()
        // SORTED accessors (merge core / set-equality): extract the hash-sorted element array / parallel hash
        // key, forcing the hash leaf's memoized sorted view on first use.
        ee.open("def asArrRef(node: SBase): Array[Object] = node match")
        ee.line("case _: SEmpty => new Array[Object](0)")
        ee.line("case o: SRefOne => { val a = new Array[Object](1); a(0) = o.elem; a }")
        ee.line("case s: SRefSorted => s.arr")
        ee.line("case h: SRefHash => { ensureSortedRef(h); h.sortedArr }")
        ee.line("case _ => new Array[Object](0)")
        ee.close()
        ee.open("def hashesOfRef(node: SBase): Array[Int] = node match")
        ee.line("case _: SEmpty => new Array[Int](0)")
        ee.line("case o: SRefOne => { val a = new Array[Int](1); a(0) = o.elem.hashCode(); a }")
        ee.line("case s: SRefSorted => s.hashes")
        ee.line("case h: SRefHash => { ensureSortedRef(h); h.sortedHashes }")
        ee.line("case _ => new Array[Int](0)")
        ee.close()
        // RAW accessors (traversal / filter / map / iterator / setHash / subsetOf — anything order-insensitive):
        // the element column in whatever order the leaf holds it, NEVER forcing the sort.
        ee.open("def rawArrRef(node: SBase): Array[Object] = node match")
        ee.line("case h: SRefHash => h.arr")
        ee.line("case _ => asArrRef(node)")
        ee.close()
        ee.open("def rawHashesRef(node: SBase): Array[Int] = node match")
        ee.line("case h: SRefHash => h.hashes")
        ee.line("case _ => hashesOfRef(node)")
        ee.close()
        // wrap an arr+hashes column into the canonical leaf (≤4 → Sorted linear-scan; else the F14 table).
        // `sorted` records whether the column is ALREADY hash-sorted (merge outputs are; filter/build outputs
        // aren't) — it presets the hash leaf's sorted view / pre-sorts the tiny Sorted leaf.
        ee.open("def buildTableRef(arr: Array[Object], hashes: Array[Int], sorted: Boolean): SBase =")
        ee.line("val n = arr.length")
        ee.line("if (n == 0) return SEmpty.INSTANCE")
        ee.line("if (n == 1) return new SRefOne(arr(0))")
        ee.open("if (n <= 4)")
        ee.line("if (!sorted) sortByHashRef(arr, hashes, n)")
        ee.line("return new SRefSorted(arr, hashes)")
        ee.close()
        ee.line("var cap = 8")
        ee.line("while (cap < n * 2) cap <<= 1")
        ee.line("val ctrl = new Array[Byte](cap)")
        ee.line("val keys = new Array[Object](cap)")
        ee.line("val useRef = cap >= 1024 && cap <= 32768") // mid-band → cheap mixRef; else mixInt (probe agrees via ctrl.length)
        ee.line("var p = 0")
        ee.open("while (p < n)")
        ee.line("val m = if (useRef) mixRef(hashes(p)) else mixInt(hashes(p))")
        ee.line("var slot = m & (cap - 1)")
        ee.line("while (ctrl(slot) != 0) slot = (slot + 1) & (cap - 1)")
        ee.line("ctrl(slot) = (((m >>> 24) & 0x7f) | 0x80).toByte")
        ee.line("keys(slot) = arr(p)")
        ee.line("p += 1")
        ee.close()
        ee.line("new SRefHash(arr, hashes, ctrl, keys, sorted)")
        ee.close()
        ee.line("def wrapRef(arr: Array[Object], hashes: Array[Int]): SBase = buildTableRef(arr, hashes, true)")
        ee.line("def wrapRefRaw(arr: Array[Object], hashes: Array[Int]): SBase = buildTableRef(arr, hashes, false)")
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
        // union of two materialized Ref leaves + the iterative left-spine collapse (Step 5, StackOverflow-safe).
        ee.open("def unionTwoRef(a: SBase, b: SBase): SBase =")
        ee.line("mergeUnionRef(asArrRef(a), hashesOfRef(a), asArrRef(b), hashesOfRef(b))")
        ee.close()
        // smart `++`: eager-merge two SMALL ref leaves (≤16, cheap); stay LAZY for large hash leaves (the O(n)
        // sort-merge is the very thing that wins on refs by being deferred). Mixed/large → lazy.
        ee.open("def unionEagerRef(a: SBase, b: SBase): SBase =")
        ee.open("(a, b) match")
        ee.line("case (_: SEmpty | _: SRefOne | _: SRefSorted, _: SEmpty | _: SRefOne | _: SRefSorted) => unionTwoRef(a, b)")
        ee.line("case _ => new SUnion(a, b)")
        ee.close()
        ee.close()
        // materialize: fold + memoize, ITERATIVELY over the left spine (mirror of materializeTree for prims —
        // see that comment; the merges here are the tie-group-aware cached-hash Ref merges).
        ee.lines(
          """def isUnsetAlgRef(node: SBase): Boolean = node match {
            |  case u: SUnion => u.memo == null
            |  case n: SInter => n.memo == null
            |  case d: SDiff => d.memo == null
            |  case x: SXor => x.memo == null
            |  case _ => false
            |}
            |def materializeRef(node: SBase): SBase = node match {
            |  case m: SMaterialized => m
            |  // SHALLOW fast paths — mirror of the prim materialize (see that comment).
            |  case u: SUnion =>
            |    val m = u.memo
            |    if (m != null) m
            |    else if (!isUnsetAlgRef(u.left)) { val r = unionTwoRef(materializeRef(u.left), materializeRef(u.right)); u.memo = r; r }
            |    else materializeTreeRef(node)
            |  case n: SInter =>
            |    val m = n.memo
            |    if (m != null) m
            |    else if (!isUnsetAlgRef(n.left)) { val acc = materializeRef(n.left); val rr = materializeRef(n.right); val r = mergeInterRef(asArrRef(acc), hashesOfRef(acc), asArrRef(rr), hashesOfRef(rr)); n.memo = r; r }
            |    else materializeTreeRef(node)
            |  case d: SDiff =>
            |    val m = d.memo
            |    if (m != null) m
            |    else if (!isUnsetAlgRef(d.left)) { val acc = materializeRef(d.left); val rr = materializeRef(d.right); val r = mergeDiffRef(asArrRef(acc), hashesOfRef(acc), asArrRef(rr), hashesOfRef(rr)); d.memo = r; r }
            |    else materializeTreeRef(node)
            |  case x: SXor =>
            |    val m = x.memo
            |    if (m != null) m
            |    else if (!isUnsetAlgRef(x.left)) { val acc = materializeRef(x.left); val rr = materializeRef(x.right); val r = mergeXorRef(asArrRef(acc), hashesOfRef(acc), asArrRef(rr), hashesOfRef(rr)); x.memo = r; r }
            |    else materializeTreeRef(node)
            |  case _ => materializeTreeRef(node)
            |}
            |def materializeBottomRef(node: SBase): SBase = node match {
            |  case m: SMaterialized => m
            |  case u: SUnion => u.memo
            |  case n: SInter => n.memo
            |  case d: SDiff => d.memo
            |  case x: SXor => x.memo
            |  case _ => throw new UnsupportedOperationException("cannot materialize/enumerate an infinite or predicate Ref set — only contains is defined")
            |}
            |def materializeTreeRef(node0: SBase): SBase = {
            |  val spine = new java.util.ArrayList[SBase]()
            |  var cur: SBase = node0
            |  var go = true
            |  while (go) cur match {
            |    case u: SUnion if u.memo == null => spine.add(u); cur = u.left
            |    case n: SInter if n.memo == null => spine.add(n); cur = n.left
            |    case d: SDiff if d.memo == null => spine.add(d); cur = d.left
            |    case x: SXor if x.memo == null => spine.add(x); cur = x.left
            |    case _ => go = false
            |  }
            |  var acc = materializeBottomRef(cur)
            |  var idx = spine.size - 1
            |  while (idx >= 0) {
            |    spine.get(idx) match {
            |      case _: SUnion =>
            |        val parts = new java.util.ArrayList[SBase]()
            |        parts.add(acc)
            |        var top = idx
            |        var run = true
            |        while (run && idx >= 0) spine.get(idx) match {
            |          case uu: SUnion => parts.add(materializeRef(uu.right)); top = idx; idx -= 1
            |          case _ => run = false
            |        }
            |        var sz = parts.size
            |        while (sz > 1) {
            |          var w = 0; var i = 0
            |          while (i + 1 < sz) { parts.set(w, unionTwoRef(parts.get(i), parts.get(i + 1))); w += 1; i += 2 }
            |          if (i < sz) { parts.set(w, parts.get(i)); w += 1 }
            |          sz = w
            |        }
            |        acc = parts.get(0)
            |        spine.get(top).asInstanceOf[SUnion].memo = acc
            |      case n: SInter =>
            |        val rr = materializeRef(n.right)
            |        acc = mergeInterRef(asArrRef(acc), hashesOfRef(acc), asArrRef(rr), hashesOfRef(rr))
            |        n.memo = acc
            |        idx -= 1
            |      case d: SDiff =>
            |        val rr = materializeRef(d.right)
            |        acc = mergeDiffRef(asArrRef(acc), hashesOfRef(acc), asArrRef(rr), hashesOfRef(rr))
            |        d.memo = acc
            |        idx -= 1
            |      case x: SXor =>
            |        val rr = materializeRef(x.right)
            |        acc = mergeXorRef(asArrRef(acc), hashesOfRef(acc), asArrRef(rr), hashesOfRef(rr))
            |        x.memo = acc
            |        idx -= 1
            |      case _ =>
            |        idx -= 1
            |    }
            |  }
            |  acc
            |}""".stripMargin)
      }
      ee.result
    }

    // ---- the surface impls (inline, dispatch the kind, route to the shared non-inline helpers) ----------

    // contains: dispatch the kind, unwrap the element, call the shared containsLeaf${K}.
    // Int inlines the bitmap bit-test at the (inline) surface so a dense-Int contains is one bit-test with NO
    // containsLeafInt call (that method is too big to inline: 12-case match + loop). Everything else defers.
    val containsV = dispatchA(k =>
      if k.name == "Int" then
        s"{ val e0 = ${unwrapElem(k, "elem")}; xs match { case b: SIntBitmap => { val i = e0 - b.base; i >= 0 && (i >>> 6) < b.words.length && (b.words(i >>> 6) >>> i & 1L) != 0L }; case _ => containsLeafInt(xs, e0) } }"
      else s"containsLeaf${k.name}(xs, ${unwrapElem(k, "elem")})"
    )

    // union: the smart `++` — kind-dispatch to unionEager${K}, which eager-merges cheap operand pairs.
    val unionV = dispatchA(k => s"unionEager${k.name}(xs, that)")

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
        s"buildSorted${k.name}(Array[$arrT]($stores), $n, true)"
      }
    }

    // fromArray: unwrap the source Array[A] and build with owned=false — NO defensive copy here. buildSorted
    // only mutates its input on the prim SORT branch (where it copies itself when !owned); the dense-Int bitmap
    // branch and the whole Ref dedup branch are read-only, so the common paths pay zero extra copies.
    val fromArrayV = dispatchA { k =>
      if k.isPrim then
        s"{ val src = as.asInstanceOf[Array[${k.arr}]]; buildSorted${k.name}(src, src.length, false) }"
      else
        s"{ val src = as.asInstanceOf[Array[Object]]; buildSorted${k.name}(src, src.length, false) }"
    }

    // from(IterableOnce): generic build — drain into a RAW growable kind array (ArrayBuffer is unspecialized
    // and would box every primitive element AND store them behind an Object[]), then build with owned=true (the
    // scratch array is ours, so the sort path sorts in place with no extra copy). (fromFArray routes through
    // here from the surface via `fa.iterator`; a direct unboxed leaf read off the FArray's ${K}Arr is a
    // NEXT-STEP zero-copy bridge.)
    val fromV = dispatchA { k =>
      val arrT = if k.name == "Ref" then "Object" else k.arr
      s"{ val it0 = it.iterator; var raw = new Array[$arrT](16); var n = 0; while (it0.hasNext) { if (n == raw.length) raw = java.util.Arrays.copyOf(raw, n << 1); raw(n) = ${storeRaw(k, unwrapElem(k, "it0.next()"))}; n += 1 }; buildSorted${k.name}(raw, n, true) }"
    }

    // dispatch on the ELEMENT kind B (mirrors FArray's appendImpl[A, B], which dispatches `${K}Repr[B]`).
    def dispatchB(body: Kind => String): String =
      "summonFrom {\n" + opKinds.map(k => s"      case r: ${k.name}Repr[B] => ${body(k)}").mkString("\n") + "\n    }"

    // incl: Union(this, One(elem)) — O(1), shares the base; returns `this` (a sound alias by immutability,
    // §3.2) when a cheap contains proves the element already present. Shape MIRRORS FArray's appendImpl[A, B]
    // exactly (two type params, dispatch on the element kind B, build a node node from the `xs: SBase` param) —
    // the structure that lets the `SBase`-result up-coerce to the opaque `FSet[B]` at foreign call sites.
    // NOTE eager bitmap incl/excl (word-copy + bit set/clear below a small word threshold) was MEASURED and
    // REVERTED: @16 it is 0.4x the lazy node (292M→115M ops/s). scala's BitSet1/BitSet2 keep the word in a
    // FIELD (no array alloc), which an array-backed bitmap copy cannot match; the lazy SUnion/SDiff node is
    // the better persistent-update shape at every size.
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

    // iterator: materialize to a leaf and walk its elements. An Int BITMAP leaf gets a direct word-scanning
    // iterator (no bitmapToArr extraction — the old path allocated and filled a full Int[] before yielding the
    // first element); other leaves walk their array. `next` boxes either way (Iterator is unspecialized — the
    // same tax every competitor's iterator pays). The cast to Iterator[A] is sound (A is concrete at the site).
    val iteratorV = dispatchA(k =>
      if k.name == "Int" then
        "{ materializeInt(xs) match { case b: SIntBitmap => new SIntBitmapIterator(b.words, b.base).asInstanceOf[Iterator[A]]; case m => asArrInt(m).iterator.asInstanceOf[Iterator[A]] } }"
      else if k.name == "Ref" then "rawArrRef(materializeRef(xs)).iterator.asInstanceOf[Iterator[A]]"
      else s"asArr${k.name}(materialize${k.name}(xs)).iterator.asInstanceOf[Iterator[A]]")

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
      if k.name == "Int" then "subsetOfInt(a, b)"
      else if k.name == "Ref" then
        "{ val arr = rawArrRef(materializeRef(a)); var i = 0; var ok = true; while (i < arr.length && ok) { if (!containsLeafRef(b, arr(i))) ok = false; i += 1 }; ok }"
      else s"{ val arr = asArr${k.name}(materialize${k.name}(a)); var i = 0; var ok = true; while (i < arr.length && ok) { if (!containsLeaf${k.name}(b, arr(i))) ok = false; i += 1 }; ok }"
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
        // one walk per op. For INT, a dense node is a bitmap → iterate the long[] words directly (numberOfTrailingZeros
        // loop, like BitSet) with NO array materialization; sparse Int + all other kinds walk asArr. `e` is the element.
        def walk(name: String, sig: String, ret: String, accum: String, perElem: String, contCond: String, result: String): Unit = {
          val cc = if contCond.nonEmpty then s" && $contCond" else ""
          ee.open(s"def ${name}Leaf$K(node: SBase, $sig): $ret =")
          if accum.nonEmpty then ee.line(accum)
          if k.name == "Int" then {
            ee.open("materializeInt(node) match")
            ee.open("case b: SIntBitmap =>")
            ee.line("val ws = b.words; val bs = b.base; var wi = 0")
            ee.open(s"while (wi < ws.length$cc)")
            ee.line("var bits = ws(wi)")
            ee.open(s"while (bits != 0L$cc)")
            ee.line("val e = bs + (wi << 6) + java.lang.Long.numberOfTrailingZeros(bits)")
            ee.line(perElem)
            ee.line("bits &= bits - 1L")
            ee.close()
            ee.line("wi += 1")
            ee.close()
            ee.closeOpen("case m =>")
            ee.line("val arr = asArrInt(m); var i = 0")
            ee.open(s"while (i < arr.length$cc)")
            ee.line("val e = arr(i)")
            ee.line(perElem)
            ee.line("i += 1")
            ee.close()
            ee.close()
            ee.close()
          } else {
            // Ref walks the RAW column (order-insensitive) so a traversal never forces the sorted view.
            val acc = if k.name == "Ref" then "rawArrRef" else s"asArr$K"
            ee.line(s"val arr = $acc(materialize$K(node)); var i = 0")
            ee.open(s"while (i < arr.length$cc)")
            ee.line("val e = arr(i)")
            ee.line(perElem)
            ee.line("i += 1")
            ee.close()
          }
          if result.nonEmpty then ee.line(result)
          ee.close()
        }
        walk("foreach", s"f: SetTraversers.${K}Consumer", "Unit", "", "f.accept(e)", "", "")
        walk("forall", s"p: SetTraversers.${K}Pred", "Boolean", "var ok = true", "if (!p.test(e)) ok = false", "ok", "ok")
        walk("exists", s"p: SetTraversers.${K}Pred", "Boolean", "var found = false", "if (p.test(e)) found = true", "!found", "found")
        walk("count", s"p: SetTraversers.${K}Pred", "Int", "var c = 0", "if (p.test(e)) c += 1", "", "c")
        // filter: keep elements satisfying p, build the same-kind leaf. Prim: the kept slice of the sorted arr
        // stays sorted+distinct → wrap directly (no re-sort). Ref: the kept subset is still distinct → re-hash+
        // sort+wrap via buildSortedRef (its dedup is a no-op on an already-distinct input).
        // shared array-filter body (operates on a local `arr`): keep p-passing elements, build the same-kind leaf.
        def primFilterBody(): Unit = {
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
        }
        ee.open(s"def filterLeaf$K(node: SBase, p: SetTraversers.${K}Pred): SBase =")
        if k.name == "Int" then {
          // materialize ONCE; a bitmap leaf is word-filtered directly (test p per set bit, build result words —
          // no extract, no sort; down-convert if the kept subset turns sparse). Other leaves take the array path.
          ee.line("val mz = materializeInt(node)")
          ee.open("mz match")
          ee.open("case b: SIntBitmap =>")
          ee.line("val ws = b.words; val bs = b.base")
          ee.line("val fout = new Array[Long](ws.length)")
          ee.line("var fwi = 0; var fcard = 0")
          ee.open("while (fwi < ws.length)")
          ee.line("var bits = ws(fwi); var rb = 0L")
          ee.open("while (bits != 0L)")
          ee.line("val t = java.lang.Long.numberOfTrailingZeros(bits)")
          ee.line("if (p.test(bs + (fwi << 6) + t)) { rb |= (1L << t); fcard += 1 }")
          ee.line("bits &= bits - 1L")
          ee.close()
          ee.line("fout(fwi) = rb; fwi += 1")
          ee.close()
          ee.line("if (fcard == 0) SEmpty.INSTANCE")
          ee.line("else if (fcard.toLong * 32 >= (ws.length.toLong << 6)) new SIntBitmap(bs, fcard, fout)")
          ee.line("else wrapInt(bitmapToArr(new SIntBitmap(bs, fcard, fout)))")
          ee.closeOpen("case _ =>")
          ee.line("val arr = asArrInt(mz)")
          primFilterBody()
          ee.close()
          ee.close()
        } else if k.isPrim then {
          setup()
          primFilterBody()
        } else {
          // Ref: the kept subset of a hash-sorted distinct leaf is STILL hash-sorted and distinct — filter the
          // element array and its parallel hash cache together and wrap directly. (The old path re-ran the whole
          // buildSortedRef pipeline: a fresh dedup table + a re-sort, all provably no-ops on this input.)
          ee.line(s"val mz = materialize$K(node)")
          ee.line("val arr = rawArrRef(mz); val hs = rawHashesRef(mz)")
          ee.line("val outA = new Array[Object](arr.length)")
          ee.line("val outH = new Array[Int](arr.length)")
          ee.line("var i = 0; var w = 0")
          ee.open("while (i < arr.length)")
          ee.line("val v = arr(i)")
          ee.line("if (p.test(v)) { outA(w) = v; outH(w) = hs(i); w += 1 }")
          ee.line("i += 1")
          ee.close()
          ee.line("if (w == 0) SEmpty.INSTANCE")
          ee.line(s"else if (w == 1) new S${K}One(outA(0))")
          ee.line(s"else wrap${K}Raw(java.util.Arrays.copyOf(outA, w), java.util.Arrays.copyOf(outH, w))")
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
      def srcOf(ka: Kind): String =
        if ka.name == "Ref" then "rawArrRef(materializeRef(xs))" else s"asArr${ka.name}(materialize${ka.name}(xs))"
      def readA(ka: Kind): String = if ka.isPrim then "ra.wrap(src(i))" else "src(i).asInstanceOf[A]"
      def storeB(kb: Kind, b: String): String = if kb.isPrim then s"rb.unwrap($b)" else s"($b).asInstanceOf[Object]"
      def genericBody(ka: Kind, kb: Kind): String =
        s"{ val src = ${srcOf(ka)}; val n = src.length; val out = new Array[${kb.arr}](n); var i = 0; while (i < n) { out(i) = ${storeB(kb, "f(" + readA(ka) + ")")}; i += 1 }; buildSorted${kb.name}(out, n, true) }"
      // Int SOURCE: skip the asArrInt materialization — word-scan the bitmap leaf directly and apply f in ONE
      // pass into out[] (saves the intermediate Int[] extraction + its pass; buildSortedInt's dense-build path
      // then dedups+routes). Non-bitmap Int leaves (sparse/wide) take the array path. `card` sizes out exactly.
      def intSrcBody(kb: Kind): String =
        s"{ materializeInt(xs) match { " +
          s"case bm: SIntBitmap => { val ws = bm.words; val bs = bm.base; val out = new Array[${kb.arr}](bm.card); var w = 0; var wi = 0; " +
          s"while (wi < ws.length) { var bits = ws(wi); while (bits != 0L) { val e = bs + (wi << 6) + java.lang.Long.numberOfTrailingZeros(bits); out(w) = ${storeB(kb, "f(ra.wrap(e))")}; w += 1; bits &= bits - 1L }; wi += 1 }; buildSorted${kb.name}(out, w, true) }; " +
          s"case m => { val src = asArrInt(m); val n = src.length; val out = new Array[${kb.arr}](n); var i = 0; while (i < n) { out(i) = ${storeB(kb, "f(ra.wrap(src(i)))")}; i += 1 }; buildSorted${kb.name}(out, n, true) } } }"
      // Int DEST: feed f's outputs straight into the growable bitmap accumulator — a dense map keeps bitmap
      // form end-to-end with no intermediate array, no min/max pass, no sort (the immutable-BitSet advantage).
      def intDestBody(ka: Kind): String =
        if ka.name == "Int" then
          "{ val bld = new IntBitmapBuilder; materializeInt(xs) match { " +
            "case bm: SIntBitmap => { val ws = bm.words; val bs = bm.base; var wi = 0; " +
            "while (wi < ws.length) { var bits = ws(wi); while (bits != 0L) { val e = bs + (wi << 6) + java.lang.Long.numberOfTrailingZeros(bits); bld.add(rb.unwrap(f(ra.wrap(e)))); bits &= bits - 1L }; wi += 1 } }; " +
            "case m => { val src = asArrInt(m); var i = 0; while (i < src.length) { bld.add(rb.unwrap(f(ra.wrap(src(i))))); i += 1 } } }; bld.result() }"
        else
          s"{ val src = ${srcOf(ka)}; val n = src.length; val bld = new IntBitmapBuilder; var i = 0; while (i < n) { bld.add(rb.unwrap(f(${readA(ka)}))); i += 1 }; bld.result() }"
      "summonFrom {\n" + opKinds.map { ka =>
        s"      case ra: ${ka.name}Repr[A] => summonFrom {\n" +
          opKinds.map { kb =>
            val body =
              if kb.name == "Int" then intDestBody(ka)
              else if ka.name == "Int" then intSrcBody(kb)
              else genericBody(ka, kb)
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
          if k.name == "Int" then {
            // bitmap word-path: sum the per-element mixes straight from the set bits (no asArr extraction).
            ee.open("case b: SIntBitmap =>")
            ee.line("val ws = b.words; val bs = b.base")
            ee.line("var h = 0; var wi = 0")
            ee.open("while (wi < ws.length)")
            ee.line("var bits = ws(wi)")
            ee.open("while (bits != 0L)")
            ee.line("val t = java.lang.Long.numberOfTrailingZeros(bits)")
            ee.line(s"h += ${slotHash(k, "(bs + (wi << 6) + t)")}")
            ee.line("bits &= bits - 1L")
            ee.close()
            ee.line("wi += 1")
            ee.close()
            ee.line("h + b.card")
            ee.close()
          }
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
          // unboxed: fold the leaf's parallel hash cache directly (commutative → order/shape-independent, so
          // the RAW column suffices — never forces the sorted view).
          ee.open("case _: SMaterialized =>")
          ee.line("val hs = rawHashesRef(node)")
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
          // two bitmaps of equal sets share a base (= (min>>6)<<6) and word array → compare words, no extraction.
          if k.name == "Int" then ee.line("case (ab: SIntBitmap, bb: SIntBitmap) => ab.base == bb.base && java.util.Arrays.equals(ab.words, bb.words)")
          ee.line(s"case (_: SMaterialized, _: SMaterialized) => java.util.Arrays.equals(asArr$K(a), asArr$K(b))")
          ee.line(s"case _ => $throwMsg")
          ee.close()
        } else {
          // UNBOXED: both leaves are HASH-SORTED, so equal sets have identical sorted-hash sequences. Two-pointer
          // over the STORED hashes (no per-element hashCode/mix/slot-probe) — at a hash mismatch reject; inside an
          // equal-hash tie-group verify the elements match as sets via .equals. (The old collectElems walk built
          // two boxed HashSets — 10x slow; this is the lone remaining Ref op being un-boxed.)
          ee.open("def setEqRef(a: SBase, b: SBase): Boolean = (a, b) match")
          ee.open("case (am: SMaterialized, bm: SMaterialized) =>")
          ee.line("if (am.size() != bm.size()) false")
          ee.open("else")
          ee.line("val aA = asArrRef(a); val aH = hashesOfRef(a); val bA = asArrRef(b); val bH = hashesOfRef(b)")
          ee.line("var i = 0; var ok = true")
          ee.open("while (i < aA.length && ok)")
          ee.line("if (aH(i) != bH(i)) ok = false")
          // POSITIONAL fast path: equal sets in the same hash-sort order match pairwise at ~every index; only a
          // differently-ordered tie group falls into the set-compare. Restricting that compare to [i, j) is
          // sound: positions before i matched pairwise and elements are distinct, so nothing at ≥ i can have
          // its match among them.
          ee.line("else if (aA(i).equals(bA(i))) i += 1")
          ee.open("else")
          ee.line("var j = i + 1; while (j < aA.length && aH(j) == aH(i)) j += 1")
          ee.line("var k = i")
          ee.open("while (k < j && ok)")
          ee.line("val v = aA(k); var found = false; var m = i")
          ee.line("while (m < j && !found) { if (bA(m).equals(v)) found = true; m += 1 }")
          ee.line("if (!found) ok = false")
          ee.line("k += 1")
          ee.close()
          ee.line("i = j")
          ee.close()
          ee.close()
          ee.line("ok")
          ee.close()
          ee.close()
          ee.line(s"case _ => $throwMsg")
          ee.close()
        }
      }
      // subsetOf(a ⊆ b): two bitmaps → WORD-CONTAINMENT (every set bit of a is set in b, base-aligned) in
      // O(span_a/64); otherwise materialize a and probe each element against b (b stays lazy).
      ee.open("def subsetOfInt(a: SBase, b: SBase): Boolean =")
      ee.line("val am = materializeInt(a)")
      ee.open("if (am.isInstanceOf[SIntBitmap])")
      ee.line("val bm = materializeInt(b)")
      ee.open("if (bm.isInstanceOf[SIntBitmap])")
      ee.line("val ab = am.asInstanceOf[SIntBitmap]; val bb = bm.asInstanceOf[SIntBitmap]")
      ee.line("val aw = ab.words; val bw = bb.words; val delta = ((ab.base.toLong - bb.base.toLong) >> 6).toInt")
      ee.line("var wi = 0; var ok = true")
      ee.open("while (wi < aw.length && ok)")
      ee.line("val w = aw(wi)")
      ee.line("if (w != 0L) { val bj = wi + delta; if (bj < 0 || bj >= bw.length || (w & ~bw(bj)) != 0L) ok = false }")
      ee.line("wi += 1")
      ee.close()
      ee.line("return ok")
      ee.close()
      ee.close()
      ee.line("val arr = asArrInt(am); var i = 0; var ok2 = true")
      ee.line("while (i < arr.length && ok2) { if (!containsLeafInt(b, arr(i))) ok2 = false; i += 1 }")
      ee.line("ok2")
      ee.close()
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
       |  // FSetFinite support: a set is "finite" (safely enumerable/materializable) iff every leaf is a
       |  // materialized leaf OR a bounded range with span < 2^20; complement / over-cap ranges / unknown
       |  // predicates are infinite. Conservative all-children check — correct for the naive enumerate-then-merge
       |  // materialize (it would over-enumerate a `hugeRange ∩ smallSet`, which a predicate-distributing
       |  // materialize could do; that's the NEXT-STEP). Kind-agnostic (walks the shared algebra nodes).
       |  def isFinite(node: SBase): Boolean = node match {
       |    case _: SMaterialized => true
       |    case r: SIntRange  => { val s = r.hi.toLong - r.lo.toLong; s >= 0L && s < (1L << 20) }
       |    case r: SLongRange => { val s = r.hi - r.lo; s >= 0L && s < (1L << 20) }
       |    case u: SUnion => isFinite(u.left) && isFinite(u.right)
       |    case n: SInter => isFinite(n.left) && isFinite(n.right)
       |    case d: SDiff  => isFinite(d.left) && isFinite(d.right)
       |    case x: SXor   => isFinite(x.left) && isFinite(x.right)
       |    case _ => false
       |  }
       |  inline def isFiniteImpl[A](xs: SBase): Boolean = isFinite(xs)
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
       |  inline def unionImpl[A](xs: SBase, that: SBase): SBase = $unionV
       |}
       |
       |// Direct word-scanning iterator over a dense-Int bitmap leaf — yields set bits low→high (ascending
       |// element order) without extracting the bitmap to an Int[] first. Tolerates leading/interior zero words
       |// (a diff can clear whole words in place).
       |// Growable dense-Int bitmap accumulator for map[_, Int]: values are OR'd straight into a word array that
       |// rebases/doubles on both ends (base stays a multiple of 64), so a dense map keeps bitmap form end-to-end
       |// with no intermediate Int[] + sort. If the running span ever exceeds 2^20 bits the builder DEGRADES once:
       |// it extracts the bits collected so far and appends raw values from then on (dups fine — buildSortedInt
       |// dedups), i.e. sparse/wide outputs pay ~the old path, dense outputs skip it entirely.
       |private[farray] final class IntBitmapBuilder {
       |  private var words: Array[Long] = null
       |  private var baseWord = 0 // word index of words(0), i.e. base >> 6
       |  private var arr: Array[Int] = null // degraded (sparse/wide) mode: raw append, dedup at result()
       |  private var n = 0
       |
       |  def add(v: Int): Unit = {
       |    if (arr != null) { addArr(v); return }
       |    val vw = v >> 6
       |    if (words == null) {
       |      words = new Array[Long](4)
       |      baseWord = vw
       |      words(0) = 1L << v
       |      return
       |    }
       |    val off = vw - baseWord
       |    if (off >= 0 && off < words.length) { words(off) |= 1L << v; return }
       |    grow(vw)
       |    if (arr != null) { addArr(v); return }
       |    words(vw - baseWord) |= 1L << v
       |  }
       |
       |  private def grow(vw: Int): Unit = {
       |    val loW = if (vw < baseWord) vw else baseWord
       |    val hiW = if (vw + 1 > baseWord + words.length) vw + 1 else baseWord + words.length
       |    val span = hiW - loW
       |    if (span > 16384) { degrade(); return }
       |    var cap = words.length << 1
       |    while (cap < span) cap <<= 1
       |    if (cap > 16384) cap = 16384
       |    val nw = new Array[Long](cap)
       |    // growing down puts the slack below (newBase = hiW - cap ≤ loW); growing up keeps base and slack above
       |    val newBaseWord = if (vw < baseWord) hiW - cap else baseWord
       |    System.arraycopy(words, 0, nw, baseWord - newBaseWord, words.length)
       |    words = nw
       |    baseWord = newBaseWord
       |  }
       |
       |  private def degrade(): Unit = {
       |    var card = 0; var i = 0
       |    while (i < words.length) { card += java.lang.Long.bitCount(words(i)); i += 1 }
       |    val cap = if (card + 16 > 32) card + 16 else 32
       |    val a = new Array[Int](cap)
       |    val base = baseWord << 6
       |    var w = 0; var wi = 0
       |    while (wi < words.length) {
       |      var bits = words(wi)
       |      while (bits != 0L) { a(w) = base + (wi << 6) + java.lang.Long.numberOfTrailingZeros(bits); w += 1; bits &= bits - 1L }
       |      wi += 1
       |    }
       |    arr = a; n = w
       |    words = null
       |  }
       |
       |  private def addArr(v: Int): Unit = {
       |    if (n == arr.length) arr = java.util.Arrays.copyOf(arr, n << 1)
       |    arr(n) = v; n += 1
       |  }
       |
       |  def result(): SBase = {
       |    if (arr != null) return FSetOps.buildSortedInt(arr, n, true)
       |    if (words == null) return SEmpty.INSTANCE
       |    var lo = 0
       |    while (lo < words.length && words(lo) == 0L) lo += 1
       |    if (lo == words.length) return SEmpty.INSTANCE
       |    var hi = words.length - 1
       |    while (words(hi) == 0L) hi -= 1
       |    var card = 0; var i = lo
       |    while (i <= hi) { card += java.lang.Long.bitCount(words(i)); i += 1 }
       |    val base = (baseWord + lo) << 6
       |    if (card == 1) return new SIntOne(base + java.lang.Long.numberOfTrailingZeros(words(lo)))
       |    val ws = if (lo == 0 && hi == words.length - 1) words else java.util.Arrays.copyOfRange(words, lo, hi + 1)
       |    FSetOps.finishBitmapInt(base, card, ws)
       |  }
       |}
       |
       |private[farray] final class SIntBitmapIterator(words: Array[Long], base: Int) extends scala.collection.AbstractIterator[Int] {
       |  private var wi = 0
       |  private var bits = if (words.length == 0) 0L else words(0)
       |  advance()
       |  private def advance(): Unit = { while (bits == 0L && wi + 1 < words.length) { wi += 1; bits = words(wi) } }
       |  def hasNext: Boolean = bits != 0L
       |  def next(): Int = {
       |    val t = java.lang.Long.numberOfTrailingZeros(bits)
       |    val e = base + (wi << 6) + t
       |    bits &= bits - 1L
       |    if (bits == 0L) advance()
       |    e
       |  }
       |}
       |""".stripMargin
  }
}
