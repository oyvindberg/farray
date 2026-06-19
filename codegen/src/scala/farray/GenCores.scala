package farray

import bleep.*
import bleep.internal.FileUtils

import java.nio.file.Files

/**
 * Generates the sealed FArray core hierarchy: `FBase` (abstract storage-shuffling API + a concrete
 * O(1) `concat` that builds a `Concat` tree node) + one `<Name>Arr` leaf per primitive + the
 * `Object[]`-backed `RefArr` leaf + the `Concat` tree node. Wired as `farray`'s sourcegen.
 *
 * Element-touching combinators and element-insert ops (`:+`/`+:`/`updated`) live in Scala (inline,
 * specialized) — NOT here. Adding a primitive = one row in `prims`.
 */
object GenCores extends BleepCodegenScript("GenCores") {

  final case class Prim(name: String, jt: String, boxed: String)

  val prims: List[Prim] = List(
    Prim("Int", "int", "Integer"),
    Prim("Long", "long", "Long"),
    Prim("Double", "double", "Double"),
    Prim("Float", "float", "Float"),
    Prim("Short", "short", "Short"),
    Prim("Byte", "byte", "Byte"),
    Prim("Char", "char", "Character"),
    Prim("Boolean", "boolean", "Boolean")
  )

  override def run(started: Started, commands: Commands, targets: List[GenCores.Target], args: List[String]): Unit =
    targets.foreach { target =>
      val dir = target.sources.resolve("farray")
      FileUtils.deleteDirectory(target.sources)
      Files.createDirectories(dir)
      Files.writeString(dir.resolve("FBase.java"), fbase)
      Files.writeString(dir.resolve("RefArr.java"), refArr)
      Files.writeString(dir.resolve("RefAppend.java"), refUnary("Append"))
      Files.writeString(dir.resolve("RefPrepend.java"), refUnary("Prepend"))
      Files.writeString(dir.resolve("Concat.java"), concat)
      prims.foreach { p =>
        Files.writeString(dir.resolve(s"${p.name}Arr.java"), primCore(p))
        Files.writeString(dir.resolve(s"${p.name}Append.java"), primAppend(p))
        Files.writeString(dir.resolve(s"${p.name}Prepend.java"), primPrepend(p))
      }
      Files.writeString(dir.resolve("FArrayOps.scala"), farrayOps)
    }

  // ===== generated per-kind Scala combinators (the read×write matrix lives here, not hand-written) =====

  /** name=core prefix (Int/Long/Double/Ref); arr=Scala array elem type; pat=erasedValue pattern; dflt=zero. */
  final case class Kind(name: String, arr: String, pat: String, dflt: String) { def lc = name.toLowerCase }

  // Start with the common numeric kinds + reference; add rows here to cover more primitives.
  val opKinds: List[Kind] = List(
    Kind("Int", "Int", "Int", "0"),
    Kind("Long", "Long", "Long", "0L"),
    Kind("Double", "Double", "Double", "0.0"),
    Kind("Ref", "Object", "AnyRef", "null")
  )

  private val grow =
    "if (sp + 2 > stack.length) { val ns = stack.length * 2; stack = java.util.Arrays.copyOf(stack, ns); tail = java.util.Arrays.copyOf(tail, ns); isTail = java.util.Arrays.copyOf(isTail, ns) }"

  private def dfsDef(k: Kind): String =
    s"""  inline def dfs${k.name}(root: FBase)(inline onLeaf: (Array[${k.arr}], Int) => Unit)(inline onOne: ${k.arr} => Unit): Unit = {
       |    var stack = new Array[FBase](16); var tail = new Array[${k.arr}](16); var isTail = new Array[Boolean](16)
       |    stack(0) = root; var sp = 1
       |    while (sp > 0) {
       |      sp -= 1
       |      if (isTail(sp)) onOne(tail(sp))
       |      else stack(sp) match {
       |        case leaf: ${k.name}Arr => onLeaf(leaf.arr, leaf.length)
       |        case p: ${k.name}Prepend => { onOne(p.elem); $grow; stack(sp) = p.base; isTail(sp) = false; sp += 1 }
       |        case a: ${k.name}Append => { $grow; tail(sp) = a.elem; isTail(sp) = true; sp += 1; stack(sp) = a.base; isTail(sp) = false; sp += 1 }
       |        case c: Concat => { $grow; stack(sp) = c.right; isTail(sp) = false; sp += 1; stack(sp) = c.left; isTail(sp) = false; sp += 1 }
       |        case _ => ()
       |      }
       |    }
       |  }""".stripMargin

  private def atDef(k: Kind): String =
    s"""  def ${k.lc}At(node: FBase, i: Int): ${k.arr} = node match {
       |    case leaf: ${k.name}Arr => leaf.arr(i)
       |    case c: Concat => if (i < c.left.length) ${k.lc}At(c.left, i) else ${k.lc}At(c.right, i - c.left.length)
       |    case a: ${k.name}Append => if (i < a.base.length) ${k.lc}At(a.base, i) else a.elem
       |    case p: ${k.name}Prepend => if (i == 0) p.elem else ${k.lc}At(p.base, i - 1)
       |    case _ => ${k.dflt}
       |  }""".stripMargin

  private def branches(body: Kind => String): String =
    opKinds.map(k => s"      case _: ${k.pat} => ${body(k)}").mkString("\n")

  private def farrayOps: String = {
    val dfs = opKinds.map(dfsDef).mkString("\n")
    val ats = opKinds.map(atDef).mkString("\n")

    // Reference reads cast the leaf array to Array[A] (checkcast hoisted out of the loop at the
    // concrete inline call site; empty leaves never evaluate it). Primitive arrays can't cast to
    // Array[A] (erases to Object[]), so they read directly.
    def rd(k: Kind): String = if k.name == "Ref" then "a.asInstanceOf[Array[A]](i)" else "a(i).asInstanceOf[A]"
    // Reference arrays are allocated typed via ClassTag (a real String[]) so the typed read succeeds.
    def alloc(k: Kind, n: String, tp: String): String =
      if k.name == "Ref" then s"summonInline[scala.reflect.ClassTag[$tp]].newArray($n).asInstanceOf[Array[Object]]"
      else s"new Array[${k.arr}]($n)"

    val foldLeft = branches(k =>
      s"dfs${k.name}(xs)((a, n) => { var i = 0; while (i < n) { acc = op(acc, ${rd(k)}); i += 1 } })(v => { acc = op(acc, v.asInstanceOf[A]) })")
    val foreach = branches(k =>
      s"dfs${k.name}(xs)((a, n) => { var i = 0; while (i < n) { f(${rd(k)}); i += 1 } })(v => { f(v.asInstanceOf[A]) })")
    val mapM = opKinds.map { ka =>
      val inner = opKinds.map { kb =>
        s"          case _: ${kb.pat} => { val out = ${alloc(kb, "n", "B")}; var o = 0; dfs${ka.name}(xs)((a, len) => { var i = 0; while (i < len) { out(o) = f(${rd(ka)}).asInstanceOf[${kb.arr}]; o += 1; i += 1 } })(v => { out(o) = f(v.asInstanceOf[A]).asInstanceOf[${kb.arr}]; o += 1 }); new ${kb.name}Arr(out, n) }"
      }.mkString("\n")
      s"      case _: ${ka.pat} => inline erasedValue[B] match {\n$inner\n      }"
    }.mkString("\n")
    val filter = branches(k =>
      s"{ val out = ${alloc(k, "n", "A")}; var o = 0; dfs${k.name}(xs)((a, len) => { var i = 0; while (i < len) { val e = ${rd(k)}; if (p(e)) { out(o) = e.asInstanceOf[${k.arr}]; o += 1 }; i += 1 } })(v => { val e = v.asInstanceOf[A]; if (p(e)) { out(o) = e.asInstanceOf[${k.arr}]; o += 1 } }); if (o == n) xs else if (o == 0) ${k.name}Arr.EMPTY else new ${k.name}Arr(java.util.Arrays.copyOf(out, o), o) }")
    val contains = branches(k =>
      if k.name == "Ref" then s"dfsRef(xs)((a, n) => { var i = 0; while (i < n) { if (${rd(k)} == elem) found = true; i += 1 } })(v => { if (v == elem) found = true })"
      else s"{ val e = elem.asInstanceOf[${k.arr}]; dfs${k.name}(xs)((a, n) => { var i = 0; while (i < n) { if (a(i) == e) found = true; i += 1 } })(v => { if (v == e) found = true }) }")
    val applyAt = branches(k => s"${k.lc}At(xs, i).asInstanceOf[A]")
    val flatMapEmpty = branches(k => s"${k.name}Arr.EMPTY")
    val flatMapRead = branches(k =>
      s"dfs${k.name}(xs)((a, n) => { var i = 0; while (i < n) { acc = acc.concat(f(${rd(k)})); i += 1 } })(v => { acc = acc.concat(f(v.asInstanceOf[A])) })")
    val updated = branches(k =>
      s"{ val out = ${alloc(k, "n", "A")}; var o = 0; dfs${k.name}(xs)((a, len) => { System.arraycopy(a, 0, out, o, len); o += len })(v => { out(o) = v.asInstanceOf[${k.arr}]; o += 1 }); out(index) = elem.asInstanceOf[${k.arr}]; new ${k.name}Arr(out, n) }")
    val append = branches(k => s"new ${k.name}Append(xs, elem.asInstanceOf[${k.arr}])")
    val prepend = branches(k => s"new ${k.name}Prepend(elem.asInstanceOf[${k.arr}], xs)")
    val emptyB = branches(k => s"${k.name}Arr.EMPTY")
    val tabulate = branches(k =>
      s"if (n <= 0) ${k.name}Arr.EMPTY else { val out = ${alloc(k, "n", "A")}; var i = 0; while (i < n) { out(i) = f(i).asInstanceOf[${k.arr}]; i += 1 }; new ${k.name}Arr(out, n) }")
    val applyVar = branches(k =>
      s"if (as.isEmpty) ${k.name}Arr.EMPTY else { val n = as.length; val out = ${alloc(k, "n", "A")}; var i = 0; while (i < n) { out(i) = as(i).asInstanceOf[${k.arr}]; i += 1 }; new ${k.name}Arr(out, n) }")
    val fromArr = branches(k => s"new ${k.name}Arr(as.asInstanceOf[Array[${k.arr}]], as.length)")

    s"""package farray
       |
       |import scala.compiletime.{erasedValue, summonInline}
       |
       |// GENERATED by GenCores — do not edit. Per-kind specialized combinator implementations.
       |object FArrayOps {
       |$dfs
       |$ats
       |
       |  inline def emptyImpl[A]: FBase = inline erasedValue[A] match {
       |$emptyB
       |  }
       |  inline def applyImpl[A](as: Seq[A]): FBase = inline erasedValue[A] match {
       |$applyVar
       |  }
       |  inline def tabulateImpl[A](n: Int)(inline f: Int => A): FBase = inline erasedValue[A] match {
       |$tabulate
       |  }
       |  inline def fromArrayImpl[A](as: Array[A]): FBase = inline erasedValue[A] match {
       |$fromArr
       |  }
       |  inline def foldLeftImpl[A, Z](xs: FBase, z: Z)(inline op: (Z, A) => Z): Z = {
       |    var acc = z
       |    inline erasedValue[A] match {
       |$foldLeft
       |    }
       |    acc
       |  }
       |  inline def foreachImpl[A](xs: FBase)(inline f: A => Unit): Unit = inline erasedValue[A] match {
       |$foreach
       |  }
       |  inline def mapImpl[A, B](xs: FBase)(inline f: A => B): FBase = {
       |    val n = xs.length
       |    inline erasedValue[A] match {
       |$mapM
       |    }
       |  }
       |  inline def filterImpl[A](xs: FBase)(inline p: A => Boolean): FBase = {
       |    val n = xs.length
       |    inline erasedValue[A] match {
       |$filter
       |    }
       |  }
       |  inline def containsImpl[A](xs: FBase, elem: A): Boolean = {
       |    var found = false
       |    inline erasedValue[A] match {
       |$contains
       |    }
       |    found
       |  }
       |  inline def applyAtImpl[A](xs: FBase, i: Int): A = inline erasedValue[A] match {
       |$applyAt
       |  }
       |  inline def flatMapImpl[A, B](xs: FBase)(inline f: A => FBase): FBase = {
       |    var acc: FBase = inline erasedValue[B] match {
       |$flatMapEmpty
       |    }
       |    inline erasedValue[A] match {
       |$flatMapRead
       |    }
       |    acc
       |  }
       |  inline def updatedImpl[A, B](xs: FBase, index: Int, elem: B): FBase = {
       |    val n = xs.length
       |    inline erasedValue[A] match {
       |$updated
       |    }
       |  }
       |  inline def appendImpl[A, B](xs: FBase, elem: B): FBase = inline erasedValue[B] match {
       |$append
       |  }
       |  inline def prependImpl[A](elem: A, xs: FBase): FBase = inline erasedValue[A] match {
       |$prepend
       |  }
       |}
       |""".stripMargin
  }

  private def fbase: String = {
    val leaves = prims.flatMap(p => List(s"${p.name}Arr", s"${p.name}Append", s"${p.name}Prepend"))
    val permits = (leaves ++ List("RefArr", "RefAppend", "RefPrepend", "Concat")).mkString(", ")
    s"""package farray;
       |
       |// GENERATED by GenCores — do not edit.
       |public sealed abstract class FBase permits $permits {
       |    public final int length;
       |    protected FBase(int length) { this.length = length; }
       |
       |    public abstract Object applyBoxed(int i);
       |    public abstract FBase take(int n);
       |    public abstract FBase drop(int n);
       |    public abstract FBase slice(int from, int until);
       |    public abstract FBase reverse();
       |    public abstract FBase init();
       |
       |    /** O(1) concat: builds a Concat tree node; never casts, so heterogeneous kinds are safe. */
       |    public final FBase concat(FBase that) {
       |        if (that.length == 0) return this;
       |        if (this.length == 0) return that;
       |        return new Concat(this, that);
       |    }
       |}
       |""".stripMargin
  }

  private def primCore(p: Prim): String = {
    val cls = s"${p.name}Arr"
    s"""package farray;
       |
       |import java.util.Arrays;
       |
       |// GENERATED by GenCores — do not edit. ${p.jt}[]-backed specialized leaf.
       |public final class $cls extends FBase {
       |    public final ${p.jt}[] arr;
       |    public $cls(${p.jt}[] arr, int length) { super(length); this.arr = arr; }
       |    public static final $cls EMPTY = new $cls(new ${p.jt}[0], 0);
       |
       |    @Override public Object applyBoxed(int i) { return ${p.boxed}.valueOf(arr[i]); }
       |
       |    @Override public FBase take(int n) {
       |        int newLen = n < 0 ? 0 : (n > length ? length : n);
       |        if (newLen == 0) return EMPTY;
       |        if (newLen == length) return this;
       |        return new $cls(Arrays.copyOfRange(arr, 0, newLen), newLen);
       |    }
       |    @Override public FBase drop(int n) {
       |        int d = n < 0 ? 0 : (n > length ? length : n);
       |        int newLen = length - d;
       |        if (newLen == length) return this;
       |        if (newLen == 0) return EMPTY;
       |        return new $cls(Arrays.copyOfRange(arr, d, length), newLen);
       |    }
       |    @Override public FBase slice(int from, int until) {
       |        int lo = from < 0 ? 0 : from;
       |        int hi = until > length ? length : until;
       |        if (lo >= hi) return EMPTY;
       |        if (lo == 0 && hi == length) return this;
       |        return new $cls(Arrays.copyOfRange(arr, lo, hi), hi - lo);
       |    }
       |    @Override public FBase reverse() {
       |        if (length < 2) return this;
       |        ${p.jt}[] out = new ${p.jt}[length];
       |        for (int i = 0; i < length; i++) out[i] = arr[length - 1 - i];
       |        return new $cls(out, length);
       |    }
       |    @Override public FBase init() { return new $cls(arr, length - 1); }
       |
       |    @Override public int hashCode() {
       |        int result = 1;
       |        for (int i = 0; i < length; i++) result = 31 * result + ${p.boxed}.hashCode(arr[i]);
       |        return result;
       |    }
       |    @Override public boolean equals(Object obj) {
       |        if (this == obj) return true;
       |        if (!(obj instanceof $cls)) return Concat.elementwiseEquals(this, obj);
       |        $cls other = ($cls) obj;
       |        if (other.length != length) return false;
       |        for (int i = 0; i < length; i++) if (arr[i] != other.arr[i]) return false;
       |        return true;
       |    }
       |    @Override public String toString() { return Concat.render(this); }
       |}
       |""".stripMargin
  }

  private def refArr: String =
    s"""package farray;
       |
       |import java.util.Arrays;
       |
       |// GENERATED by GenCores — do not edit. Object[]-backed leaf for reference element types.
       |public final class RefArr extends FBase {
       |    public final Object[] arr;
       |    public RefArr(Object[] arr, int length) { super(length); this.arr = arr; }
       |    public static final RefArr EMPTY = new RefArr(new Object[0], 0);
       |
       |    @Override public Object applyBoxed(int i) { return arr[i]; }
       |
       |    @Override public FBase take(int n) {
       |        int newLen = n < 0 ? 0 : (n > length ? length : n);
       |        if (newLen == 0) return EMPTY;
       |        if (newLen == length) return this;
       |        return new RefArr(Arrays.copyOfRange(arr, 0, newLen), newLen);
       |    }
       |    @Override public FBase drop(int n) {
       |        int d = n < 0 ? 0 : (n > length ? length : n);
       |        int newLen = length - d;
       |        if (newLen == length) return this;
       |        if (newLen == 0) return EMPTY;
       |        return new RefArr(Arrays.copyOfRange(arr, d, length), newLen);
       |    }
       |    @Override public FBase slice(int from, int until) {
       |        int lo = from < 0 ? 0 : from;
       |        int hi = until > length ? length : until;
       |        if (lo >= hi) return EMPTY;
       |        if (lo == 0 && hi == length) return this;
       |        return new RefArr(Arrays.copyOfRange(arr, lo, hi), hi - lo);
       |    }
       |    @Override public FBase reverse() {
       |        if (length < 2) return this;
       |        Object[] out = (Object[]) java.lang.reflect.Array.newInstance(arr.getClass().getComponentType(), length);
       |        for (int i = 0; i < length; i++) out[i] = arr[length - 1 - i];
       |        return new RefArr(out, length);
       |    }
       |    @Override public FBase init() { return new RefArr(arr, length - 1); }
       |
       |    @Override public int hashCode() {
       |        int result = 1;
       |        for (int i = 0; i < length; i++) { Object e = arr[i]; result = 31 * result + (e == null ? 0 : e.hashCode()); }
       |        return result;
       |    }
       |    @Override public boolean equals(Object obj) {
       |        if (this == obj) return true;
       |        if (!(obj instanceof RefArr)) return Concat.elementwiseEquals(this, obj);
       |        RefArr other = (RefArr) obj;
       |        if (other.length != length) return false;
       |        for (int i = 0; i < length; i++) {
       |            Object x = arr[i], y = other.arr[i];
       |            if (x == null ? y != null : !x.equals(y)) return false;
       |        }
       |        return true;
       |    }
       |    @Override public String toString() { return Concat.render(this); }
       |}
       |""".stripMargin

  private def concat: String =
    s"""package farray;
       |
       |// GENERATED by GenCores — do not edit. Tree node: O(1) concat, structural ops recurse (Java
       |// recursion is fine here — only the inline Scala combinators must avoid recursion to keep the
       |// element op spliced). Element traversal uses an explicit stack in the Scala combinators.
       |public final class Concat extends FBase {
       |    public final FBase left;
       |    public final FBase right;
       |    public Concat(FBase left, FBase right) {
       |        super(left.length + right.length);
       |        this.left = left;
       |        this.right = right;
       |    }
       |
       |    @Override public Object applyBoxed(int i) {
       |        return i < left.length ? left.applyBoxed(i) : right.applyBoxed(i - left.length);
       |    }
       |    @Override public FBase take(int n) {
       |        if (n <= 0) return left.take(0);
       |        if (n >= length) return this;
       |        if (n <= left.length) return left.take(n);
       |        return new Concat(left, right.take(n - left.length));
       |    }
       |    @Override public FBase drop(int n) {
       |        if (n <= 0) return this;
       |        if (n >= length) return right.drop(right.length);
       |        if (n >= left.length) return right.drop(n - left.length);
       |        return new Concat(left.drop(n), right);
       |    }
       |    @Override public FBase slice(int from, int until) {
       |        int lo = from < 0 ? 0 : from;
       |        int hi = until > length ? length : until;
       |        if (lo >= hi) return left.take(0);
       |        return drop(lo).take(hi - lo);
       |    }
       |    @Override public FBase reverse() { return new Concat(right.reverse(), left.reverse()); }
       |    @Override public FBase init() { return take(length - 1); }

       |
       |    @Override public int hashCode() {
       |        int result = 1;
       |        for (int i = 0; i < length; i++) { Object e = applyBoxed(i); result = 31 * result + (e == null ? 0 : e.hashCode()); }
       |        return result;
       |    }
       |    @Override public boolean equals(Object obj) { return elementwiseEquals(this, obj); }
       |    @Override public String toString() { return render(this); }
       |
       |    /** Shared element-wise equality across any two cores (used when concrete kinds differ). */
       |    static boolean elementwiseEquals(FBase self, Object obj) {
       |        if (self == obj) return true;
       |        if (!(obj instanceof FBase)) return false;
       |        FBase other = (FBase) obj;
       |        if (other.length != self.length) return false;
       |        for (int i = 0; i < self.length; i++) {
       |            Object x = self.applyBoxed(i), y = other.applyBoxed(i);
       |            if (x == null ? y != null : !x.equals(y)) return false;
       |        }
       |        return true;
       |    }
       |    static String render(FBase self) {
       |        StringBuilder sb = new StringBuilder("FArray(");
       |        for (int i = 0; i < self.length; i++) { if (i != 0) sb.append(", "); sb.append(self.applyBoxed(i)); }
       |        return sb.append(')').toString();
       |    }
       |}
       |""".stripMargin

  private def primAppend(p: Prim): String = {
    val cls = s"${p.name}Append"
    s"""package farray;
       |
       |// GENERATED by GenCores — do not edit. Unary append node (base + one ${p.jt}, base shared).
       |public final class $cls extends FBase {
       |    public final FBase base;
       |    public final ${p.jt} elem;
       |    public $cls(FBase base, ${p.jt} elem) { super(base.length + 1); this.base = base; this.elem = elem; }
       |    @Override public Object applyBoxed(int i) { return i < base.length ? base.applyBoxed(i) : ${p.boxed}.valueOf(elem); }
       |    @Override public FBase take(int n) { return n >= length ? this : base.take(n); }
       |    @Override public FBase drop(int n) {
       |        if (n <= 0) return this;
       |        if (n >= length) return base.take(0);
       |        return new $cls(base.drop(n), elem);
       |    }
       |    @Override public FBase slice(int from, int until) {
       |        int lo = from < 0 ? 0 : from; int hi = until > length ? length : until;
       |        if (lo >= hi) return base.take(0);
       |        return drop(lo).take(hi - lo);
       |    }
       |    @Override public FBase reverse() { return new ${p.name}Prepend(elem, base.reverse()); }
       |    @Override public FBase init() { return base; }
       |    @Override public int hashCode() {
       |        int result = 1;
       |        for (int i = 0; i < length; i++) { Object e = applyBoxed(i); result = 31 * result + (e == null ? 0 : e.hashCode()); }
       |        return result;
       |    }
       |    @Override public boolean equals(Object obj) { return Concat.elementwiseEquals(this, obj); }
       |    @Override public String toString() { return Concat.render(this); }
       |}
       |""".stripMargin
  }

  private def primPrepend(p: Prim): String = {
    val cls = s"${p.name}Prepend"
    s"""package farray;
       |
       |// GENERATED by GenCores — do not edit. Unary prepend node (one ${p.jt} + base, base shared).
       |public final class $cls extends FBase {
       |    public final ${p.jt} elem;
       |    public final FBase base;
       |    public $cls(${p.jt} elem, FBase base) { super(base.length + 1); this.elem = elem; this.base = base; }
       |    @Override public Object applyBoxed(int i) { return i == 0 ? ${p.boxed}.valueOf(elem) : base.applyBoxed(i - 1); }
       |    @Override public FBase take(int n) {
       |        if (n <= 0) return base.take(0);
       |        if (n >= length) return this;
       |        return new $cls(elem, base.take(n - 1));
       |    }
       |    @Override public FBase drop(int n) {
       |        if (n <= 0) return this;
       |        if (n >= length) return base.take(0);
       |        return base.drop(n - 1);
       |    }
       |    @Override public FBase slice(int from, int until) {
       |        int lo = from < 0 ? 0 : from; int hi = until > length ? length : until;
       |        if (lo >= hi) return base.take(0);
       |        return drop(lo).take(hi - lo);
       |    }
       |    @Override public FBase reverse() { return new ${p.name}Append(base.reverse(), elem); }
       |    @Override public FBase init() { return take(length - 1); }
       |    @Override public int hashCode() {
       |        int result = 1;
       |        for (int i = 0; i < length; i++) { Object e = applyBoxed(i); result = 31 * result + (e == null ? 0 : e.hashCode()); }
       |        return result;
       |    }
       |    @Override public boolean equals(Object obj) { return Concat.elementwiseEquals(this, obj); }
       |    @Override public String toString() { return Concat.render(this); }
       |}
       |""".stripMargin
  }

  private def refUnary(kind: String): String = {
    val cls = s"Ref$kind"
    val isAppend = kind == "Append"
    val fields = if isAppend then "public final FBase base;\n    public final Object elem;"
                 else "public final Object elem;\n    public final FBase base;"
    val ctorArgs = if isAppend then "FBase base, Object elem" else "Object elem, FBase base"
    val ctorBody = if isAppend then "this.base = base; this.elem = elem;" else "this.elem = elem; this.base = base;"
    val applyB = if isAppend then "i < base.length ? base.applyBoxed(i) : elem" else "i == 0 ? elem : base.applyBoxed(i - 1)"
    val take = if isAppend then "return n >= length ? this : base.take(n);"
               else "if (n <= 0) return base.take(0); if (n >= length) return this; return new RefPrepend(elem, base.take(n - 1));"
    val drop = if isAppend then "if (n <= 0) return this; if (n >= length) return base.take(0); return new RefAppend(base.drop(n), elem);"
               else "if (n <= 0) return this; if (n >= length) return base.take(0); return base.drop(n - 1);"
    val reverse = if isAppend then "return new RefPrepend(elem, base.reverse());" else "return new RefAppend(base.reverse(), elem);"
    val init = if isAppend then "return base;" else "return take(length - 1);"
    s"""package farray;
       |
       |// GENERATED by GenCores — do not edit. Unary $kind node for reference elements (base shared).
       |public final class $cls extends FBase {
       |    $fields
       |    public $cls($ctorArgs) { super(base.length + 1); $ctorBody }
       |    @Override public Object applyBoxed(int i) { return $applyB; }
       |    @Override public FBase take(int n) { $take }
       |    @Override public FBase drop(int n) { $drop }
       |    @Override public FBase slice(int from, int until) {
       |        int lo = from < 0 ? 0 : from; int hi = until > length ? length : until;
       |        if (lo >= hi) return base.take(0);
       |        return drop(lo).take(hi - lo);
       |    }
       |    @Override public FBase reverse() { $reverse }
       |    @Override public FBase init() { $init }
       |    @Override public int hashCode() {
       |        int result = 1;
       |        for (int i = 0; i < length; i++) { Object e = applyBoxed(i); result = 31 * result + (e == null ? 0 : e.hashCode()); }
       |        return result;
       |    }
       |    @Override public boolean equals(Object obj) { return Concat.elementwiseEquals(this, obj); }
       |    @Override public String toString() { return Concat.render(this); }
       |}
       |""".stripMargin
  }
}
