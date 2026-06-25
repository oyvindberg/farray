package farray;

/**
 * PROTOTYPE (SAM variant) — shared hand-written Java traversal plus a SINGLE-abstract-method per-(I,O)
 * operation, so the op is a real lambda (SAM-convertible) rather than a two-method anonymous object.
 *
 * Difference from the run/one variant: the RUN LOOP now lives in the traverse — it loops over each
 * leaf's backing array calling apply() per element. Single values (One / deferred Append-Prepend) are
 * one apply() call. ReverseNode / Range / Slice-over-non-leaf iterate per element via apply(). So there
 * is exactly ONE abstract method, at the cost of a per-element virtual call (monomorphic at a given
 * call site -> the JIT can inline it; megamorphic across many call sites -> it cannot).
 *
 * Output cursor / accumulator threaded as a Java local. Canonical return (Empty / IntOne / IntArr).
 */
public final class Proto {
    private Proto() {}

    /** map Int -> Int (SAM). */
    public interface IntMapFn { int apply(int v); }

    /** foldLeft Int -> Z (SAM); Z erases to a reference ("Ref"). */
    public interface IntFoldFn<Z> { Z apply(Z acc, int v); }

    /** map Int -> Int. Returns a canonical FBase of length `n`. */
    public static FBase traverseMapIntInt(FBase root, int n, IntMapFn f) {
        int[] out = new int[n];
        int o = 0;

        FBase cur = root;
        FBase[] stack = null;
        int[] tail = null;
        boolean[] isTail = null;
        int sp = 0;

        while (cur != null) {
            FBase cont = null;
            if (cur instanceof IntArr leaf) {
                int[] a = leaf.arr; int e = leaf.length;
                for (int j = 0; j < e; j++) { out[o] = f.apply(a[j]); o++; }
            } else if (cur instanceof IntOne x) {
                out[o] = f.apply(x.elem); o++;
            } else if (cur instanceof IntPrepend pp) {
                out[o] = f.apply(pp.elem); o++;
                cont = pp.base;
            } else if (cur instanceof IntAppend a) {
                if (stack == null) { stack = new FBase[16]; }
                else if (sp == stack.length) { int nl = sp * 2; stack = java.util.Arrays.copyOf(stack, nl); if (isTail != null) { tail = java.util.Arrays.copyOf(tail, nl); isTail = java.util.Arrays.copyOf(isTail, nl); } }
                if (isTail == null) { tail = new int[stack.length]; isTail = new boolean[stack.length]; }
                tail[sp] = a.elem; isTail[sp] = true; sp += 1;
                cont = a.base;
            } else if (cur instanceof Concat c) {
                if (stack == null) { stack = new FBase[16]; }
                else if (sp == stack.length) { int nl = sp * 2; stack = java.util.Arrays.copyOf(stack, nl); if (isTail != null) { tail = java.util.Arrays.copyOf(tail, nl); isTail = java.util.Arrays.copyOf(isTail, nl); } }
                if (isTail != null) isTail[sp] = false;
                stack[sp] = c.right; sp += 1;
                cont = c.left;
            } else if (cur instanceof ReverseNode rev) {
                FBase b = rev.base;
                if (b instanceof IntArr lf) {
                    int[] ba = lf.arr;
                    for (int i = lf.length - 1; i >= 0; i--) { out[o] = f.apply(ba[i]); o++; }
                } else {
                    for (int i = b.length - 1; i >= 0; i--) { out[o] = f.apply(((Integer) b.applyBoxed(i)).intValue()); o++; }
                }
            } else if (cur instanceof SliceNode s) {
                int sn = s.length, so = s.offset;
                if (s.base instanceof IntArr lf) {
                    int[] a = lf.arr;
                    for (int i = 0; i < sn; i++) { out[o] = f.apply(a[so + i]); o++; }
                } else {
                    for (int i = 0; i < sn; i++) { out[o] = f.apply(((Integer) s.base.applyBoxed(so + i)).intValue()); o++; }
                }
            } else if (cur instanceof IntPad pad) {
                int bl = pad.base.length;
                if (pad.base instanceof IntArr lf) {
                    int[] a = lf.arr;
                    for (int i = 0; i < bl; i++) { out[o] = f.apply(a[i]); o++; }
                } else {
                    for (int i = 0; i < bl; i++) { out[o] = f.apply(((Integer) pad.base.applyBoxed(i)).intValue()); o++; }
                }
                int pf = pad.filler;
                for (int pj = bl; pj < pad.length; pj++) { out[o] = f.apply(pf); o++; }
            } else if (cur instanceof IntUpdated u) {
                if (u.base instanceof IntArr lf) {
                    int[] la = lf.arr; int ui = u.index;
                    for (int i = 0; i < ui; i++) { out[o] = f.apply(la[i]); o++; }
                    out[o] = f.apply(u.elem); o++;
                    for (int i = ui + 1; i < u.length; i++) { out[o] = f.apply(la[i]); o++; }
                } else {
                    for (int i = 0; i < u.length; i++) {
                        int v = (i == u.index) ? u.elem : ((Integer) u.base.applyBoxed(i)).intValue();
                        out[o] = f.apply(v); o++;
                    }
                }
            } else if (cur instanceof RangeNode rng) {
                int rn = rng.length;
                for (int ri = 0; ri < rn; ri++) { out[o] = f.apply(rng.start + ri * rng.step); o++; }
            }

            if (cont != null) {
                cur = cont;
            } else {
                cur = null;
                while (sp > 0 && cur == null) {
                    sp -= 1;
                    if (isTail != null && isTail[sp]) { out[o] = f.apply(tail[sp]); o++; }
                    else cur = stack[sp];
                }
            }
        }

        if (o == 0) return Empty.INSTANCE;
        if (o == 1) return new IntOne(out[0]);
        return new IntArr(out, o);
    }

    /** foldLeft Int -> Z. Returns the final accumulator. */
    public static <Z> Z traverseFoldIntRef(FBase root, Z z, IntFoldFn<Z> f) {
        Z acc = z;

        FBase cur = root;
        FBase[] stack = null;
        int[] tail = null;
        boolean[] isTail = null;
        int sp = 0;

        while (cur != null) {
            FBase cont = null;
            if (cur instanceof IntArr leaf) {
                int[] a = leaf.arr; int e = leaf.length;
                for (int j = 0; j < e; j++) acc = f.apply(acc, a[j]);
            } else if (cur instanceof IntOne x) {
                acc = f.apply(acc, x.elem);
            } else if (cur instanceof IntPrepend pp) {
                acc = f.apply(acc, pp.elem);
                cont = pp.base;
            } else if (cur instanceof IntAppend a) {
                if (stack == null) { stack = new FBase[16]; }
                else if (sp == stack.length) { int nl = sp * 2; stack = java.util.Arrays.copyOf(stack, nl); if (isTail != null) { tail = java.util.Arrays.copyOf(tail, nl); isTail = java.util.Arrays.copyOf(isTail, nl); } }
                if (isTail == null) { tail = new int[stack.length]; isTail = new boolean[stack.length]; }
                tail[sp] = a.elem; isTail[sp] = true; sp += 1;
                cont = a.base;
            } else if (cur instanceof Concat c) {
                if (stack == null) { stack = new FBase[16]; }
                else if (sp == stack.length) { int nl = sp * 2; stack = java.util.Arrays.copyOf(stack, nl); if (isTail != null) { tail = java.util.Arrays.copyOf(tail, nl); isTail = java.util.Arrays.copyOf(isTail, nl); } }
                if (isTail != null) isTail[sp] = false;
                stack[sp] = c.right; sp += 1;
                cont = c.left;
            } else if (cur instanceof ReverseNode rev) {
                FBase b = rev.base;
                if (b instanceof IntArr lf) {
                    int[] ba = lf.arr;
                    for (int i = lf.length - 1; i >= 0; i--) acc = f.apply(acc, ba[i]);
                } else {
                    for (int i = b.length - 1; i >= 0; i--) acc = f.apply(acc, ((Integer) b.applyBoxed(i)).intValue());
                }
            } else if (cur instanceof SliceNode s) {
                int sn = s.length, so = s.offset;
                if (s.base instanceof IntArr lf) {
                    int[] a = lf.arr;
                    for (int i = 0; i < sn; i++) acc = f.apply(acc, a[so + i]);
                } else {
                    for (int i = 0; i < sn; i++) acc = f.apply(acc, ((Integer) s.base.applyBoxed(so + i)).intValue());
                }
            } else if (cur instanceof IntPad pad) {
                int bl = pad.base.length;
                if (pad.base instanceof IntArr lf) {
                    int[] a = lf.arr;
                    for (int i = 0; i < bl; i++) acc = f.apply(acc, a[i]);
                } else {
                    for (int i = 0; i < bl; i++) acc = f.apply(acc, ((Integer) pad.base.applyBoxed(i)).intValue());
                }
                int pf = pad.filler;
                for (int pj = bl; pj < pad.length; pj++) acc = f.apply(acc, pf);
            } else if (cur instanceof IntUpdated u) {
                if (u.base instanceof IntArr lf) {
                    int[] la = lf.arr; int ui = u.index;
                    for (int i = 0; i < ui; i++) acc = f.apply(acc, la[i]);
                    acc = f.apply(acc, u.elem);
                    for (int i = ui + 1; i < u.length; i++) acc = f.apply(acc, la[i]);
                } else {
                    for (int i = 0; i < u.length; i++) {
                        int v = (i == u.index) ? u.elem : ((Integer) u.base.applyBoxed(i)).intValue();
                        acc = f.apply(acc, v);
                    }
                }
            } else if (cur instanceof RangeNode rng) {
                int rn = rng.length;
                for (int ri = 0; ri < rn; ri++) acc = f.apply(acc, rng.start + ri * rng.step);
            }

            if (cont != null) {
                cur = cont;
            } else {
                cur = null;
                while (sp > 0 && cur == null) {
                    sp -= 1;
                    if (isTail != null && isTail[sp]) acc = f.apply(acc, tail[sp]);
                    else cur = stack[sp];
                }
            }
        }

        return acc;
    }
}
