package proto;

/** int[]-backed specialized core: no boxing, no per-element checkcast on the typed path. */
public final class IntArr extends FBase {
    public final int[] a;
    public IntArr(int[] a, int length) { super(length); this.a = a; }
    @Override public Object applyBoxed(int i) { return Integer.valueOf(a[i]); }
}
