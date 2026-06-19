package proto;

/** Object[]-backed core for reference element types. */
public final class RefArr extends FBase {
    public final Object[] a;
    public RefArr(Object[] a, int length) { super(length); this.a = a; }
    @Override public Object applyBoxed(int i) { return a[i]; }
}
