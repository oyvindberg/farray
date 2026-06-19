package proto;

/** Binary concat node (can nest) — the case that forces explicit-stack DFS instead of recursion. */
public final class Concat extends FBase {
    public final FBase left;
    public final FBase right;
    public Concat(FBase left, FBase right) {
        super(left.length + right.length);
        this.left = left;
        this.right = right;
    }
    @Override public Object applyBoxed(int i) {
        return i < left.length ? left.applyBoxed(i) : right.applyBoxed(i - left.length);
    }
}
