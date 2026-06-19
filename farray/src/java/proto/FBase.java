package proto;

/** Proof-of-concept sealed-ish base for per-primitive specialization. */
public abstract class FBase {
    public final int length;
    protected FBase(int length) { this.length = length; }
    /** Boxing fallback used on the abstract/AnyRef access path (covariant up-casts). */
    public abstract Object applyBoxed(int i);
}
