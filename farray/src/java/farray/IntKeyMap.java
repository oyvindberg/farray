package farray;

import java.util.function.BiConsumer;

/**
 * Open-addressing hash map with an UNBOXED {@code int} key, for the fused
 * `groupReduceBy`/`groupCount`/ `groupSum` terminals — written in Java for the same reason the
 * FArray core and the JSON scanner are: the per-element probe loop is a hot, branch-heavy primitive
 * and we want no {@code Integer.valueOf} per element.
 *
 * <p>Values are stored either in a primitive {@code long[]} (for Int/Long/Double accumulators — Int
 * widened, Long direct, Double via {@code doubleToRawLongBits}) or an {@code Object[]} (reference
 * accumulators), chosen at construction. The macro picks the right pair of accessors.
 *
 * <p>Hot-path contract (how the macro uses it, so no closure/SAM is ever stored here — `reduce`
 * stays inlined at the call site):
 *
 * <pre>
 *   int slot = m.probe(key);                       // find-or-insert; may grow+rehash; sets `wasNew`
 *   if (m.wasNew()) m.setValuePrim(slot, v);        // first sight of this key → seed
 *   else            m.setValuePrim(slot, reduce(m.getValuePrim(slot), v));  // reduce INLINED by the macro
 * </pre>
 *
 * The returned slot is valid until the next {@code probe} (growth happens inside {@code probe},
 * before it returns), and the macro uses it immediately — so lazy growth-on-probe is safe.
 */
public final class IntKeyMap {
  private int[] keys;
  private long[] valuesPrim; // used when primitiveValues
  private Object[] valuesRef; // used otherwise
  private boolean[] used;
  private int size;
  private int mask; // capacity - 1 (capacity is a power of two)
  private int growAt; // size threshold for growth
  private final boolean primitiveValues;
  private boolean wasNew;

  public IntKeyMap(int initialCapacity, boolean primitiveValues) {
    int cap = 8;
    while (cap < initialCapacity) cap <<= 1;
    this.primitiveValues = primitiveValues;
    alloc(cap);
  }

  private void alloc(int cap) {
    keys = new int[cap];
    used = new boolean[cap];
    if (primitiveValues) valuesPrim = new long[cap];
    else valuesRef = new Object[cap];
    mask = cap - 1;
    growAt = (cap * 3) / 5; // load factor ~0.6
    size = 0;
  }

  /**
   * find-or-insert {@code key}; returns its slot index. Grows+rehashes first if needed. Sets {@link
   * #wasNew}.
   */
  public int probe(int key) {
    if (size >= growAt) grow();
    int i = mix(key) & mask;
    while (used[i]) {
      if (keys[i] == key) {
        wasNew = false;
        return i;
      }
      i = (i + 1) & mask;
    }
    used[i] = true;
    keys[i] = key;
    size++;
    wasNew = true;
    return i;
  }

  /** whether the most recent {@link #probe} just inserted (first sight of that key). */
  public boolean wasNew() {
    return wasNew;
  }

  public long getValuePrim(int slot) {
    return valuesPrim[slot];
  }

  public void setValuePrim(int slot, long v) {
    valuesPrim[slot] = v;
  }

  public Object getValueRef(int slot) {
    return valuesRef[slot];
  }

  public void setValueRef(int slot, Object v) {
    valuesRef[slot] = v;
  }

  public int size() {
    return size;
  }

  private void grow() {
    int[] ok = keys;
    boolean[] ou = used;
    long[] ovp = valuesPrim;
    Object[] ovr = valuesRef;
    alloc((mask + 1) << 1);
    for (int j = 0; j < ou.length; j++) {
      if (ou[j]) {
        int slot = probe(ok[j]); // wasNew always true during rehash; reinsert
        if (primitiveValues) valuesPrim[slot] = ovp[j];
        else valuesRef[slot] = ovr[j];
      }
    }
  }

  /** fast integer hash mix (avoids clustering for sequential / low-entropy keys). */
  private static int mix(int k) {
    k *= 0x9E3779B1; // Fibonacci hashing
    k ^= k >>> 16;
    return k;
  }

  /** iterate the live (key, long-value) entries — the only place a key is boxed (O(#keys)). */
  public void foreachEntryPrim(java.util.function.ObjLongConsumer<Integer> f) {
    for (int i = 0; i < used.length; i++) if (used[i]) f.accept(keys[i], valuesPrim[i]);
  }

  /** iterate the live (key, ref-value) entries. */
  public void foreachEntryRef(BiConsumer<Integer, Object> f) {
    for (int i = 0; i < used.length; i++) if (used[i]) f.accept(keys[i], valuesRef[i]);
  }
}
