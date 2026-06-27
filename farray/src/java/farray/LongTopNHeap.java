package farray;

/**
 * Unboxed top-N heap keyed by a {@code long}. See {@link DoubleTopNHeap} for the scheme; identical
 * but for a {@code long[]} key array (no per-element boxing).
 */
public final class LongTopNHeap {
  private final Object[] elems;
  private final long[] keys;
  private final int cap;
  private final boolean largest;
  private int size;

  public LongTopNHeap(int n, boolean largest) {
    this.cap = Math.max(1, n);
    this.elems = new Object[cap];
    this.keys = new long[cap];
    this.largest = largest;
    this.size = 0;
  }

  private boolean worse(long a, long b) {
    return largest ? a < b : a > b;
  }

  public void offer(Object elem, long key) {
    if (size < cap) {
      elems[size] = elem;
      keys[size] = key;
      size++;
      int i = size - 1;
      while (i > 0) {
        int p = (i - 1) >>> 1;
        if (!worse(keys[i], keys[p])) break;
        swap(i, p);
        i = p;
      }
    } else if (worse(keys[0], key)) {
      elems[0] = elem;
      keys[0] = key;
      siftDown(0);
    }
  }

  private void siftDown(int i) {
    for (; ; ) {
      int l = 2 * i + 1, r = l + 1, m = i;
      if (l < size && worse(keys[l], keys[m])) m = l;
      if (r < size && worse(keys[r], keys[m])) m = r;
      if (m == i) break;
      swap(i, m);
      i = m;
    }
  }

  private void swap(int a, int b) {
    Object te = elems[a];
    elems[a] = elems[b];
    elems[b] = te;
    long tk = keys[a];
    keys[a] = keys[b];
    keys[b] = tk;
  }

  public Object[] toSortedArray() {
    int n = size;
    Object[] out = new Object[n];
    for (int k = 0; k < n; k++) {
      out[n - 1 - k] = elems[0];
      size--;
      if (size > 0) {
        elems[0] = elems[size];
        keys[0] = keys[size];
        siftDown(0);
      }
    }
    return out;
  }
}
