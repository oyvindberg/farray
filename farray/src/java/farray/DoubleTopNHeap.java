package farray;

/**
 * Unboxed top-N heap keyed by a {@code double} (for `topNBy(_.doubleField)` / `largest`/`smallest`
 * over Double). Keys live in a primitive {@code double[]}; the per-element comparison is a direct
 * {@code <}/{@code >} — NO boxing per offered element (the generic {@link TopNHeap} boxes the key
 * on every comparison). See that class for the min-heap "largest" scheme; {@code largest=false}
 * reverses the comparison for "smallest n".
 */
public final class DoubleTopNHeap {
  private final Object[] elems;
  private final double[] keys;
  private final int cap;
  private final boolean largest;
  private int size;

  public DoubleTopNHeap(int n, boolean largest) {
    this.cap = Math.max(1, n);
    this.elems = new Object[cap];
    this.keys = new double[cap];
    this.largest = largest;
    this.size = 0;
  }

  /**
   * true iff key `a` is WORSE than `b` (a should sink toward the root, the first-to-evict slot).
   */
  private boolean worse(double a, double b) {
    return largest ? a < b : a > b;
  }

  public void offer(Object elem, double key) {
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
    } else if (worse(keys[0], key)) { // root is worse than the newcomer → replace
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
    double tk = keys[a];
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
