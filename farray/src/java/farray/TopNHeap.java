package farray;

import java.util.Comparator;

/**
 * Bounded heaps that keep the top-N elements by a key, in ONE streaming pass — O(N log n) time, O(n) memory,
 * for the fused `topN`/`Agg.topNBy`/`largest`/`smallest` terminals. Written in Java for the same bytecode-control
 * reason as the FArray core / JsonNum: the per-element sift is the hot path.
 *
 * <p>For "largest n" we keep a MIN-heap of size n keyed by the comparison value: the root is the smallest of the
 * current top-n, so a new element is admitted iff it beats the root (then the root is evicted and the new element
 * sifted down). "smallest n" reverses the comparison. Element and key are stored in parallel arrays so the key
 * isn't recomputed; materialization sorts into best-first (descending-key) order.
 *
 * <p>This generic variant boxes the key (it holds an {@code Object[]} of keys and a {@code Comparator}); it is used
 * only when the key is a REFERENCE type. Primitive keys (Int/Long/Double — the common `topNBy(_.numericField)`)
 * use the unboxed sibling heaps {@link IntTopNHeap} / {@link LongTopNHeap} / {@link DoubleTopNHeap}, whose key
 * arrays are primitive and whose comparison is a direct {@code <}/{@code >} (no per-element boxing).
 */
public final class TopNHeap {
    private final Object[] elems;
    private final Object[] keys;
    private final Comparator<Object> cmp; // worst (first-evicted) key == minimum
    private final int cap;
    private int size;

    @SuppressWarnings("unchecked")
    public TopNHeap(int n, Comparator<?> cmp) {
        this.cap = Math.max(1, n);
        this.elems = new Object[cap];
        this.keys = new Object[cap];
        this.cmp = (Comparator<Object>) cmp;
        this.size = 0;
    }

    public void offer(Object elem, Object key) {
        if (size < cap) {
            elems[size] = elem; keys[size] = key; size++;
            int i = size - 1;
            while (i > 0) { int p = (i - 1) >>> 1; if (cmp.compare(keys[i], keys[p]) >= 0) break; swap(i, p); i = p; }
        } else if (cmp.compare(key, keys[0]) > 0) {
            elems[0] = elem; keys[0] = key; siftDown(0);
        }
    }

    private void siftDown(int i) {
        for (;;) {
            int l = 2 * i + 1, r = l + 1, m = i;
            if (l < size && cmp.compare(keys[l], keys[m]) < 0) m = l;
            if (r < size && cmp.compare(keys[r], keys[m]) < 0) m = r;
            if (m == i) break;
            swap(i, m); i = m;
        }
    }

    private void swap(int a, int b) {
        Object te = elems[a]; elems[a] = elems[b]; elems[b] = te;
        Object tk = keys[a]; keys[a] = keys[b]; keys[b] = tk;
    }

    /** retained elements, best-first (descending key). Drains the heap. */
    public Object[] toSortedArray() {
        int n = size; Object[] out = new Object[n];
        for (int k = 0; k < n; k++) {
            out[n - 1 - k] = elems[0]; size--;
            if (size > 0) { elems[0] = elems[size]; keys[0] = keys[size]; siftDown(0); }
        }
        return out;
    }
}
