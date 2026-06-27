package farray.json;

/**
 * Byte-level number parsing for the fused JSON scanner — written in Java for tight bytecode control
 * over a hot, branch-heavy primitive operating directly on {@code byte[]} (the same reasoning the
 * FArray core is Java: the JIT-visible, allocation-free, no-megamorphism path matters here most).
 *
 * <p>The critical property: the common path allocates <b>nothing</b> (no {@code new String}, no
 * boxing). The v0a Scala scanner's {@code Double.parseDouble(new String(...))} allocated ~100
 * bytes/record, which alone blew the constant-memory thesis; this replaces it.
 *
 * <p>Double parsing is tiered like jsoniter-scala (which credits rust-lexical / Eisel-Lemire):
 *
 * <ol>
 *   <li>integral mantissa, no fraction/exponent → {@code (double) mantissa};
 *   <li>mantissa fits in 2^52 and the decimal exponent is in the exactly-representable window → a
 *       single multiply/divide by an exact power of ten from {@link #POW10} (provably correctly
 *       rounded);
 *   <li>otherwise fall back to {@link Double#parseDouble} on a freshly-allocated substring (rare;
 *       the only allocating path). The Eisel-Lemire 128-bit-multiply moderate path is a later,
 *       fuzz-gated addition — see docs/json-parser-research.md §2.1.
 * </ol>
 *
 * <p>Result + new position are returned packed: the parser writes the value into a caller-provided
 * holder and returns the new {@code pos}. (Avoids a tuple/array allocation per call.)
 */
public final class JsonNum {
  private JsonNum() {}

  /** exact powers of ten representable as a double, 10^0 .. 10^22 (10^22 is the last exact one). */
  private static final double[] POW10 = new double[23];

  static {
    double p = 1.0;
    for (int i = 0; i <= 22; i++) {
      POW10[i] = p;
      p *= 10.0;
    }
  }

  /**
   * holder for a parsed double + the position just past it (single reused instance per scanner).
   */
  public static final class D {
    public double value;
    public int pos;
  }

  /**
   * Parse a JSON number at {@code from} (which must point at the first byte of the number, after
   * any whitespace) into {@code out}; returns nothing (out.pos is set). No allocation on tiers 1-2.
   */
  public static void parseDouble(byte[] buf, int from, int end, D out) {
    int pos = from;
    boolean neg = false;
    if (pos < end && buf[pos] == '-') {
      neg = true;
      pos++;
    }

    // accumulate mantissa as a long, counting fractional digits.
    long m = 0L;
    int mantDigits = 0;
    int fracDigits = 0;
    boolean truncated = false;

    // integer part
    while (pos < end) {
      int c = buf[pos];
      if (c >= '0' && c <= '9') {
        if (mantDigits < 18) {
          m = m * 10 + (c - '0');
          mantDigits++;
        } else truncated = true; // too many digits for the long fast path
        pos++;
      } else break;
    }
    // fraction
    if (pos < end && buf[pos] == '.') {
      pos++;
      while (pos < end) {
        int c = buf[pos];
        if (c >= '0' && c <= '9') {
          if (mantDigits < 18) {
            m = m * 10 + (c - '0');
            mantDigits++;
            fracDigits++;
          } else truncated = true;
          pos++;
        } else break;
      }
    }
    // exponent
    int exp = 0;
    boolean hasExp = false;
    if (pos < end && (buf[pos] == 'e' || buf[pos] == 'E')) {
      hasExp = true;
      pos++;
      boolean eneg = false;
      if (pos < end && (buf[pos] == '+' || buf[pos] == '-')) {
        eneg = buf[pos] == '-';
        pos++;
      }
      int e = 0;
      while (pos < end) {
        int c = buf[pos];
        if (c >= '0' && c <= '9') {
          e = e * 10 + (c - '0');
          pos++;
        } else break;
      }
      exp = eneg ? -e : e;
    }

    out.pos = pos;

    // net base-10 exponent applied to the integer mantissa m
    int e10 = exp - fracDigits;

    double result;
    if (truncated) {
      // slow, correct fallback (rare): only the genuinely-long inputs allocate.
      result =
          Double.parseDouble(
              new String(buf, from, pos - from, java.nio.charset.StandardCharsets.ISO_8859_1));
    } else if (e10 == 0) {
      result = (double) m; // tier 1: integral
    } else if (e10 > 0 && e10 <= 22 && m < (1L << 52)) {
      result = m * POW10[e10]; // tier 2: single exact multiply
    } else if (e10 < 0 && e10 >= -22 && m < (1L << 52)) {
      result = m / POW10[-e10]; // tier 2: single exact divide
    } else {
      // out of the exact window — fall back for correctness.
      result =
          Double.parseDouble(
              new String(buf, from, pos - from, java.nio.charset.StandardCharsets.ISO_8859_1));
    }
    out.value = neg ? -result : result;
  }
}
