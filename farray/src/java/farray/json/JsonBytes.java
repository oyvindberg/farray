package farray.json;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;

/**
 * SWAR (SIMD-within-a-register) byte-level scanning primitives — the hot inner loops of the fused JSON
 * scanner, in Java for bytecode control. Lifts jsoniter-scala / borer's 8-byte-at-a-time tricks: read a
 * {@code long} of input, test all 8 bytes at once for a target byte with the classic
 * "has-a-zero-byte" bit trick, and {@code numberOfTrailingZeros >> 3} to find which byte matched.
 *
 * <p>These replace the scalar byte-at-a-time {@code scanStringEnd}/{@code skipString} that the v0a stack
 * profile showed dominating (skipWs 26%, scanStringEnd 12%). The whole-document skip of unwanted fields is
 * the bulk of a projection query's work, so this is the lever for the throughput target.
 */
public final class JsonBytes {
    private JsonBytes() {}

    private static final VarHandle VH_LONG =
            MethodHandles.byteArrayViewVarHandle(long[].class, ByteOrder.LITTLE_ENDIAN);

    /** broadcast a byte to all 8 lanes of a long. */
    private static long broadcast(byte b) {
        return (b & 0xFFL) * 0x0101010101010101L;
    }

    private static final long QUOTE = broadcast((byte) '"');
    private static final long BACKSLASH = broadcast((byte) '\\');
    private static final long HI = 0x8080808080808080L;
    private static final long LO = 0x0101010101010101L;

    /** classic SWAR "find a zero byte": returns a word with the high bit set in each lane that is zero. */
    private static long zeroByte(long x) {
        return (x - LO) & ~x & HI;
    }

    /**
     * Scan a string body to its closing UNESCAPED quote. {@code start} points just after the opening quote.
     * Returns the index of the closing quote. Handles escapes by falling to a scalar step whenever a
     * backslash appears in the 8-byte window (rare for typical data).
     */
    public static int scanStringEnd(byte[] buf, int start, int end) {
        int pos = start;
        int limit = end - 7;
        while (pos < limit) {
            long w = (long) VH_LONG.get(buf, pos);
            long q = zeroByte(w ^ QUOTE);      // quote lanes
            long bs = zeroByte(w ^ BACKSLASH); // backslash lanes
            if ((q | bs) != 0) {
                int qi = (q != 0) ? (Long.numberOfTrailingZeros(q) >> 3) : 8;
                int bi = (bs != 0) ? (Long.numberOfTrailingZeros(bs) >> 3) : 8;
                if (qi < bi) {
                    return pos + qi; // closing quote before any backslash in this window
                }
                // a backslash comes first: skip the escaped byte scalar-wise, then resume SWAR
                pos = scanScalarUntilQuoteOrBackslashConsumed(buf, pos, bi, end);
                limit = end - 7;
            } else {
                pos += 8;
            }
        }
        // tail
        while (pos < end) {
            byte b = buf[pos];
            if (b == '"') return pos;
            if (b == '\\') pos += 2;
            else pos++;
        }
        throw new JsonParseException("unterminated string from " + start);
    }

    /** advance to position of first backslash (bi bytes into the window), consume it + escaped byte. */
    private static int scanScalarUntilQuoteOrBackslashConsumed(byte[] buf, int windowStart, int bi, int end) {
        int pos = windowStart + bi; // at the backslash
        // pos is a backslash: skip it and the escaped char. A unicode escape is 6 chars, but +2 is safe to
        // re-enter the loop, which re-finds the next structural byte; the 4 hex digits are ordinary bytes
        // that won't be mistaken for a quote/backslash unless they actually are escapes.
        return pos + 2;
    }

    /** Skip a string VALUE (pos at opening quote): returns position just past the closing quote. */
    public static int skipString(byte[] buf, int pos, int end) {
        return scanStringEnd(buf, pos + 1, end) + 1;
    }
}
