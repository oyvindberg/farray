package farray.json;

/** Thrown on malformed JSON during scanning. */
public final class JsonParseException extends RuntimeException {
    public JsonParseException(String msg) { super(msg); }
}
