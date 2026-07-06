package farray.json

import farray.{Framed, FramedByteSource}

// start:ndjson-frame
/** The NDJSON framer, in its entirety: with the carry/stitch machinery owned by [[farray.FramedByteSource]], a format's whole job is one function from a byte
  * window to a [[farray.Framed]] verdict. For NDJSON that is a newline scan; the trailing record at end of stream is complete even without its `\n`.
  */
private[json] class Framer(read: (Array[Byte], Int, Int) => Int, blockSize: Int, doClose: () => Unit) extends FramedByteSource(read, blockSize, doClose):

  protected def frame(buf: Array[Byte], from: Int, limit: Int, atEof: Boolean): Framed =
    if from >= limit then (if atEof then Framed.End else Framed.NeedMore)
    else
      var e = from
      while e < limit && buf(e) != '\n' do e += 1
      if e < limit then Framed.Record(from, e) // found '\n' → complete record
      else if atEof then Framed.Record(from, limit) // last record, no trailing '\n' → still complete
      else Framed.Partial(from) // ran off the end with bytes pending → unfinished record
// stop:ndjson-frame
