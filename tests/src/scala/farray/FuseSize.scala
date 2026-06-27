package farray

/** A long fused chain in ONE method. The whole pipeline inlines into `longChain`'s body; for the JVM JIT to compile it, that body must stay under the
  * HugeMethodLimit (~8000 bytecodes, -XX:-DontCompileHugeMethods). The fused macro emits one loop nest with small inlined stage bodies (the heavy traversal is
  * the plain indexed while-loop, never a duplicated dfs), so this should stay well under the limit. Measured by javap in the no-blowup test below.
  */
object FuseSize:
  def longChain(xs: FArray[Int]): List[Int] =
    xs.fuse
      .map(_ + 1)
      .filter(_ % 2 == 0)
      .map(_ * 3)
      .filter(_ > 0)
      .drop(1)
      .map(_ - 1)
      .take(50)
      .filter(_ != 17)
      .map(_ + 100)
      .filter(_ < 1000)
      .map(_ * 2)
      .filter(_ % 4 == 0)
      .map(_ / 2)
      .run
      .toList

  /** the JVM Code-attribute length (bytecode size) of method `name` in this module's class, read straight from the .class file — used by the no-blowup test to
    * assert a long fused chain stays under HugeMethodLimit.
    */
  def codeSizeOf(name: String): Int =
    val in = new java.io.DataInputStream(getClass.getResourceAsStream("FuseSize$.class"))
    try
      in.readInt(); in.readUnsignedShort(); in.readUnsignedShort() // magic, minor, major
      val cpCount = in.readUnsignedShort()
      val utf8 = new Array[String](cpCount)
      var i = 1
      while i < cpCount do
        in.readUnsignedByte() match
          case 1                                  => utf8(i) = in.readUTF()
          case 7 | 8 | 16 | 19 | 20               => in.skipBytes(2)
          case 15                                 => in.skipBytes(3)
          case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 => in.skipBytes(4)
          case 5 | 6                              => in.skipBytes(8); i += 1 // long/double take two cp slots
          case other                              => throw new RuntimeException(s"bad cp tag $other")
        i += 1
      in.skipBytes(6) // access_flags, this_class, super_class
      in.skipBytes(in.readUnsignedShort() * 2) // interfaces
      def skipAttrs(): Unit =
        var a = in.readUnsignedShort(); while a > 0 do { in.skipBytes(2); in.skipBytes(in.readInt()); a -= 1 }
      var f = in.readUnsignedShort(); while f > 0 do { in.skipBytes(6); skipAttrs(); f -= 1 } // fields
      var m = in.readUnsignedShort(); var size = -1
      while m > 0 do
        in.readUnsignedShort(); val nameIdx = in.readUnsignedShort(); in.readUnsignedShort()
        var a = in.readUnsignedShort()
        while a > 0 do
          val attrName = utf8(in.readUnsignedShort()); val attrLen = in.readInt()
          if attrName == "Code" && utf8(nameIdx) == name then
            in.skipBytes(4); size = in.readInt(); in.skipBytes(attrLen - 8) // max_stack+max_locals, code_length, rest
          else in.skipBytes(attrLen)
          a -= 1
        m -= 1
      size
    finally in.close()
