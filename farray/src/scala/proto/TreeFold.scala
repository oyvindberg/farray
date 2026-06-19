package proto

/**
 * Proof: fold an Int-kind tree (nested Concats over IntArr leaves) in ONE pass, no materialize, no
 * recursion — an explicit local stack drives a preorder DFS, and the inline `op` is spliced into the
 * tight per-leaf `int[]` loop so it stays unboxed.
 */
object TreeFold:
  inline def sumInt(xs: FBase)(inline op: (Int, Int) => Int): Int =
    var acc = 0
    var stack = new Array[FBase](16)
    stack(0) = xs
    var sp = 1
    while sp > 0 do
      sp -= 1
      stack(sp) match
        case leaf: IntArr =>
          val a = leaf.a
          val n = leaf.length
          var i = 0
          while i < n do
            acc = op(acc, a(i))
            i += 1
        case c: Concat =>
          if sp + 2 > stack.length then stack = java.util.Arrays.copyOf(stack, stack.length * 2)
          stack(sp) = c.right // push right first so left pops first (left-to-right order)
          sp += 1
          stack(sp) = c.left
          sp += 1
        case _ => ()
    acc

object TreeMain:
  // (((1,2,3) ++ (4,5)) ++ (6)) as a nested Concat tree
  def buildTree(): FBase =
    new Concat(new Concat(new IntArr(Array(1, 2, 3), 3), new IntArr(Array(4, 5), 2)), new IntArr(Array(6), 1))

  def sumTree(xs: FBase): Int = TreeFold.sumInt(xs)((a, b) => a + b)
