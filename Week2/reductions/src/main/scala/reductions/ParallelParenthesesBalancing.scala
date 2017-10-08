package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = loopBalance(chars)

  def recBalance(chars: Array[Char]): Boolean = {
    def inner(xs: Array[Char], acc: Int): Boolean = {
      if (acc < 0) false
      else if (xs.isEmpty) acc == 0
      else if (xs.head == '(') inner(xs.tail, acc + 1)
      else if (xs.head == ')') inner(xs.tail, acc - 1)
      else inner(xs.tail, acc)
    }

    inner(chars, 0)
  }

  def loopBalance(chars: Array[Char]): Boolean = {
    var i = 0
    var count = 0

    while (i < chars.length) {
      val c = chars(i)

      if (c == '(') count += 1
      else if (c == ')') count -= 1

      i += (if (count < 0) chars.length else 1)
    }

    count == 0
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, sx: Int, dx: Int): (Int, Int) = {
      if (idx == until) (sx, dx)
      else {
        val c = chars(idx)

        if (c == '(') traverse(idx + 1, until, sx, dx + 1)
        else if (c == ')') {
          if (dx > 0) traverse(idx + 1, until, sx, dx - 1)
          else traverse(idx + 1, until, sx + 1, dx)
        }
        else traverse(idx + 1, until, sx, dx)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (x1, x2) = parallel(reduce(from, mid), reduce(mid, until))
        (x1._1, x1._2 - x2._1 + x2._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
