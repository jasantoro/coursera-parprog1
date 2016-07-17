package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

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
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanced(chars: Array[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)
    }
    balanced(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, open: Int, offset: Int): BigInt = {
      val current = idx + offset
      if(current >= until || current >= chars.length) open
      else {
        val nowOpen: Int = {
          val c = chars(current)
          if(c == '(') open + 1
          else if (c == ')') open - 1
          else open
        }
        if(idx == 0 && nowOpen < 0) Int.MaxValue
        else traverse(idx, until, nowOpen, offset + 1)
      }
    }

    def reduce(from: Int, until: Int): BigInt = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = until - (until - from) / 2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        if(l == Int.MaxValue || r == Int.MaxValue) Int.MaxValue
        else l + r
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
