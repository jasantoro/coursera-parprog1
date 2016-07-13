import org.coursera.week1._

def power(a: Int, p: Double): Int = math.pow(a, p).toInt

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int =
  (s until t).foldLeft(0)((acc, i) => acc + power(a(i), p))

sumSegment(Array(1, 2, 4, 5, 6, 9), 2.0, 2, 4)

def pNorm(a: Array[Int], p: Double): Int =
  power(sumSegment(a, p, 0, a.length), 1/p)

pNorm(Array(1, 2, 4, 5, 6, 9), 2.0)

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = (sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  power(sum1 + sum2, 1/p)
}

def pNormPar(a: Array[Int], p: Double): Int = {
  import org.coursera.week1._
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  power(sum1 + sum2, 1/p)
}

def pNormParRec(a: Array[Int], p: Double): Int = {
  val threshold = 3
  def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    if(t - s < threshold) {
      sumSegment(a, p, s, t)
    } else {
      val m = s + (t - s)/2
      val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
      sum1 + sum2
    }
  }
  power(segmentRec(a, p, 0, a.length), 1/p)
}