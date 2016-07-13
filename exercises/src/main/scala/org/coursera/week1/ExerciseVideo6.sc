import scala.util.Random

def mcCount(iter: Int): Int = {
  val randomX = new Random()
  val randomY= new Random()
  var hits = 0
  for(i <- 0 until iter) {
    val x = randomX.nextDouble
    val y = randomY.nextDouble
    if(x*x + y*y < 1) hits = hits + 1
  }
  hits
}
def monteCarloPISeq(iter: Int): Double = 4.0 * mcCount(iter) / iter
def monteCarloPIPar(iter: Int): Double = {
  import org.coursera.week1._
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(mcCount(iter/4), mcCount(iter/4)),
    parallel(mcCount(iter/4), mcCount(iter - 3*iter/4))
  )
  4.0 * (pi1+pi2+pi3+pi4)/iter
}

monteCarloPISeq(20)
monteCarloPISeq(200)
monteCarloPISeq(400)
monteCarloPIPar(20)
monteCarloPIPar(200)
monteCarloPIPar(400)