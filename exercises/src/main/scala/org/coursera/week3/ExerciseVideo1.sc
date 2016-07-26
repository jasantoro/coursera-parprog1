def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for(i <- (0 until xs.length).par) {
    xs(i) = v
  }
}

private def computePixel(xc: Double, yc: Double, maxIteration: Int): Int = {
  var i = 0;
  var x, y = 0.0
  while(x * x + y * y < 4 && i < maxIteration) {
    val xt = x * x - y * y + xc
    val yt = 2 * x * y + yc
    x = xt
    y = yt
    i += 1
  }
  color(i, maxIteration)
}

private def color(iters: Int, maxIteration: Int): Int = {
  val a = 0xff << 24
  val r = math.min(255, 1.0 * iters / maxIteration * 255).toInt << 16
  val g = math.min(255, 2.0 * iters / maxIteration * 255).toInt << 8
  val b = math.min(255, 3.0 * iters / maxIteration * 255).toInt << 0
  a | r | g | b
}

def parRender(): Unit = {
//  for(idx <- (0 until image.length).par) {
//    val (xc, yc) = coordinateFor(idx)
//    image(idx) = computePixel(xc, yc, maxIteration)
//  }
}