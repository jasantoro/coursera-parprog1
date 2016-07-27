def combine(xs: Array[Int], ys: Array[Int]): Array[Int] = {
  val r = new Array[Int](xs.length + ys.length)
  Array.copy(xs, 0, r, 0, xs.length)
  Array.copy(ys, 0, r, xs.length, ys.length)
  r
}