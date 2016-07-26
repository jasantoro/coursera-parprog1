(1 until 1000).par
.filter(n => n % 3 == 0)
.count(n => n.toString == n.toString.reverse)

def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

sum(Array(1, 3, 5))