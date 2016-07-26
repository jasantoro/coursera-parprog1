val a = Array(1, 3, 5, 7, 9)

def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}
sum(a)

def max(xs: Array[Int]): Int = {
  xs.par.fold(Integer.MIN_VALUE)(math.max)
}
max(a)

private def isVowel(c: Char): Boolean = c.toLower match {
  case 'a' => true
  case 'e' => true
  case 'i' => true
  case 'o' => true
  case 'u' => true
  case _ => false
}

Array('E', 'P', 'F', 'L').par
  .aggregate(0)((count, c) => if(isVowel(c)) count + 1 else count, _ + _)