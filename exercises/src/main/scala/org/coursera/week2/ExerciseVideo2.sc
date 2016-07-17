import org.coursera._

def mapSeq[A, B](lst: List[A], f: A => B): List[B] = lst match {
  case Nil => Nil
  case h :: t => f(h) :: mapSeq(t, f)
}

def mapPar[A, B](lst: List[A], f: A => B): List[B] = lst match {
  case Nil => Nil
  case h :: t => {
    val (mh, mt) = parallel(f(h), mapSeq(t, f))
    mh :: mt
  }
}

def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
  // writes to out(i) for left <= i <= right - 1
  var i = left
  while(i < right) {
    out(i) = f(inp(i))
    i = i + 1
  }
}
val in = Array(2, 3, 4, 5, 6)
val out = Array(0, 0, 0, 0, 0)
val f = (x: Int) => x * x
mapASegSeq(in, 0, in.length, f, out)
out

val threshold = 3
def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
  // writes to out(i) for left <= i <= right - 1
  if(right - left < threshold) {
    mapASegSeq(inp, left, right, f, out)
  } else {
    val mid = left + (right - left) / 2
    parallel(mapASegPar(inp, left, mid, f, out),
      mapASegPar(inp, mid, right, f, out))
  }
}
val out2 = Array(0, 0, 0, 0, 0)
mapASegPar(in, 0, in.length, f, out2)
out2

def power(a: Int, b: Double): Int = math.pow(a, b).toInt

def normsOfSeq(inp: Array[Int], p: Double, left: Int, right: Int, out: Array[Int]): Unit = {
  var i = left
  while (i < right) {
    out(i) = power(inp(i), p)
    i = i + 1
  }
}

def normsOfPar(inp: Array[Int], p: Double, left: Int, right: Int, out: Array[Int]): Unit = {
  if(right - left < threshold) {
    normsOfSeq(inp, p, left, right, out)
  } else {
    val mid = left + (right - left) / 2
    parallel(normsOfPar(inp, p, left, mid, out),
      normsOfPar(inp, p, mid, right, out))
  }
}

sealed abstract class Tree[A] { val size: Int }
case class Leaf[A](a: Array[A]) extends Tree[A] {
  override val size = a.size
}
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
  override val size = l.size + r.size
}

def mapTreePar[A, B](t: Tree[A], f: A => B): Tree[B] =
  t match {
    case Leaf(a: Array[A]) => {
      val len = a.length;
      val b = new Array[B](len)
      var i = 0
      while(i < len) {
        b(i) = f(a(i))
        i = i + 1
      }
      Leaf(b)
    }
    case Node(l, r) => {
      val (lb, rb) = parallel(mapTreePar(l, f),
        mapTreePar(r, f))
      Node(lb, rb)
    }
  }