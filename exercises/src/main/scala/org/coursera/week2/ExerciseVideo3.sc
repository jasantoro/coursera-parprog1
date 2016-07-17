import org.coursera._

List(1, 3, 8).foldLeft(100)((s, x) => s - x)
List(1, 3, 8).foldRight(100)((s, x) => s - x)
List(1, 3, 8).reduceLeft((s, x) => s - x)
List(1, 3, 8).reduceRight((s, x) => s - x)

sealed abstract class Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) => f(reduce(l, f), reduce(r, f))
}

def fMinus = (x: Int, y: Int) => x - y
reduce(Node(Leaf(1), Node(Leaf(3), Leaf(8))), fMinus)
reduce(Node(Node(Leaf(1), Leaf(3)), Leaf(8)), fMinus)

def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) => {
    val (lv, rv) = parallel(reduce(l, f), reduce(r, f))
    f(lv, rv)
  }
}

def toList[A](tree: Tree[A]): List[A] = tree match {
  case Leaf(x) => List(x)
  case Node(l, r) => toList(l) ++ toList(r)
}

def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
  case Leaf(v) => Leaf(f(v))
  case Node(l, r) => Node(map(l, f), map(r, f))
}

// val t = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
// toList(t) == reduce(map(t, List(_)), _ ++ _)