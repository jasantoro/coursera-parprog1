import org.coursera._

List(1, 3, 8).scanLeft(100)(_ + _)
List(1, 3, 8).scanRight(100)(_ + _)

def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
  out(0) = a0
  var a = a0
  var i = 0
  while(i < inp.length) {
    a = f(a, inp(i))
    i = i + 1
    out(i) = a
  }
}

sealed abstract class Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

sealed abstract class TreeRes[A] { val res: A}
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](left: TreeRes[A], override val res: A, right: TreeRes[A]) extends TreeRes[A]

def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tl, tr) = (reduceRes(l, f), reduceRes(r, f))
    NodeRes(tl, f(tl.res, tr.res), tr)
  }
}

val t1 = Node(Node(Leaf(1), Leaf(3)), Node(Leaf(8), Leaf(50)))
val plus = (x: Int, y: Int) => x + y
reduceRes(t1, plus)

def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tl, tr) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(tl, f(tl.res, tr.res), tr)
  }
}

def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(v) => Leaf(f(a0, v))
  case NodeRes(l, _, r) => {
    val (tl, tr) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
    Node(tl, tr)
  }
}

def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}

def prepend[A](a: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(a), Leaf(v))
  case Node(l, r) => Node(prepend(a, l), r)
}

