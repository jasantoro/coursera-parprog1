sealed trait Tree[+T]
case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](elem: T) extends Tree[T]
case object EmptyTree extends Tree[Nothing]

import org.coursera._

import scala.annotation.tailrec
import scala.reflect.ClassTag

def filter[T](tree: Tree[T])(p: T => Boolean): Tree[T] = tree match {
  case Node(left, right) => {
    val (rl, rr) = parallel(filter(left)(p), filter(right)(p))
    Node(rl, rr)
  }
  case Leaf(elem) => if(p(elem)) tree else EmptyTree
  case EmptyTree => EmptyTree
}

case object Empty extends Conc[Nothing] {
  def level = 0
  def size = 0
  def left = throw new Error()
  def right = throw new Error()
}
case class Single[T](val x: T) extends Conc[T] {
  def level = 0
  def size = 1
  def left = throw new Error()
  def right = throw new Error()
}
sealed trait Conc[T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]

  def <>(that: Conc[T]): Conc[T] = {
    def concat(xs: Conc[T], ys: Conc[T]): Conc[T] = {
      val diff = ys.level - xs.level
      if (diff >= -1 && diff <= 1) new <>(xs, ys)
      else {
        if (xs.left.level >= xs.right.level) new <>(xs.left, concat(xs.right, ys))
        else {
          val nrr = concat(xs.right.right, ys)
          if (nrr.level == xs.level - 3) {
            new <>(xs.left, new <>(xs.right.left, nrr))
          } else {
            new <>(new <>(xs.left, xs.right.left), nrr)
          }
        }
      }
    }
    if(this == Empty) that
    else if(that == Empty) this
    else concat(this, that)
  }
  def +=(elem: T): Conc[T] = {
    this <> Single(elem)
  }
}

case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  def level = 1 + math.max(left.level, right.level)
  def size = left.size + right.size
}

case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}

def appendLeaf_noLinear[T](xs: Conc[T], y: T): Conc[T] = Append(xs, new Single(y))

def appendLeaf[T](xs: Conc[T], ys: Conc[T]): Conc[T] = xs match {
  case Empty => ys
  case xs: Single[T] => new <>(xs, ys)
  case _ <> _ => new Append(xs, ys)
  case xs: Append[T] => append(xs, ys)
}
@tailrec private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
  if(xs.right.level > ys.level) new Append(xs, ys)
  else {
    val zs = new <>(xs.right, ys)
    xs.left match {
      case ws @ Append(_, _) => append(ws, zs)
      case ws if ws.level <= zs.level => ws <> zs
      case ws => new Append(ws, zs)
    }
  }
}

class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
  private var chunk: Array[T] = new Array(k)
  private var chunkSize: Int = 0
  final def +=(elem: T): Unit = {
    if(chunkSize >= k) expand()
    chunk(chunkSize) = elem
    chunkSize += 1
  }
  class Chunk[T](val array: Array[T], val size: Int) extends Conc[T] {
    def level = 0
  }
  def result: Conc[T] = {
    conc = appendLeaf(conc, new Chunk(chunk, chunkSize))
    conc
  }
  private def expand(): Unit = {
    conc = appendLeaf(conc, new Chunk(chunk, chunkSize))
    chunk = new Array(k)
    chunkSize = 0
  }
  private def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    val combinedConc = this.result <> that.result
    new ConcBuffer(k, combinedConc)
  }
}
