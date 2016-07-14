import org.coursera._

def quickSort(xs: Array[Int], offset: Int, length: Int): Unit = {
  def swap(i: Int, j: Int) {
    val t = xs(i);
    xs(i) = xs(j);
    xs(j) = t
  }
  def sort1(xs: Array[Int], offset: Int, length: Int) {
    val pivot = xs((offset + length) / 2)
    var i = offset;
    var j = length
    while (i <= j) {
      while (xs(i) < pivot) i += 1
      while (xs(j) > pivot) j -= 1
      if (i <= j) {
        swap(i, j)
        i += 1
        j -= 1
      }
    }
    if (offset < j) sort1(xs, offset, j)
    if (j < length) sort1(xs, i, length)
  }
  sort1(xs, offset, length)
}

def parMergeSort(xs: Array[Int], maxDepth: Int) = {
  val ys = new Array[Int](xs.length)
  def sort(from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth) {
      quickSort(xs, from, until - from)
    } else {
      val mid = (from + until) / 2
      parallel(sort(mid, until, depth + 1), sort(from, mid, depth + 1))
      val flip = (maxDepth - depth) % 2 == 0
      val src = if (flip) ys else xs
      val dst = if (flip) xs else ys
      merge(src, dst, from, mid, until)
    }
  }

  def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {
    var left = from
    var right = mid
    var i = from
    while (left < mid && right < until) {
      while (left < mid && src(left) <= src(right)) {
        dst(i) = src(left)
        i += 1
        left += 1
      }
      while (right < until && src(right) <= src(left)) {
        dst(i) = src(right)
        i += 1
        right += 1
      }
    }
    while (left < mid) {
      dst(i) = src(left)
      i += 1
      left += 1
    }
    while (right < mid) {
      dst(i) = src(right)
      i += 1
      right += 1
    }
  }
  sort(0, xs.length, 0)

  def copy(src: Array[Int], target: Array[Int], from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth) {
      Array.copy(src, from, target, from, until - from)
    } else {
      val mid = (from + until) / 2
      parallel(
        copy(src, target, mid, until, depth + 1),
        copy(src, target, from, mid, depth + 1)
      )
    }
  }

  copy(ys, xs, 0, xs.length, 0)
}

import org.scalameter._

def initialize(xs: Array[Int]) {
  var i = 0
  while (i < xs.length) {
    xs(i) = i % 100
    i += 1
  }
}

val time3 = config {
  Key.exec.minWarmupRuns -> 20
  Key.exec.maxWarmupRuns -> 60
  Key.verbose -> true
} withWarmer (new Warmer.Default) measure {
  (0 until 1000000).toArray
}

def main(args: Array[String]) {
  val length = 10000000
  val maxDepth = 7
  val xs = new Array[Int](length)
  val seqtime =
    config {
      Key.exec.minWarmupRuns -> 20
      Key.exec.maxWarmupRuns -> 60
      Key.verbose -> true
    } withWarmer (new Warmer.Default) setUp {
      _ => initialize(xs)
    } measure {
      quickSort(xs, 0, xs.length)
    }
  println(s"sequential sum time: $seqtime ms")
  val partime = config {
    Key.exec.minWarmupRuns -> 20
    Key.exec.maxWarmupRuns -> 60
    Key.verbose -> true
  } withWarmer (new Warmer.Default) setUp {
    _ => initialize(xs)
  } measure {
    parMergeSort(xs, maxDepth)
  }
  println(s"fork / join time: $partime ms")
  println(s"speedup: ${seqtime / partime}")
}