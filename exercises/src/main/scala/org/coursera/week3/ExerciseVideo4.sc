import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.concurrent.TrieMap
import scala.collection.{GenSeq, GenSet, mutable}

def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) =>
      if(n > largest && n.toString == n.toString.reverse) n else largest,
    math.max
  )
}
val array = (0 until 1000000).toArray
largestPalindrome(array)
largestPalindrome(array.par)

def intersectionWrong(a: GenSet[Int], b: GenSet[Int]) = {
  val result = mutable.Set[Int]()
  for(x <- a) if (b contains x) result.add(x)
  result
}

intersectionWrong((0 until 1000).toSet, (0 until 1000 by 4).toSet).size
intersectionWrong((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet).size

def intersection(a: GenSet[Int], b: GenSet[Int]) = {
  val result = new ConcurrentSkipListSet[Int]()
  for(x <- a) if (b contains x) result.add(x)
  result
}

intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet).size
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet).size

def intersectionOther(a: GenSet[Int], b: GenSet[Int]) = {
  if(a.size < b.size) a.filter(b(_))
  else b.filter(a(_))
}

intersectionOther((0 until 1000).toSet, (0 until 1000 by 4).toSet).size
intersectionOther((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet).size


val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0
for((k, v) <- graph.par) graph(k) = graph(v)
val violation1 = graph.find({case (i, v) => v != (i + 2) % graph.size})
println(s"violation: $violation1")

val safeGraph = TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
safeGraph(safeGraph.size - 1) = 0
val previous = safeGraph.snapshot()
for((k, v) <- safeGraph.par) safeGraph(k) = previous(v)
val violation2 = safeGraph.find({case (i, v) => v != (i + 2) % graph.size})
println(s"violation: $violation2")
