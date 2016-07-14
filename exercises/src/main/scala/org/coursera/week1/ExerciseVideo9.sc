val xs = List(1, 2, 3)
assert(xs.reverse == List(3, 2, 1))

val xs2 = List(1, 2, 3)
val startTime = System.nanoTime()
xs2.reverse
println((System.nanoTime() - startTime) / 1000000)

import org.scalameter._
val time = measure {
  (0 until 1000000).toArray
}
println(s"Array initialization time: $time ms")

val time2 = withWarmer(new Warmer.Default) measure {
  (0 until 1000000).toArray
}
println(s"Array initialization time: $time2 ms")

val time3 = config {
  Key.exec.minWarmupRuns -> 20
  Key.exec.maxWarmupRuns -> 60
  Key.verbose -> true
} withWarmer(new Warmer.Default) measure {
  (0 until 1000000).toArray
}
println(s"Array initialization time: $time3 ms")
