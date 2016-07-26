
trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while(hasNext) result = f(result, next())
    result
  }
}

trait Splitter[T] {
  val threshold = 10
  def split: Seq[Splitter[T]]
  def remaining: Int
  def foldLeft[S](z: S)(f: (S, T) => S): S = ???
  def fold(z: T)(f: (T, T) => T): T = {
    import org.coursera._
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children = for(child <- split) yield task { child.fold(z)(f) }
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}

trait Builder[A, Repr] {
  def +=(elem: A): Builder[A, Repr]
  def result: Repr
}
trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach(x => if(p(x)) b+=x)
    b.result
  }
}

trait Combiner[A, Repr] extends Builder[A, Repr] {
  def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
}