package object Util {
  def allNonSwapPairs[T](xs: Seq[T]) : Seq[(T,T)] = {
    var output = List[(T,T)]()

    for (i <- xs.indices) {
      for (j <- i until xs.length) {
        if (j > i) {
          output = (xs(i),xs(j)) :: output
        }
      }
    }

    output.reverse
  }

  def invertMap[T,U](m: Map[T,U]) : Map[U,T] = {
    val m2 = m.map { case (t,u) => u -> t }
    assert(m.size == m2.size, "Map is not invertible.")
    m2
  }
}
