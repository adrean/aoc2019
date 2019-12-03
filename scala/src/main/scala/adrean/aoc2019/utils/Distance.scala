package adrean.aoc2019.utils

object Distance {

  def manhattanDistance(a: (Int, Int), b: (Int, Int)): Int = {
    scala.math.abs(a._1 - b._1) + scala.math.abs(a._2 - b._2)
  }

  def manhattanDistanceToOrigin(a:(Int,Int)): Int = manhattanDistance(a, (0,0))

}
