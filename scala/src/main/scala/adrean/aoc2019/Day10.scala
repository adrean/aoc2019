package adrean.aoc2019

import scala.collection.mutable

object Day10 {

  def getGcd(a: Int, b: Int): Int =
    if (b == 0) a else getGcd(b, a % b)

  def main(args: Array[String]): Unit = {
    val resourceName = "day10.txt"
    val data = scala.io.Source.fromResource(resourceName).getLines().toArray.map(_.toCharArray)

    val asteroids = getAsteroids(data)

    val station = runPart1(asteroids)
    println("Part 1 : " + station._2 + " from station " +station._1)

    val vaporizedAsteroids = runPart2(station._1, asteroids)
    println(s"Part 2 ${vaporizedAsteroids(199)}")
  }

  def getAsteroids(data: Array[Array[Char]]): Array[(Int,Int)] = {
    var asteroids = Array[(Int,Int)]()
    for (i <- data.indices)
      for (j <- data(i).indices)
        if (data(i)(j) == '#') asteroids :+= (i,j)
    asteroids
  }

  def runPart1(asteroids: Array[(Int,Int)]): ((Int, Int), Int) = {
    var asteroidsInSights:mutable.HashMap[(Int,Int), Int] = new mutable.HashMap[(Int,Int), Int]()

    for (asteroid <- asteroids) {

      // start working
      var inSights = 0
      for (otherAsteroid <- asteroids) {
        // This is an asteroids, let's see if this in sight
        val direction = (asteroid._1 - otherAsteroid._1, asteroid._2 - otherAsteroid._2)
        val gcd = getGcd(direction._1, direction._2)
        if (scala.math.abs(gcd) > 1) {
          var inSight = true
          val unitDirection = (direction._1 / scala.math.abs(gcd), direction._2 / scala.math.abs(gcd))
          for (i <- 1 until scala.math.abs(gcd)) {
            if (asteroids.contains((otherAsteroid._1 + unitDirection._1 * i, otherAsteroid._2 + unitDirection._2 * i))) inSight = false
          }
          if (inSight) inSights += 1
        } else if (scala.math.abs(gcd) == 1){
          inSights += 1
        }
      }

      // End Work, store data
      asteroidsInSights += (asteroid -> inSights)
      println(asteroid -> inSights)
    }
    val result = asteroidsInSights.maxBy(_._2)
    result
  }

  def runPart2(station:(Int,Int), asteroids: Array[(Int,Int)]) : Array[(Int,Int)] = {
    var vaporizedAsteroids = Array[(Int,Int)]()
    var vaporized = 0
    var asteroidMap:mutable.HashMap[(Int,Int), Boolean] = mutable.HashMap(asteroids.filter(_ != station).map(asteroid => asteroid -> false):_*)
    while (vaporized < 200 & asteroidMap.nonEmpty) {
      updateAsteroidMap(station, asteroidMap)
      var vaporizedRound = asteroidMap.filter(_._2).keys.toArray.sortBy(asteroid => -getAngle(station, asteroid))
      vaporized += vaporizedRound.length
      for (asteroid <- vaporizedRound) asteroidMap -= asteroid
      vaporizedAsteroids ++= vaporizedRound
    }
    vaporizedAsteroids
  }

  def getAngle(station:(Int,Int), asteroid:(Int,Int)): Double = {
    val direction = (asteroid._1 - station._1, asteroid._2 - station._2)
    val angle = scala.math.atan2(direction._2, direction._1)
    angle
  }

  def updateAsteroidMap(station:(Int,Int), map: mutable.HashMap[(Int,Int), Boolean]): Unit = {
    for (asteroid <- map.keys) {
      val direction = (asteroid._1 - station._1, asteroid._2 - station._2)
      val gcd = getGcd(direction._1, direction._2)
      if (scala.math.abs(gcd) > 1) {
        var inSight = true
        val unitDirection = (direction._1 / scala.math.abs(gcd), direction._2 / scala.math.abs(gcd))
        for (i <- 1 until scala.math.abs(gcd)) {
          if (map.contains((station._1 + unitDirection._1 * i, station._2 + unitDirection._2 * i))) inSight = false
        }
        map(asteroid) = inSight
      } else map(asteroid) = true
    }

  }

}
