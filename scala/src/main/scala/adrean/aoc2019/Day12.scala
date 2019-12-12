package adrean.aoc2019

import scala.collection.mutable
import scala.math.abs
import scala.collection.mutable.Set

import adrean.aoc2019.utils.Math.lcm

object Day12 {
  def main(args: Array[String]): Unit = {
    val source = "day12.txt"
    var step = 1
    val moons = scala.io.Source.fromResource(source)
      .getLines()
      .toArray
      .map(_.split(",").map(_.split("=")(1).toInt))
      .map(x => (x, Array(0, 0, 0)))

    val detectedLoop = mutable.HashMap(0 -> 0L, 1 -> 0L, 2 -> 0L)

    val stepValues = Array.fill(3)(Set[(Int, Int, Int, Int, Int, Int, Int, Int)]())
    while (detectedLoop.minBy(_._2)._2 == 0) {
      for (pair <- 0 to 3 combinations 2) {
        for (axis <- 0 to 2) {
          val vel = comparePositions(moons(pair(0))._1(axis), moons(pair(1))._1(axis))
          moons(pair(0))._2(axis) += vel
          moons(pair(1))._2(axis) += -vel
        }
      }

      for (moon <- moons)
        for (axis <- 0 to 2)
          moon._1(axis) += moon._2(axis)

      for (axis <- detectedLoop.filter(_._2 == 0).keys) {
        val stepValue = (moons(0)._1(axis), moons(1)._1(axis), moons(2)._1(axis), moons(3)._1(axis), moons(0)._2(axis), moons(1)._2(axis), moons(2)._2(axis), moons(3)._2(axis))
        if (stepValues(axis).contains(stepValue)){
          detectedLoop(axis) = step - 1
        }
        stepValues(axis) += stepValue
      }

      if (step == 1000) {
        val energy = moons.map(moon => moon._1.map(abs).sum * moon._2.map(abs).sum).sum
        println(s"Part 1 : $energy")
      }
      step += 1
    }

    println(s"Part 2 : ${lcm(detectedLoop.values.toSeq:_*)}")
  }

  def comparePositions(a:Int, b:Int) :Int = {
    if (a > b) -1
    else if (a == b) 0
    else 1
  }


}
