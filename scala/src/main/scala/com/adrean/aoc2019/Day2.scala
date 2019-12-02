package com.adrean.aoc2019

object Day2 {

  def main(args: Array[String]): Unit = {
    val input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,103,1,103,6,0,99,2,14,0,0"

    // Part 1
    val data = input.split(",").map(_.toInt)
    runProgram(12, 2, data)
    println(data.mkString(", "))

    // Part 2
    for (i <- 1 to 100) {
      for (j <- 1 to 100) {
        val input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,103,1,103,6,0,99,2,14,0,0"
        val data = input.split(",").map(_.toInt)
        runProgram(i, j, data)
        if (data(0) == 19690720) println(data.mkString(", "))
      }
    }
  }

  def runProgram(noun: Int, verb: Int, data:Array[Int]) {
    var index = 0
    data(1) = noun
    data(2) = verb
    while (data(index) != 99) {
      data(index) match {
        case 1 => data(data(index + 3)) = data(data(index + 1)) + data(data(index + 2))
        case 2 => data(data(index + 3)) = data(data(index + 1)) * data(data(index + 2))
      }
      index += 4
    }
  }

}
