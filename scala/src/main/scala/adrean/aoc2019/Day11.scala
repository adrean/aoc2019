package adrean.aoc2019

import scala.collection.mutable

object Day11 {
  def main(args: Array[String]): Unit = {
    // part 1
    runProgram()
    // part 2
    runProgram(true)
  }

  def runProgram(initCell:Boolean=false) : Unit = {
    var data = "3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,29,1006,0,98,2,1005,8,10,1,1107,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,62,1006,0,27,2,1002,12,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,90,1,1006,1,10,2,1,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,121,1,1003,5,10,1,1003,12,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,151,1006,0,17,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,175,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,197,2,6,14,10,1006,0,92,1006,0,4,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,229,1006,0,21,2,102,17,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,259,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,280,1006,0,58,1006,0,21,2,6,11,10,101,1,9,9,1007,9,948,10,1005,10,15,99,109,633,104,0,104,1,21101,937150919572,0,1,21102,328,1,0,1105,1,432,21101,0,387394675496,1,21102,1,339,0,1106,0,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,46325083283,1,1,21102,1,386,0,1106,0,432,21101,0,179519401051,1,21102,397,1,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,868410348308,1,21102,1,420,0,1105,1,432,21102,718086501140,1,1,21102,1,431,0,1105,1,432,99,109,2,22101,0,-1,1,21101,40,0,2,21101,0,463,3,21101,453,0,0,1106,0,496,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1101,0,0,458,109,-2,2105,1,0,0,109,4,2102,1,-1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,22102,1,-3,1,22102,1,-2,2,21102,1,1,3,21102,1,532,0,1105,1,537,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,22101,0,-4,-4,1105,1,628,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,579,0,1105,1,537,22101,0,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,22102,1,-1,1,21102,1,620,0,105,1,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0".split(",").map(_.toLong)
    val size = 100
    var painting = Array.fill(size)(Array.fill(size)('.'))
    var position = (size / 2, size / 2)
    if (initCell) painting(position._1)(position._2) = '#'
    val directions:Array[(Int,Int)]= Array((-1, 0), (0, 1), (1, 0), (0, -1))
    var currentDirection = 0
    var outputType = "color"
    var paintedCells:mutable.HashMap[(Int,Int),Char] = mutable.HashMap()
    data ++= Array.fill(9000)(0L)
    var output:Long = 0L
    var pointer= 0
    var relative = 0
    var input = 0

    val valueFromMode = (parameter:Int, mode:Int) => mode match {
      case 1 =>
        pointer + parameter
      case 0 =>
        data(pointer + parameter).toInt
      case 2 =>
        relative + data(pointer + parameter).toInt
    }

    while (data(pointer) != 99) {
      var pointerDigits = data(pointer).toString.map(_.asDigit).reverse.toArray
      if (pointerDigits.length < 5) for (_ <- pointerDigits.length until 5) pointerDigits :+= 0
      pointerDigits(0) match {
        case 1 =>
          data(valueFromMode(3, pointerDigits(4))) = data(valueFromMode(1, pointerDigits(2))) + data(valueFromMode(2, pointerDigits(3)))
          pointer += 4
        case 2 =>
          data(valueFromMode(3, pointerDigits(4))) = data(valueFromMode(1, pointerDigits(2))) * data(valueFromMode(2, pointerDigits(3)))
          pointer += 4
        case 3 =>
          input = painting(position._1)(position._2) match {
            case '.' => 0
            case '#' => 1
          }
          data(valueFromMode(1, pointerDigits(2))) = input
          pointer += 2
        case 4 =>
          output = data(valueFromMode(1, pointerDigits(2)))
          outputType match {
            case "color" =>
              val cell = if (output == 0) '.' else '#'
              painting(position._1)(position._2) = cell
              outputType = "dir"
              if (!paintedCells.contains((position._1,position._2))) {
                paintedCells += ((position._1,position._2) ->cell)
              } else {
                paintedCells((position._1,position._2)) = cell
              }
            case "dir" =>
              currentDirection = if (output == 0) (currentDirection + 4 -1) % 4 else (currentDirection + 1) % 4
              val dir = directions(currentDirection)
              position = (position._1 + dir._1, position._2 + dir._2)
              outputType = "color"
          }
          pointer += 2
        case 5 =>
          if (data(valueFromMode(1, pointerDigits(2))) != 0) pointer = data(valueFromMode(2, pointerDigits(3))).toInt
          else pointer += 3
        case 6 =>
          if (data(valueFromMode(1, pointerDigits(2))) == 0) pointer = data(valueFromMode(2, pointerDigits(3))).toInt
          else pointer += 3
        case 7 =>
          if (data(valueFromMode(1, pointerDigits(2))) < data(valueFromMode(2, pointerDigits(3)))) {
            data(valueFromMode(3, pointerDigits(4))) = 1L
          } else { data(valueFromMode(3, pointerDigits(4))) = 0L}
          pointer += 4
        case 8 =>
          if (data(valueFromMode(1, pointerDigits(2))) == data(valueFromMode(2, pointerDigits(3)))) {
            data(valueFromMode(3, pointerDigits(4))) = 1L
          } else { data(valueFromMode(3, pointerDigits(4))) = 0L}
          pointer += 4
        case 9 =>
          relative = data(valueFromMode(1,pointerDigits(2))).toInt + relative
          pointer += 2
      }
    }

    for (i <- painting.indices) println(painting(i).mkString(""))


    println(s"Painted cells : ${paintedCells.size}")
  }
}
