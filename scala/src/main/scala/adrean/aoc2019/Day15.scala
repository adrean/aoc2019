package adrean.aoc2019

import scala.collection.mutable

object Day15 {
  def main(args: Array[String]): Unit = {
    val source = "day15.txt"
    val data = scala.io.Source.fromResource(source).getLines().toArray.head
    runProgram(data)
  }

  def printGrid(grid: mutable.HashMap[(Int, Int), Char]) = {
    val minX = grid.minBy(_._1._1)._1._1
    val maxX = grid.maxBy(_._1._1)._1._1
    val minY = grid.minBy(_._1._2)._1._2
    val maxY = grid.maxBy(_._1._2)._1._2
    for (x <- minX to maxX) {
      for (y <- minY to maxY) {
        print(grid.getOrElse((x,y), ' '))
      }
      print('\n')
    }
  }

  def runProgram(raw_data: String): Long = {
    var data = raw_data.split(",").map(_.toLong)
    data ++= Array.fill(9000)(0L)
    var output:Long = 1L
    var pointer= 0
    var relative = 0
    //var path: mutable.HashMap[(Int,Int),Array[Int]] = mutable.HashMap[(Int,Int),Array[Int]]((500,500) -> Array(1, 2, 3, 4))
    var path: Array[(Int, Int)] = Array[(Int, Int)]((500,500))
    var grid: mutable.HashMap[(Int,Int),Char] = mutable.HashMap[(Int,Int), Char]()
    grid += ((500, 500) -> 'D')
    var pos = (500,500)
    var input = 1
    val valueFromMode = (parameter:Int, mode:Int) => mode match {
      case 1 =>
        pointer + parameter
      case 0 =>
        data(pointer + parameter).toInt
      case 2 =>
        relative + data(pointer + parameter).toInt
    }

    var goingBack:Boolean = false
    var oxygen:Array[(Int,Int)] = Array[(Int,Int)]()
    while (data(pointer) != 99 & path.length > 0) {
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
          data(valueFromMode(1, pointerDigits(2))) = input
          pointer += 2
        case 4 =>
          output = data(valueFromMode(1, pointerDigits(2)))
          val getDirection = (input:Int) => input match {
            case 1 => (-1,0)
            case 2 => (1,0)
            case 3 => (0,-1)
            case 4 => (0,1)
          }
          val direction = getDirection(input)
          output match {
            case 0 =>
              var directionAvailable = false
              grid += ((pos._1 + direction._1, pos._2 + direction._2) -> '#')
              for (d <- 1 to 4) {
                if (!grid.contains((pos._1 + getDirection(d)._1, pos._2 + getDirection(d)._2))) {
                  directionAvailable = true
                  input = d
                }
              }
              if (!directionAvailable) {
                val blocked = path.last
                path = path.dropRight(1)
                if (path.length >0) {
                  input = (path.last._1 - blocked._1, path.last._2 - blocked._2) match {
                    case (-1, 0) => 1
                    case (1, 0) => 2
                    case (0, -1) => 3
                    case (0, 1) => 4
                  }
                  goingBack = true
                }
              }
            case 1 =>
              var directionAvailable = false
              pos = (pos._1 + direction._1, pos._2 + direction._2)
              if (!goingBack) {
                grid += (pos -> '.')
                path :+= pos
              } else {
                goingBack = false
              }
              for (d <- 1 to 4) {
                if (!grid.contains((pos._1 + getDirection(d)._1, pos._2 + getDirection(d)._2))) {
                  directionAvailable = true
                  input = d
                }
              }
              if (!directionAvailable) {
                val blocked = path.last
                path = path.dropRight(1)
                if (path.length > 0) {
                  input = (path.last._1 - blocked._1, path.last._2 - blocked._2) match {
                    case (-1, 0) => 1
                    case (1, 0) => 2
                    case (0, -1) => 3
                    case (0, 1) => 4
                  }
                  goingBack = true
                }
              }
            case 2 =>
              println(s"Part 1: ${path.length}")
              var directionAvailable = false
              pos = (pos._1 + direction._1, pos._2 + direction._2)
              if (!goingBack) {
                grid += (pos -> 'O')
                path :+= pos
                oxygen :+= pos
              } else {
                goingBack = false
              }
              for (d <- 1 to 4) {
                if (!grid.contains((pos._1 + getDirection(d)._1, pos._2 + getDirection(d)._2))) {
                  directionAvailable = true
                  input = d
                }
              }
              if (!directionAvailable) {
                val blocked = path.last
                if (path.length > 0) {
                  path = path.dropRight(1)
                  input = (path.last._1 - blocked._1, path.last._2 - blocked._2) match {
                    case (-1, 0) => 1
                    case (1, 0) => 2
                    case (0, -1) => 3
                    case (0, 1) => 4
                  }
                  goingBack = true
                }
              }
          }

          /*input = output match {
            case 0 => ((input + 1) % 4) + 1
            case 1 => input
          }*/
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
    printGrid(grid)
    print("\n\n")
    var spreadCount = 1
    var minutes = 0
    while (spreadCount > 0) {
      spreadCount = 0
      for (pos <- oxygen) {
        for (dir <- Array((1,0),(-1,0),(0,1),(0,-1))) {
          if (Array('.','D').contains(grid((pos._1 + dir._1, pos._2 + dir._2)))) {
            grid((pos._1 + dir._1, pos._2 + dir._2)) = 'O'
            oxygen :+= (pos._1 + dir._1, pos._2 + dir._2)
            spreadCount += 1
          }
        }
      }
      minutes += 1
    }
    printGrid(grid)
    println(s"Part 2 : $minutes")
    output
  }


}
