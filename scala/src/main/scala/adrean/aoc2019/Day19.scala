package adrean.aoc2019

import scala.collection.mutable

object Day19 {

  def main(args: Array[String]): Unit = {
    val source = "day19.txt"
    val raw_data = scala.io.Source.fromResource(source).getLines.next
    runProgram(raw_data)
  }

  def runProgram(raw_data: String): Unit = {
    var grid: mutable.HashMap[(Int,Int), Char] = mutable.HashMap[(Int,Int),Char]()
    var current = 'x'
    for (x <- 0 to 1299) {
      for (y <- 0 to 1299) {
        var data = raw_data.split(",").map(_.toLong)
        data ++= Array.fill(9000)(0L)
        var output:Long = 0L
        var pointer, relative= 0
        var current = 'x'
        var input = x

        val valueFromMode = (parameter:Int, mode:Int) => mode match {
          case 1 =>
            pointer + parameter
          case 0 =>
            data(pointer + parameter).toInt
          case 2 =>
            relative + data(pointer + parameter).toInt
        }
        //data(pointer)
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
              data(valueFromMode(1, pointerDigits(2))) = input
              input = current match {
                case 'x' =>
                  current = 'y'
                  y
                case 'y' =>
                  current = 'x'
                  x
              }
              pointer += 2
            case 4 =>
              output = data(valueFromMode(1, pointerDigits(2)))
              output match {
                case 0 =>
                  grid((x, y)) = '.'
                case 1 =>
                  grid((x, y)) = '#'
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
              } else {
                data(valueFromMode(3, pointerDigits(4))) = 0L
              }
              pointer += 4
            case 8 =>
              if (data(valueFromMode(1, pointerDigits(2))) == data(valueFromMode(2, pointerDigits(3)))) {
                data(valueFromMode(3, pointerDigits(4))) = 1L
              } else {
                data(valueFromMode(3, pointerDigits(4))) = 0L
              }
              pointer += 4
            case 9 =>
              relative = data(valueFromMode(1, pointerDigits(2))).toInt + relative
              pointer += 2
          }
        }
      }
    }
    for (i <- 5 to 1299)
      getBestShip(grid, i)

    println(s"Part 1 : ${grid.count(_._2 == '#')}")
  }

  def getBestShip(grid: mutable.HashMap[(Int, Int), Char], line:Int): Unit = {
    var shipFit = true
    var beam = grid.filter(x => x ._1._1 == line & x._2 == '#')
    if (beam.isEmpty) return
    var start = beam.minBy(_._1._2)._1
    var shipSize = 0
    while (shipFit & shipSize < 110) {
      shipSize += 1
      if (grid((start._1, start._2 +shipSize)) != '#' | grid((start._1 - shipSize, start._2 + shipSize)) != '#'){
        shipFit = false
      }
    }
    println(s"$start - ${shipSize - 1}")
  }


}
