package adrean.aoc2019

import scala.collection.mutable

object Day13 {
  def main(args: Array[String]): Unit = {
    val source = "day13.txt"
    var data = scala.io.Source.fromResource(source).getLines().toArray.head.split(",").map(_.toLong)
    println("Part 1 : " + runProgram(data, data(0).toInt))
    println("Part 2 : " + runProgram(data, 2))
  }

  def runProgram(inputData:Array[Long],init:Int):Int = {
    var data = inputData.clone()
    data(0) = init
    data ++= Array.fill(9000)(0L)
    var output:Long = 0L
    var pointer, relative, input= 0
    var bx, px = 0
    var position = Array[Int]()
    val tiles = mutable.HashMap[(Int,Int),Char]()

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
          input = if (bx < px) -1 else if (bx == px) 0 else 1
          data(valueFromMode(1, pointerDigits(2))) = input
          pointer += 2
        case 4 =>
          output = data(valueFromMode(1, pointerDigits(2)))
          if (position.length < 2) position :+= output.toInt
          else {
            if (position(0) == -1 & position(1) == 0) println(s"Score : $output")
            else {
              val tile = output match {
                case 0 =>
                  (position(0), position(1)) -> 'e'
                case 1 =>
                  (position(0), position(1)) -> 'w'
                case 2 =>
                  (position(0), position(1)) -> 'b'
                case 3 =>
                  px = position(0)
                  (position(0), position(1)) -> 'p'
                case 4 =>
                  bx = position(0)
                  (position(0), position(1)) -> 'B'
              }
              tiles += tile
            }
            position = Array.emptyIntArray
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
    tiles.count(_._2 == 'b')
  }
}
