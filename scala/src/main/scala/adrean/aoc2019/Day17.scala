package adrean.aoc2019

object Day17 {
  def main(args: Array[String]): Unit = {
    val source = "day17.txt"
    val input=scala.io.Source.fromResource(source).getLines().next()
    runPart1(0,input)

    val programs  = "A,B,A,B,C,B,C,A,B,C"
    val A = "R,4,R,10,R,8,R,4"
    val B = "R,10,R,6,R,4"
    val C = "R,4,L,12,R,6,L,12"
    val inputs =Array(programs,A,B,C,"\n").mkString("\n").toCharArray.map(_.toInt)
    runPart2(inputs,input)
  }

  def runPart1(input: Int , raw_data: String): Long = {
    var data = raw_data.split(",").map(_.toLong)
    data ++= Array.fill(9000)(0L)
    var output:Long = 0L
    var pointer= 0
    var relative = 0
    val valueFromMode = (parameter:Int, mode:Int) => mode match {
      case 1 =>
        pointer + parameter
      case 0 =>
        data(pointer + parameter).toInt
      case 2 =>
        relative + data(pointer + parameter).toInt
    }
    var grid = Array[Array[Char]]()
    grid :+= Array[Char]()
    var line=0
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
          pointer += 2
        case 4 =>
          output = data(valueFromMode(1, pointerDigits(2)))
          if (output == 10) {
            line += 1
            grid :+= Array[Char]()
            print(output.toChar)
          } else {
            grid(line) :+= output.toChar
            print(output.toChar)
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
    var total = 0
    for (i <- 1 to grid.length - 4) {
      for (j <- 1 to grid(i).length - 2) {
        if (grid(i)(j) == '#') {
          var intersect = true
          for (dir <- Array((1,0),(-1,0),(0,-1),(0,1))) {
            if (grid(i + dir._1)(j + dir._2) != '#') intersect = false
          }
          if (intersect) {
            grid(i)(j) = 'O'
            total += i * j
          }
        }
        print(grid(i)(j))
      }
      print('\n')
    }
    println(s"Part 1 : $total")
    output
  }

  def runPart2(inputs:Array[Int] , raw_data: String): Long = {
    var data = raw_data.split(",").map(_.toLong)
    data(0) = 2
    data ++= Array.fill(9000)(0L)
    var output:Long = 0L
    var pointer= 0
    var relative = 0
    var inputIndex = 0
    val valueFromMode = (parameter:Int, mode:Int) => mode match {
      case 1 =>
        pointer + parameter
      case 0 =>
        data(pointer + parameter).toInt
      case 2 =>
        relative + data(pointer + parameter).toInt
    }
    var grid = Array[Array[Char]]()
    grid :+= Array[Char]()
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
          data(valueFromMode(1, pointerDigits(2))) = inputs(inputIndex)
          inputIndex = (inputIndex + 1) % inputs.length
          pointer += 2
        case 4 =>
          output = data(valueFromMode(1, pointerDigits(2)))
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
    println(s"Part 2 : output")
    output
  }
}
