package adrean.aoc2019

import scala.collection.mutable

object Day7 {

  def main(args: Array[String]): Unit = {

    val raw_data = "3,8,1001,8,10,8,105,1,0,0,21,38,63,76,93,118,199,280,361,442,99999,3,9,101,3,9,9,102,3,9,9,101,4,9,9,4,9,99,3,9,1002,9,2,9,101,5,9,9,1002,9,5,9,101,5,9,9,1002,9,4,9,4,9,99,3,9,101,2,9,9,102,3,9,9,4,9,99,3,9,101,2,9,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,102,4,9,9,1001,9,3,9,1002,9,5,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99".split(",").map(_.toInt)
    runProgram(raw_data = raw_data)

  }

  def runProgram(raw_data: Array[Int]): Unit = {

    var max= (0,Array[Int]())
    var inputs = Array.fill(5)(mutable.Queue[Int]())
    for (permutation <- (5 to 9).permutations) {
      // setup inputs
      for (i <- 0 to 4) inputs(i).enqueue(permutation(i))
      inputs(0).enqueue(0)

      val pointers = Array.fill(5)(0)

      var currentAmp = 0
      var ampData = Array.fill(5)(raw_data.clone())
      var data = ampData(currentAmp)
      var output = 0

      val valueFromMode = (parameter:Int, mode:Int) => mode match {
        case 1 => parameter
        case 0 => data(parameter)
      }

      while (ampData.last(pointers.last) != 99 | currentAmp != 4) {
        var pointerDigits = data(pointers(currentAmp)).toString.map(_.asDigit).reverse.toArray
        if (pointerDigits.length < 4) for (_ <- pointerDigits.length until 4) pointerDigits :+= 0
        pointerDigits(0) match {
          case 1 =>
            data(data(pointers(currentAmp) + 3)) = valueFromMode(data(pointers(currentAmp) + 1), pointerDigits(2)) + valueFromMode(data(pointers(currentAmp) + 2), pointerDigits(3))
            pointers(currentAmp) += 4
          case 2 =>
            data(data(pointers(currentAmp) + 3)) = valueFromMode(data(pointers(currentAmp) + 1), pointerDigits(2)) * valueFromMode(data(pointers(currentAmp) + 2), pointerDigits(3))
            pointers(currentAmp) += 4
          case 3 =>
            data(data(pointers(currentAmp) + 1)) = inputs(currentAmp).dequeue()
            pointers(currentAmp) += 2
          case 4 =>
            inputs((currentAmp + 1) % 5).enqueue(data(data(pointers(currentAmp) +1)))
            pointers(currentAmp) += 2
            currentAmp = (currentAmp + 1) % 5
            data = ampData(currentAmp)
          case 5 =>
            if (valueFromMode(data(pointers(currentAmp) + 1), pointerDigits(2)) != 0) pointers(currentAmp) = valueFromMode(data(pointers(currentAmp) + 2), pointerDigits(3))
            else pointers(currentAmp) += 3
          case 6 =>
            if (valueFromMode(data(pointers(currentAmp) + 1), pointerDigits(2)) == 0) pointers(currentAmp) = valueFromMode(data(pointers(currentAmp) + 2), pointerDigits(3))
            else pointers(currentAmp) += 3
          case 7 =>
            if (valueFromMode(data(pointers(currentAmp) + 1), pointerDigits(2)) < valueFromMode(data(pointers(currentAmp) + 2), pointerDigits(3))) {
              data(data(pointers(currentAmp) + 3)) = 1
            } else { data(data(pointers(currentAmp) + 3)) = 0}
            pointers(currentAmp) += 4
          case 8 =>
            if (valueFromMode(data(pointers(currentAmp) + 1), pointerDigits(2)) == valueFromMode(data(pointers(currentAmp) + 2), pointerDigits(3))) {
              data(data(pointers(currentAmp) + 3)) = 1
            } else { data(data(pointers(currentAmp) + 3)) = 0}
            pointers(currentAmp) += 4
          case 9 =>
            inputs((currentAmp + 1) % 5).enqueue(inputs(currentAmp).dequeue())
            currentAmp = (currentAmp + 1) % 5
            data = ampData(currentAmp)
        }
      }

      output = inputs.last.dequeue()
      if (output > max._1) {
        max = (output, permutation.toArray)
      }
    }
    println(max._1)
    println(max._2.mkString(","))
  }


}
