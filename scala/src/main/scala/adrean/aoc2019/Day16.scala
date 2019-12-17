package adrean.aoc2019

import scala.math.abs

object Day16 {

  def main(args: Array[String]): Unit = {
    var input = "59719811742386712072322509550573967421647565332667367184388997335292349852954113343804787102604664096288440135472284308373326245877593956199225516071210882728614292871131765110416999817460140955856338830118060988497097324334962543389288979535054141495171461720836525090700092901849537843081841755954360811618153200442803197286399570023355821961989595705705045742262477597293974158696594795118783767300148414702347570064139665680516053143032825288231685962359393267461932384683218413483205671636464298057303588424278653449749781937014234119757220011471950196190313903906218080178644004164122665292870495547666700781057929319060171363468213087408071790"
    var test_input = "03036732577212944063491565474664"

    var data = input.map(_.asDigit).toArray
    val pattern = Array(0, 1, 0, -1)
    val size = data.length
    var newData = Array[Int]()
    for (_ <- 1 to 100) {
      for (i <- 1 to size) {
        val repeatingPattern = Array.fill(size / i + 1)(pattern.flatMap(p => Array.fill(i)(p))).flatten.drop(1)
        newData :+= abs(data.zip(repeatingPattern).map(x => x._1 * x._2).sum % 10)
      }
      data = newData.clone()
      newData = Array[Int]()
    }
    println(s"Part 1 : ${data.slice(0,8).mkString}")

    // Part 2
    val bigdata = input * 10000
    val offset = bigdata.slice(0,7).toInt
    data = bigdata.substring(offset).map(_.asDigit).toArray

    for (_ <- 1 to 100) {
      for (i <- (data.length -2) to 0 by -1) {
        data(i) = (data(i+1) + data(i)) % 10
      }
      newData = Array[Int]()
    }
    println(s"Part 2 : ${data.slice(0,8).mkString}")

  }
}
