package adrean.aoc2019

object Day6 {

  def main(args: Array[String]): Unit = {
    println("Part 1: " + runPart1())
    println("Part 2: " + runPart2())
  }

  def runPart1(): Int = {
    val data: Map[String, Array[String]] = scala.io.Source.fromResource("day6.txt").getLines.toArray.map(_.split("\\)")).groupBy(_ (0)).map(x => x._1 -> x._2.map(_ (1)))
    val values = data.values.flatten.toSet
    var origins: Array[String] = data.keys.toSet.diff(values).toArray
    var stepNodes: Array[String] = origins.clone()
    var step = 1
    var count = 0
    while (stepNodes.length > 0) {
      var newStepNodes: Array[String] = Array()
      for (node <- stepNodes) {
        if (data.contains(node)) {
          count += step * data(node).length
          newStepNodes ++= data(node)
        }
      }
      step += 1
      stepNodes = newStepNodes
    }
    count
  }

  def runPart2(): Int = {
    val data: Map[String, String] = scala.io.Source.fromResource("day6.txt").getLines.toArray.map(_.split("\\)")).map(x => x(1) -> x(0)).toMap
    var you = "YOU"
    var san = "SAN"
    var stepMap:Map[String, Int] = Map()
    var step = 0
    while (data.contains(san)) {
      stepMap += (data(san) -> step)
      step += 1
      san = data(san)
    }
    step = 0
    while (data.contains(you)) {
      if (stepMap.contains(data(you))) {
        return stepMap(data(you)) + step
      } else {
        stepMap += (data(you) -> step)
        you = data(you)
        step += 1
      }
    }
    step
  }

}
