package adrean.aoc2019

import scala.collection.mutable

case class Material(var level:Int, var produce:Int, var need:Long, val children:Map[String,Int])

object Day14 {
  def main(args: Array[String]): Unit = {
    val sourceName = "day14.txt"
    val raw_data = scala.io.Source.fromResource(sourceName).getLines().toArray
    var materialMap: mutable.HashMap[String, Material] = mutable.HashMap[String, Material]()
    //build map
    for (reaction <- raw_data) {
      val out = reaction.split(" => ").last
      val in = reaction.split(" => ").head
      var material = Material(
        level = 0,
        produce = out.split(" ").head.toInt,
        need = 0L,
        children = in.split(", ").map(x => x.split(" ").last -> x.split(" ").head.toInt).toMap
      )
      materialMap += (out.split(" ").last -> material)
    }
    materialMap += ("ORE" -> Material(level = 0, produce = 0, need = 0, children = Map()))
    // update levels
    materialMap("FUEL").need = 1L
    var parents = Array(materialMap("FUEL"))
    var children:Array[String] = parents.flatMap(_.children.keys).distinct
    var distance = 1
    while (children.length > 1 | children.head != "ORE") {
      for (parent <- parents) {
        for (childString <- parent.children.keys) {
          var child  = materialMap(childString)
          child.level = distance
        }
      }
      distance += 1
      val tmp = children.flatMap(child => materialMap(child).children.keys).distinct
      parents = children.map(child => materialMap(child))
      children = tmp
    }
    //browse map
    for (distance <- 0 to materialMap.maxBy(_._2.level)._2.level) {
      val materials = materialMap.filter(_._2.level == distance)
      for (material <- materials) {
        if (material._1 != "ORE") {
          for (child <- material._2.children) {
            val multiplier = (material._2.need.toFloat / material._2.produce).ceil.toLong
            materialMap(child._1).need += child._2 * multiplier
          }
        }
      }
    }
    val oreForFuel = materialMap("ORE").need
    println(oreForFuel)

    var startingPoint = 1000000000000L / oreForFuel
    var neededFuel:Long = 0
    while (neededFuel < 1000000000000L) {
      //update map
      for (material <- materialMap) material._2.need = 0
      materialMap("FUEL").need = startingPoint
      //browse map
      for (distance <- 0 to materialMap.maxBy(_._2.level)._2.level) {
        val materials = materialMap.filter(_._2.level == distance)
        for (material <- materials) {
          if (material._1 != "ORE") {
            for (child <- material._2.children) {
              val multiplier = (material._2.need.toFloat / material._2.produce).ceil.toLong
              materialMap(child._1).need += child._2 * multiplier
            }
          }
        }
      }

      println(s" $startingPoint : ${materialMap("ORE").need}")
      neededFuel = materialMap("ORE").need
      startingPoint += 1
    }
  }
}
