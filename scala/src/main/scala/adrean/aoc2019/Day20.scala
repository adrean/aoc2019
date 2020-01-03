package adrean.aoc2019

import scala.collection.mutable

case class Portal(var inner:(Int,Int), var outer:(Int,Int))
case class Path(var level:Int, var coords:Array[((Int,Int),Int)])

object Day20 {
  def main(args: Array[String]): Unit = {
    val source = "day20.txt"
    val data = scala.io.Source.fromResource(source).getLines().toArray
    // scan that data
    var innerPos: (Int,Int) = null
    var innerSize: (Int,Int) = null
    var line = 0
    var grid: mutable.HashMap[(Int,Int), Char] = mutable.HashMap[(Int,Int), Char]()
    var portals: mutable.HashMap[String,Portal] = new mutable.HashMap[String,Portal]() {
      override def default(key:String) = Portal(null, null)
    }


    while (line < data.length) {
      for (i <- 0 until data(line).length) {
        if (data(line)(i) == '#') {
          grid += ((line,i) -> '#')
        } else if (data(line)(i) == '.') {
          grid += ((line,i) -> '.')
        } else if (innerPos == null && data(line)(i) == ' ' && line > 5 && i > 5 && i < data(line).length - 5) {
          innerPos = (line, i)
          var width = 0
          while (data(line)(i + width) !='#') {
            width += 1
          }
          var height = 0
          while (data(line + height)(i) !='#') {
            height += 1
          }
          innerSize = (height - 1, width - 1)
          println(innerPos)
          println(innerSize)

        } else if (data(line)(i) >= 'A' & data(line)(i) <= 'Z') {

          if (line == 0) { // outer up
            val portalName = Array(data(line)(i),data(line + 1)(i)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).outer = (line + 2, i)
          } else if (line == data.length - 2) { // outer down
            val portalName = Array(data(line)(i),data(line + 1)(i)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).outer = (line - 1, i)
          } else if (i == 0) { // outer left
            val portalName = Array(data(line)(i),data(line)(i + 1)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).outer = (line, i + 2)
          } else if (i == data(line).length - 2) { //outer right
            val portalName = Array(data(line)(i),data(line)(i + 1)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).outer = (line, i - 1)
          } else if (innerPos != null && line == innerPos._1) { // inner up
            val portalName = Array(data(line)(i),data(line + 1)(i)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).inner = (line - 1, i)
          } else if (innerPos != null && line == innerPos._1 +innerSize._1) { // inner down
            val portalName = Array(data(line - 1)(i),data(line)(i)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).inner = (line + 1, i)
          } else if (innerPos != null && innerPos._2 == i) { // inner left
            val portalName = Array(data(line)(i),data(line)(i + 1)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).inner = (line, i - 1)
          } else if (innerPos != null && i == innerPos._2 + innerSize._2) { // inner right
            val portalName = Array(data(line)(i - 1),data(line)(i)).mkString
            if (!portals.contains(portalName)) portals += (portalName -> Portal(null, null))
            portals(portalName).inner = (line, i + 1)
          }
        }
      }
      line +=1
    }
    // Get distances
    for (portal <- portals) {
      println(portal._1 + " : " + portal._2.inner + " - " + portal._2.outer)
    }
    var level = 0
    // Start from AA
    var paths:mutable.HashMap[Int,Path] = mutable.HashMap(0 ->Path(0,Array((portals("AA").outer,0))))
    var pathIndex = 1
    var stop:Boolean = false
    var seenLevels: Array[Int] = Array[Int]()
    while (!stop) {
      var pathsToRemove:Array[Int] = Array()
      for (pathKey <- paths.keys) {
        var path = paths(pathKey).coords
        val lastPosition  = path.last
        var possiblePos = Array[(((Int,Int),Int),Int)]()
        for (portal <- portals) {
          if (portal._1 != "AA" && path.length > 1 &&  lastPosition._1 == portal._2.inner && path(path.length - 2)._1 != portal._2.outer) {
            possiblePos :+= ((portal._2.outer, lastPosition._2 + 1), +1)
          } else if (portal._1 != "AA" &&  path.length > 1 && lastPosition._1 == portal._2.outer && path(path.length - 2)._1 != portal._2.inner) {
            if (path.last._2 > 0)
              possiblePos :+= ((portal._2.inner,lastPosition._2 - 1), -1)
          }
        }
        for (dir <- Array((1,0),(-1,0),(0,-1),(0,1))) {
          val next = ((lastPosition._1._1 + dir._1, lastPosition._1._2 + dir._2),lastPosition._2)
          if (next._1 == portals("ZZ").outer) {
            println(s" level : ${paths(pathKey).coords.last._2} - Length : ${path.length}")
            if (seenLevels.contains(paths(pathKey).level))
              pathsToRemove :+= pathKey
            else
              seenLevels :+= paths(pathKey).level
            if (paths(pathKey).level == 0) {
              stop = true
            }
          } else if (path.length == 1) {
            if (grid.getOrElse(next._1,' ') == '.') possiblePos :+= (next, 0)
          } else if (!path.contains(next)) {
            if (grid.getOrElse(next._1,' ') == '.') possiblePos :+= (next, 0)
          }
        }
        if (possiblePos.length == 0) {
          pathsToRemove :+= pathKey
        } else if (!pathsToRemove.contains(pathKey))  {
          paths(pathKey).coords :+= possiblePos(0)._1
          paths(pathKey).level += possiblePos(0)._2
          if (possiblePos.length > 1) {
            for (otherPos <- 1 until possiblePos.length) {
              var newPath:Path = Path(paths(pathKey).level, paths(pathKey).coords.dropRight(1))
              newPath.coords :+= possiblePos(otherPos)._1
              newPath.level += possiblePos(otherPos)._2
              paths += (pathIndex -> newPath)
              pathIndex += 1
            }
          }
        }
      }
      if (pathsToRemove.length > 0) {
        pathsToRemove.distinct.foreach(pathIndex => paths.remove(pathIndex))
      }
    }
  }
}
