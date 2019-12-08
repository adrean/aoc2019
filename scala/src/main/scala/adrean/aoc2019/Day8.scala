package adrean.aoc2019

object Day8 {

  def main(args: Array[String]): Unit = {
    val data = scala.io.Source.fromResource("day8.txt").getLines().next()
    var layer = 0
    var minZeros = 150
    while (layer * 150 < data.length) {
      var layerData = data.slice(layer * 150, layer * 150 + 150)
      var countZeros = layerData.map(_.asDigit).filter(_ == 0).length
      var countOnes = layerData.map(_.asDigit).filter(_ == 1).length
      var countTwos = layerData.map(_.asDigit).filter(_ == 2).length
      if (countZeros < minZeros) {
        println(s"layer ${layer + 1} ${countZeros} zeros with value ${countOnes * countTwos}")
        minZeros = countZeros
      }
      layer += 1
    }

    var image = Array.fill[Int](150)(2)
    layer = 0
    while(layer * 150 < data.length) {
      var layerData = data.slice(layer * 150, layer * 150 + 150).map(_.asDigit)
      for (i <- 0 until 150) {
        if (layerData(i) < image(i) && image(i) == 2) image(i) = layerData(i)
      }
      layer += 1
    }
    for (j <- 0 until 6)
      println(image.slice(j * 25, j * 25 + 25).mkString(""))
  }
}
