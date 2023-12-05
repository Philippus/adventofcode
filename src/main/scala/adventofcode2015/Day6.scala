package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day6:
  var grid: Array[Array[Boolean]] = Array.fill(1000)(Array.fill(1000)(false))

  def getCoordinates(startx: String, endx: String, starty: String, endy: String): Seq[(Int, Int)] =
    for
      x <- startx.toInt.to(endx.toInt)
      y <- starty.toInt.to(endy.toInt)
    yield (x, y)

  def manipulateGrid(command: String): Unit =
    command match
      case s"turn on $startx,$starty through $endx,$endy"  =>
        getCoordinates(startx, endx, starty, endy)
          .foreach((x, y) => grid(x)(y) = true)
      case s"turn off $startx,$starty through $endx,$endy" =>
        getCoordinates(startx, endx, starty, endy)
          .foreach((x, y) => grid(x)(y) = false)
      case s"toggle $startx,$starty through $endx,$endy"   =>
        getCoordinates(startx, endx, starty, endy)
          .foreach((x, y) => grid(x)(y) = !grid(x)(y))

  def calculateLitLights: Int =
    Using.resource(Source.fromResource("2015/day6input.txt")):
      _.getLines().foreach(manipulateGrid)
    grid.flatten.count(_ == true)

  var dimmableGrid: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))

  def manipulateBrightness(command: String): Unit =
    command match
      case s"turn on $startx,$starty through $endx,$endy"  =>
        getCoordinates(startx, endx, starty, endy)
          .foreach((x, y) => dimmableGrid(x)(y) = dimmableGrid(x)(y) + 1)
      case s"turn off $startx,$starty through $endx,$endy" =>
        getCoordinates(startx, endx, starty, endy)
          .foreach((x, y) => dimmableGrid(x)(y) = math.max(dimmableGrid(x)(y) - 1, 0))
      case s"toggle $startx,$starty through $endx,$endy"   =>
        getCoordinates(startx, endx, starty, endy)
          .foreach((x, y) => dimmableGrid(x)(y) = dimmableGrid(x)(y) + 2)

  def resetDimmableGrid(): Unit =
    dimmableGrid = Array.fill(1000)(Array.fill(1000)(0))

  def calculateBrightness: Int =
    Using.resource(Source.fromResource("2015/day6input.txt")):
      _.getLines().foreach(manipulateBrightness)
    dimmableGrid.flatten.sum
end Day6
