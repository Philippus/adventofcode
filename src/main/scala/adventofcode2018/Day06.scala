package adventofcode2018

import scala.io.Source
import scala.util.Using

object Day06:
  case class Pos(x: Int, y: Int)

  def sizeOfAreaThatIsntInfinite(coordinates: Vector[Pos]): Int =
    val area                       = (for
      x           <- coordinates.minBy(_.x).x to coordinates.maxBy(_.x).x
      y           <- coordinates.minBy(_.y).y to coordinates.maxBy(_.y).y
      coordsToDist = coordinates.zipWithIndex.map: coordinate =>
                       (coordinate, math.abs(x - coordinate._1.x) + math.abs(y - coordinate._1.y))
      min          = coordsToDist.minBy(_._2)
      if coordsToDist.count(_._2 == min._2) == 1
    yield Pos(x, y) -> min._1._2).toVector
    val coordinatesClosestToBorder = area.filter: pos =>
      pos._1.x == coordinates.minBy(_.x).x ||
        pos._1.y == coordinates.minBy(_.y).y ||
        pos._1.x == coordinates.maxBy(_.x).x ||
        pos._1.y == coordinates.maxBy(_.y).y
    val finiteArea                 = area.filterNot: p =>
      coordinatesClosestToBorder.map(_._1).contains(p._1)
    finiteArea.groupBy(_._2).maxBy(_._2.length)._2.length

  def sizeOfRegion(coordinates: Vector[Pos], safeDistance: Int): Int =
    (for
      x <- coordinates.minBy(_.x).x to coordinates.maxBy(_.x).x
      y <- coordinates.minBy(_.y).y to coordinates.maxBy(_.y).y
      totalDistance = coordinates.map: coordinate =>
        math.abs(x - coordinate.x) + math.abs(y - coordinate.y)
      .sum
      if totalDistance < safeDistance
    yield
      Pos(x, y)).length

  def handleLines(lines: Seq[String]): Vector[Pos] =
    lines.map:
      case s"$x, $y" => Pos(x.toInt, y.toInt)
    .toVector

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day06
