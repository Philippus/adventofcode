package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  case class Point(x: Int, y: Int, vx: Int, vy: Int)

  def createGrid(minX: Int, maxX: Int, minY: Int, maxY: Int): Seq[(Int, Int)] =
    for
      y <- minY to maxY
      x <- minX to maxX
    yield (x, y)

  def drawGrid(points: Vector[Point]): String =
    val grid = createGrid(points.minBy(_.x).x, points.maxBy(_.x).x, points.minBy(_.y).y, points.maxBy(_.y).y)
    grid.map:
      case (x, y) => (if x == points.minBy(_.x).x then "\n" else "") :+
          (if points.exists(p => p.x == x && p.y == y) then '#' else '.')
    .mkString

  def cohesiveMessage(points: Vector[Point], maxXY: Int): (Vector[Point], Int) =
    @tailrec
    def loop(points: Vector[Point], round: Int): (Vector[Point], Int) =
      // let's figure out when the points are pretty close together
      val (minX, maxX, minY, maxY) =
        (points.minBy(_.x).x, points.maxBy(_.x).x, points.minBy(_.y).y, points.maxBy(_.y).y)
      if math.abs(maxX - minX) <= maxXY && math.abs(maxY - minY) <= maxXY then
        (points, round)
      else
        val newPoints = points.map: point =>
          Point(point.x + point.vx, point.y + point.vy, point.vx, point.vy)
        loop(newPoints, round + 1)

    loop(points, 0)

  def handleLines(lines: List[String]): Vector[Point] =
    lines.map:
      case s"position=<$x,$y> velocity=<$vx,$vy>" =>
        Point(x.trim.toInt, y.trim.toInt, vx.trim.toInt, vy.trim.toInt)
    .toVector

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day10
