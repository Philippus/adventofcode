package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day10:
  case class Point(x: Int, y: Int, char: Char)

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(trailheads: Seq[Point], width: Int, height: Int): String =
    val grid = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => "\n" + trailheads.find(a => a.x == 0 && a.y == y).get._3
      case (x, y) => trailheads.find(a => a.x == x && a.y == y).get._3
    .mkString

  def sumScoreOfTrailheads(points: Seq[Point]): Int =
    def loop(x: Int, y: Int): Set[(Int, Int)] =
      if points.find(t => t.x == x && t.y == y).get._3 == '9' then
        Set((x, y))
      else
        val currentHeight = points.find(t => t.x == x && t.y == y).get._3
        points.toSet.flatMap:
          case t if t.x == x && t.y == y - 1 && currentHeight + 1 == t.char =>
            loop(x, y - 1)
          case t if t.x == x && t.y == y + 1 && currentHeight + 1 == t.char =>
            loop(x, y + 1)
          case t if t.x == x - 1 && t.y == y && currentHeight + 1 == t.char =>
            loop(x - 1, y)
          case t if t.x == x + 1 && t.y == y && currentHeight + 1 == t.char =>
            loop(x + 1, y)
          case _                                                            =>
            Set((999, 999))

    points.map(t => if t.char == '0' then loop(t.x, t.y).size - 1 else 0).sum

  def sumScoreOfTrailheadsPt2(trailheads: Seq[Point]): Int =
    def loop(x: Int, y: Int): Int =
      if trailheads.find(t => t.x == x && t.y == y).get._3 == '9' then
        1
      else
        val currentHeight = trailheads.find(t => t.x == x && t.y == y).get._3
        trailheads.map:
          case t if t.x == x && t.y == y - 1 && currentHeight + 1 == t.char =>
            loop(x, y - 1)
          case t if t.x == x && t.y == y + 1 && currentHeight + 1 == t.char =>
            loop(x, y + 1)
          case t if t.x == x - 1 && t.y == y && currentHeight + 1 == t.char =>
            loop(x - 1, y)
          case t if t.x == x + 1 && t.y == y && currentHeight + 1 == t.char =>
            loop(x + 1, y)
          case _                                                            =>
            0
        .sum

    trailheads.map(t => if t.char == '0' then loop(t.x, t.y) else 0).sum

  def parse(line: String, y: Int): Seq[Point] =
    line.zipWithIndex.collect:
      case (char, x) => Point(x, y, char)

  def handleLines(lines: List[String]): Seq[Point] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day10
