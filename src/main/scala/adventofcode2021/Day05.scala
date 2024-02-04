package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:
  case class Line(start: (Int, Int), end: (Int, Int))

  def positions(line: Line): Seq[(Int, Int)] =
    for
      x <- if line.start._1 <= line.end._1 then line.start._1.to(line.end._1) else line.end._1.to(line.start._1)
      y <- if line.start._2 <= line.end._2 then line.start._2.to(line.end._2) else line.end._2.to(line.start._2)
      if line.start._1 == line.end._1 || line.start._2 == line.end._2
    yield (x, y)

  def diagonalPositions(line: Line): Seq[(Int, Int)] =
    if line.start._1 == line.end._1 || line.start._2 == line.end._2 then
      Seq.empty
    else
      var i                  = 0
      var (x, y)             = (line.start._1, line.start._2)
      var s: Seq[(Int, Int)] = Seq.empty
      while x != line.end._1 do
        x = if line.start._1 <= line.end._1 then line.start._1 + i else line.start._1 - i
        y = if line.start._2 <= line.end._2 then line.start._2 + i else line.start._2 - i
        s = s :+ (x, y)
        i += 1
      s

  def overlappingPoints(lines: Seq[Line]): Int =
    lines.flatMap(positions).groupBy(identity).count(_._2.length > 1)

  def overlappingPointsIncludingDiagonals(lines: Seq[Line]): Int =
    lines.flatMap(x => positions(x) ++ diagonalPositions(x)).groupBy(identity).count(_._2.length > 1)

  def handleLine(s: String): Line =
    s match
      case s"$startX,$startY -> $endX,$endY" =>
        Line((startX.toInt, startY.toInt), (endX.toInt, endY.toInt))

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day05input.txt")): source =>
      source.getLines().toSeq
end Day05
