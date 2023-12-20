package adventofcode2022

import scala.io.Source
import scala.util.Using

object Day03:
  def charToPriority(c: Char): Int =
    val i = c.toInt
    if i > 96 then i - 96
    else i - 38

  def handleLine(s: String): Int =
    val splits = s.splitAt(s.length / 2)
    charToPriority(splits._1.intersect(splits._2).head)

  def handleLinesPartTwo(lines: Seq[String]): Int =
    lines.grouped(3).map:
      case Seq(a, b, c) => a.intersect(b).intersect(c)
    .map(s => charToPriority(s.head)).sum

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2022/day03input.txt")): source =>
      source.getLines().toSeq.filter(_.nonEmpty)
end Day03
