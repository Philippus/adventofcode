package adventofcode2022

import scala.io.Source
import scala.util.Using

object Day04:
  case class Pair(start: Int, end: Int):
    def contains(other: Pair): Boolean =
      this == other ||
        (other.start > this.start && other.end < this.end) ||
        (other.start == this.start && other.end < this.end) ||
        (other.start > this.start && other.end == this.end)

  def handleLine(line: String): Boolean =
    line match
      case s"$s1-$e1,$s2-$e2" =>
        val pairOne = Pair(s1.toInt, e1.toInt)
        val pairTwo = Pair(s2.toInt, e2.toInt)
        pairOne.contains(pairTwo) || pairTwo.contains(pairOne)

  def overlapsBetween(pairOne: Pair, pairTwo: Pair): Boolean =
    Range.inclusive(pairOne.start, pairOne.end).intersect(Range.inclusive(pairTwo.start, pairTwo.end)).nonEmpty

  def handleLinePartTwo(line: String): Boolean =
    line match
      case s"$s1-$e1,$s2-$e2" =>
        val pairOne = Pair(s1.toInt, e1.toInt)
        val pairTwo = Pair(s2.toInt, e2.toInt)
        overlapsBetween(pairOne, pairTwo)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2022/day04input.txt")): source =>
      source.getLines().toSeq.filter(_.nonEmpty)
end Day04
