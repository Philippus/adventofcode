package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12:
  def handleLine(line: String): Int =
    line match
      case s"$x: $y," => y.trim.toIntOption.getOrElse(0)
      case s"$x: $y" => y.trim.toIntOption.getOrElse(0)
      case s"$x," => x.trim.toIntOption.getOrElse(0)
      case s"$x" => x.trim.toIntOption.getOrElse(0)
  def sumNumbersInJsonFile: Int =
    Using.resource(Source.fromResource("2015/day12input.json")): source =>
      source.getLines().map(handleLine).sum
end Day12
