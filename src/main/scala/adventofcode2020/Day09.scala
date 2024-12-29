package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  def findFirstWrongNumber(numbers: List[Long], preamble: Int): Long =
    numbers.sliding(preamble + 1).collectFirst:
      case window
          if !window.take(preamble).combinations(2).toSeq
            .map:
              case Seq(a, b) => a + b
            .contains(window.last) => window.last
    .get

  def importLines(): List[Long] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList.map(_.toLong)
end Day09
