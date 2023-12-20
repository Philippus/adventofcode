package adventofcode2022

import scala.io.Source
import scala.util.Using

object Day06:
  def allCharsUnique(s: String): Boolean =
    s.distinct == s

  def firstMarkerAt(s: String): Int =
    s.sliding(4, 1).map(allCharsUnique).indexWhere(x => x) + 4

  def firstMessageAt(s: String): Int =
    s.sliding(14, 1).map(allCharsUnique).indexWhere(x => x) + 14

  def importLines(): String =
    Using.resource(Source.fromResource("2022/day06input.txt")): source =>
      source.getLines().toSeq.head
end Day06
