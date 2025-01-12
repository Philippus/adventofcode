package adventofcode2017

import scala.io.Source
import scala.util.Using

import Day10.*

object Day14:
  def hashToBits(s: String): String =
    s.map(char =>
      val i = Integer.parseInt(char.toString, 16)
      val b = i.toBinaryString
      "0" * (4 - b.length) ++ b
    ).mkString

  def knotHash(str: String): String =
    hashToBits(Day10.processLengthsAsAscii(0.to(255), stringToLengthsAndSuffix(str)))

  def countUsedSquares(str: String): Int =
    0.to(127).map(i => knotHash(s"$str-$i")).map(c => c.count(_.==('1'))).sum

  def importLines(): String =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next()
end Day14
