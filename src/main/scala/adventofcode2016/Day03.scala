package adventofcode2016

import scala.io.Source
import scala.util.Using

object Day03:
  def isValidTriangle(ints: Array[Int]): Boolean =
    (ints(0) + ints(1) > ints(2)) &&
      (ints(1) + ints(2) > ints(0)) &&
      (ints(0) + ints(2) > ints(1))

  def validTrianglesInGroup(group: Seq[Array[Int]]): Seq[Boolean] =
    Seq(
      isValidTriangle(Array(group(0)(0), group(1)(0), group(2)(0))),
      isValidTriangle(Array(group(0)(1), group(1)(1), group(2)(1))),
      isValidTriangle(Array(group(0)(2), group(1)(2), group(2)(2)))
    )

  def handleLine(line: String): Array[Int] =
    line.trim.split("\\s+").map(_.trim).map(_.toInt)

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day03input.txt")): source =>
      source.getLines().toSeq
end Day03
