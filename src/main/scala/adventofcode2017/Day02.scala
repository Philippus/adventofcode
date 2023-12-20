package adventofcode2017

import scala.io.Source
import scala.util.Using

object Day02:
  def rowsChecksum(rows: Seq[String]): Int =
    rows.map: row =>
      val values = row.split("\\s+").map(_.toInt)
      values.max - values.min
    .sum

  def evenlyDivisibleChecksum(rows: Seq[String]): Int =
    rows.map: row =>
      val values       = row.split("\\s+").map(_.toInt)
      val combinations = values.combinations(2)
      combinations.find(combination =>
        combination.head.max(combination.last) % combination.head.min(combination.last) == 0
      ).map(combination =>
        combination.head.max(combination.last) / combination.head.min(combination.last)
      ).get
    .sum

  def readInputFile: Seq[String] =
    Using.resource(Source.fromResource("2017/day02input.txt")):
      _.getLines().toSeq
end Day02
