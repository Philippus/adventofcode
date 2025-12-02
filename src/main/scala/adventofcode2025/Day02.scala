package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02:
  def isValid(id: String): Boolean =
    id.length % 2 == 1 || !id.endsWith(id.substring(0, id.length / 2))

  def isValidWithNewRules(id: String): Boolean =
    val validations =
      for
        i        <- 1.to(id.length / 2)
        substring = id.substring(0, i)
      yield id != substring.repeat(id.length / substring.length)
    !validations.contains(false)

  def countInvalidIds(lines: List[(String, String)], rule: String => Boolean): Long =
    val invalidIds: List[Long] =
      for
        line <- lines
        id   <- line.head.toLong.to(line.last.toLong)
        if !rule(id.toString)
      yield id
    invalidIds.sum

  def importLines(): List[(String, String)] =
    Using.resource(Source.fromResource("2025/day02input.txt")): source =>
      source.getLines().toList.head.split(',').toList.map:
        case s"$a-$b" => (a, b)
end Day02
