package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  @tailrec
  def addUpMultiplications(input: String, acc: Long): Long =
    input match {
      case "" => acc
      case s"mul(${a},${b})$rest" if a.forall(_.isDigit) && b.forall(_.isDigit) && a.length <= 3 && b.length <= 3 && a.length >= 0 && b.length >= 0 =>
        addUpMultiplications(rest, (a.toLong * b.toLong) + acc)
      case _ =>
        addUpMultiplications(input.tail, acc)
    }

  @tailrec
  def addUpEnabledMultiplications(line: String, acc: Long, enabled: Boolean): Long =
    line match {
      case "" => acc
      case s"do()$rest" => addUpEnabledMultiplications(rest, acc, true)
      case s"don't()$rest" => addUpEnabledMultiplications(rest, acc, false)
      case s"mul(${a},${b})$rest" if a.forall(_.isDigit) && b.forall(_.isDigit) && a.length <= 3 && b.length <= 3 && a.length >= 0 && b.length >= 0 && enabled =>
        addUpEnabledMultiplications(rest, (a.toLong * b.toLong) + acc, enabled)
      case s"mul(${a},${b})$rest" if a.forall(_.isDigit) && b.forall(_.isDigit) && a.length <= 3 && b.length <= 3 && a.length >= 0 && b.length >= 0 =>
        addUpEnabledMultiplications(rest, acc, enabled)
      case _ =>
        addUpEnabledMultiplications(line.tail, acc, enabled)
    }

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2024/day03input.txt")): source =>
      source.getLines().toSeq
end Day03
