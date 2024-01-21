package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day01:
  def handleExpenseReport(expenses: Seq[Int]): Int =
    (for
      expense1 <- expenses
      expense2 <- expenses
      if expense1 + expense2 == 2020
    yield expense1 * expense2).head

  def handleExpenseReportPartTwo(expenses: Seq[Int]): Int =
    (for
      expense1 <- expenses
      expense2 <- expenses
      expense3 <- expenses
      if expense1 + expense2 + expense3 == 2020
    yield expense1 * expense2 * expense3).head

  def readInputFile(): Seq[Int] =
    Using.resource(Source.fromResource("2020/day01input.txt")):
      _.getLines().toSeq.map(_.toInt)
end Day01
