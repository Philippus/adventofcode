package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  def maximumJoltageWith2Batteries(banks: List[String]): Int =
    banks.map: bank =>
      val left  = bank.init.max
      val right = bank.drop(bank.indexOf(left) + 1).max
      s"$left$right".toInt
    .sum

  def maximumJoltageWithNBatteries(banks: List[String], batteries: Int): Long =
    @tailrec
    def loop(bank: String, batteries: Int, acc: String): Long =
      if batteries == 0 then
        acc.toLong
      else
        val max = bank.dropRight(batteries - 1).max
        loop(bank.drop(bank.indexOf(max) + 1), batteries - 1, acc :+ max)

    banks.map: bank =>
      loop(bank, batteries, "")
    .sum

  def maximumJoltageWith12Batteries(banks: List[String]): Long =
    maximumJoltageWithNBatteries(banks, 12)

  def importLines(): List[String] =
    Using.resource(Source.fromResource("2025/day03input.txt")): source =>
      source.getLines().toList
end Day03
