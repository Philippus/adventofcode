package adventofcode2021

import scala.io.Source
import scala.util.Using

object Day07:
  def fuelCost(positions: Seq[Long]): Long =
    val fuelCosts =
      for
        i <- positions.min.to(positions.max)
      yield positions.map(pos => math.abs(pos - i)).sum
    fuelCosts.min

  def fuelCostPartTwo(positions: Seq[Long]): Long =
    val fuelCosts =
      for
        i <- positions.min.to(positions.max)
      yield positions.map(pos => 1.to(math.abs(pos.toInt - i.toInt)).sum).sum
    fuelCosts.min

  def importLines(): Seq[Long] =
    Using.resource(Source.fromResource("2021/day07input.txt")): source =>
      source.getLines().toSeq.head.split(',').map(_.toLong)
end Day07
