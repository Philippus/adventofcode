package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06:
  def nextState(state: Map[Int, Long]): Map[Int, Long] =
    val newFish  = state.getOrElse(0, 0L)
    val newState = state.map((k, v) => (k - 1, v)) + (8 -> newFish)
    newState.filter(k => k._1 >= 0 && k._1 != 6) + (6 -> (newState.getOrElse(-1, 0L) + newState.getOrElse(6, 0L)))

  def simulateLanternfish(lanternfish: Map[Int, Long], days: Int): Long =
    @tailrec
    def loop(lanternfish: Map[Int, Long], daysLeft: Int): Long =
      if daysLeft == 0 then
        lanternfish.values.sum
      else
        loop(nextState(lanternfish), daysLeft - 1)

    loop(lanternfish, days)

  def importLines(): Map[Int, Long] =
    Using.resource(Source.fromResource("2021/day06input.txt")): source =>
      source.getLines().toSeq.head.split(',').map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong))
end Day06
