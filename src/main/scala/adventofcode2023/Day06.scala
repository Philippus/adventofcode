package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06:
  case class Race(time: Int, distance: BigInt)

  def waysToWinRace(race: Race): Int =
    @tailrec
    def recurse(c: BigInt, acc: Int): Int =
      if c == 0 then
        acc
      else if c * (race.time - c) > race.distance then
        recurse(c - 1, acc + 1)
      else
        recurse(c - 1, acc)
    recurse(race.time, 0)

  def runRaces(races: Seq[Race]): Int =
    races.map(waysToWinRace).product
end Day06
