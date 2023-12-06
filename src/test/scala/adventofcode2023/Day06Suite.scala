package adventofcode2023

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("works for a race"):
    assertEquals(waysToWinRace(Race(7, 9)), 4)

  test("run example races"):
    assertEquals(runRaces(Seq(Race(7, 9), Race(15, 40), Race(30, 200))), 288)

  test("run races for input file"):
    assertEquals(
      runRaces(Seq(
        Race(35, 212),
        Race(93, 2060),
        Race(73, 1201),
        Race(66, 1044)
      )),
      114400
    )

  test("run example race of part two"):
    assertEquals(runRaces(Seq(Race(71530, 940200))), 71503)

  test("run race for input file part two"):
    assertEquals(runRaces(Seq(Race(35937366, BigInt(212206012011044L)))), 21039729)
end Day06Suite
