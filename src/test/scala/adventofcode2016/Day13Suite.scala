package adventofcode2016

import scala.io.Source
import scala.util.Using

import adventofcode2016.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("determines if coordinate is a wall"):
    assert(!isWall(Pos(0, 0), 10))
    assert(isWall(Pos(1, 0), 10))
    assert(!isWall(Pos(0, 1), 10))
    assert(!isWall(Pos(5, 3), 10))
    assert(isWall(Pos(9, 6), 10))

  test("finds fewst number of steps to cubicle for the sample"):
    assertEquals(fewestStepsToCubicle(Pos(1, 1), Pos(7, 4), 10), 11L)

  test("finds fewst number of steps to cubicle for the input"):
    assertEquals(fewestStepsToCubicle(Pos(1, 1), Pos(31, 39), importLines()), 92L)

  test("finds cubicles reachable within at most 50 steps"):
    assertEquals(reachableInAtMost50Steps(Pos(1, 1), Pos(31, 39), importLines()), 124L)
end Day13Suite
