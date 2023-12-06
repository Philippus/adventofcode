package adventofcode2015

import adventofcode2015.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("finds shortest distance for example"):
    assertEquals(
      shortestRoute(Set(
        Leg(Set("London", "Dublin"), 464),
        Leg(Set("London", "Belfast"), 518),
        Leg(Set("Dublin", "Belfast"), 141)
      )),
      605
    )

  test("finds shortest distance for input file"):
    assertEquals(calculateShortestRouteForFile, 251)

  test("finds longest distance for example"):
    assertEquals(
      longestRoute(Set(
        Leg(Set("London", "Dublin"), 464),
        Leg(Set("London", "Belfast"), 518),
        Leg(Set("Dublin", "Belfast"), 141)
      )),
      982
    )

  test("finds longest distance for input file"):
    assertEquals(calculateLongestRouteForFile, 898)
end Day09Suite
