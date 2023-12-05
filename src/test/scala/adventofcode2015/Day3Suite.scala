package adventofcode2015

import adventofcode2015.Day3.*
import munit.FunSuite

class Day3Suite extends FunSuite:
  test("determines the amount of visited houses"):
    assertEquals(determineVisitedHouses(">").size, 2)
    assertEquals(determineVisitedHouses("^>v<").size, 4)
    assertEquals(determineVisitedHouses("^v^v^v^v^v").size, 2)

  test("calculates the right amount of visited houses for the input file"):
    assertEquals(calculatedVisitedHousesForFile, 2565)

  test("determines the amount of visited houses with a robo santa in play"):
    assertEquals(determineVisitedHousesWithRoboSanta("^v").size, 3)
    assertEquals(determineVisitedHousesWithRoboSanta("^>v<").size, 3)
    assertEquals(determineVisitedHousesWithRoboSanta("^v^v^v^v^v").size, 11)

  test("calculates the amount of visited houses  with a robo santa in play"):
    assertEquals(calculatedVisitedHousesWithRoboSantaForFile, 2639)
end Day3Suite

