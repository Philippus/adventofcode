package adventofcode2015

import adventofcode2015.Day17.*
import munit.FunSuite

class Day17Suite extends FunSuite:
  test("counts combinations for the example"):
    val containers = List(20, 15, 10, 5, 5)
    assertEquals(countCombinations(containers, 25), 4)

  test("counts combinations for the input file"):
    val containers = List(33, 14, 18, 20, 45, 35, 16, 35, 1, 13, 18, 13, 50, 44, 48, 6, 24, 41, 30, 42)
    assertEquals(countCombinations(containers, 150), 1304)

  test("finds minimum amount of containers for the example"):
    val containers = List(20, 15, 10, 5, 5)
    assertEquals(findMinimumAmountOfContainers(containers, 25), 2)

  test("counts combinations with container limit for the example"):
    val containers = List(20, 15, 10, 5, 5)
    assertEquals(countCombinationsWithContainerLimit(containers, 2, 25), 3)

  test("finds minimum amount of containers for the input file"):
    val containers = List(33, 14, 18, 20, 45, 35, 16, 35, 1, 13, 18, 13, 50, 44, 48, 6, 24, 41, 30, 42)
    assertEquals(findMinimumAmountOfContainers(containers, 150), 4)

  test("counts combinations with container limit for the example"):
    val containers = List(33, 14, 18, 20, 45, 35, 16, 35, 1, 13, 18, 13, 50, 44, 48, 6, 24, 41, 30, 42)
    assertEquals(countCombinationsWithContainerLimit(containers, 4, 150), 18)
end Day17Suite
