package adventofcode2021

import adventofcode2021.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("finds sum of risk levels for the example"):
    val lines = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678".split('\n')
    val input = handleLines(lines)
    assertEquals(findSumOfRiskLevels(input), 15)

  test("finds sum of risk levels for the input"):
    val lines = importLines()
    val input = handleLines(lines)
    assertEquals(findSumOfRiskLevels(input), 468)

  test("finds three largest basins for the example"):
    val lines = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678".split('\n')
    val input = handleLines(lines)
    assertEquals(productOfThreeLargestBasins(input), 1134)

  test("finds three largest basins for the input"):
    val lines = importLines()
    val input = handleLines(lines)
    assertEquals(productOfThreeLargestBasins(input), 1280496)
end Day09Suite
