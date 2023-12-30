package adventofcode2015

import adventofcode2015.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
  val packages: Seq[BigInt] = importLines()

  test("finds quantum entanglement of the first group of packages in the ideal configuration"):
    assertEquals(determineMinimumCombinations(packages), BigInt(10439961859L))

  test("finds quantum entanglement of the first group of packages in the ideal configuration with the trunk"):
    assertEquals(determineMinimumCombinationsForPartTwo(packages), BigInt(72050269L))
end Day24Suite
