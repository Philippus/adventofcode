package adventofcode2017

import adventofcode2017.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("finds spiral"):
    assertEquals(findSpiral(24, Spiral(1, 1)), Spiral(25, 5))
    assertEquals(findSpiral(349281, Spiral(1, 1)), Spiral(349281, 591))

  test("finds manhattan distance from value to spiral"):
    assertEquals(manhattanDistance(347991, Spiral(349281, 591)), 480)
end Day03Suite
