package adventofcode2016

import adventofcode2016.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("finds lowerst positive integer for 0, 1, 0, 1, ... repeating clock signal"):
    val instructions = readInputfile()
    assertEquals(findLowestPositiveInteger(instructions), 189)
end Day25Suite
