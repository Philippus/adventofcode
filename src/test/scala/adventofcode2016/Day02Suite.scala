package adventofcode2016

import adventofcode2016.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("finds code for input file"):
    val instructions = readInputfile()
    val code         = followInstructions(instructions)
    assertEquals(code, "56983")

  test("finds code for input file part two"):
    val instructions = readInputfile()
    val code         = followInstructionsPartTwo(instructions)
    assertEquals(code, "8B8B1")
end Day02Suite
