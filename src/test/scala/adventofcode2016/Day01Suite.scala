package adventofcode2016

import adventofcode2016.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("finds distance to Easter Bunny HQ"):
    val instructions = readInputfile().map(handlLine)
    val code         = followInstructions(instructions.head)
    assertEquals(code, 291)

  test("finds distance to Easter Bunny HQ part two example"):
    val instructions = Seq("R8, R4, R4, R8").map(handlLine)
    val code         = followInstructionsPartTwo(instructions.head)
    assertEquals(code, 291)

  test("finds distance to Easter Bunny HQ part two"):
    val instructions = readInputfile().map(handlLine)
    val code         = followInstructionsPartTwo(instructions.head)
    assertEquals(code, 159)
end Day01Suite
