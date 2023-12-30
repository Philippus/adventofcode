package adventofcode2015

import adventofcode2015.Day23.*
import munit.FunSuite

class Day23Suite extends FunSuite:
  test("reads instructions"):
    val instructions = Seq(
      "inc b",
      "jio b, +2",
      "tpl b",
      "inc b"
    )
    assertEquals(readInstructions(instructions, 0, 0, 0), 2)

  test("reads instructions from file"):
    val instructions = readInstrucionsFromFile()
    assertEquals(readInstructions(instructions, 0, 0, 0), 307)

  test("reads instructions from file part two"):
    val instructions = readInstrucionsFromFile()
    assertEquals(readInstructions(instructions, 0, 1, 0), 160)
end Day23Suite
