package adventofcode2018

import adventofcode2018.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("determines matching opcodes for the sample"):
    val sample = Sample(Vector(3, 2, 1, 1), Vector(9, 2, 1, 2), Vector(3, 2, 2, 1))
    assertEquals(testSample(sample).toSet, Set("mulr", "addi", "seti"))

  test("determines samples behaving like three or more opcodes for the input"):
    val samples = handleLines(importLines())._1
    assertEquals(samplesBehavingLikeThreeOrMoreOpcodes(samples), 560)

  test("determines opcodes and executes test program"):
    val (samples, instructions) = handleLines(importLines())
    assertEquals(determineOpcodesAndExecuteTestProgram(samples, instructions), 622)
end Day16Suite
