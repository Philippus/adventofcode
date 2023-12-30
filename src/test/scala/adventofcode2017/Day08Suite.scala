package adventofcode2017

import adventofcode2017.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("determines largest value in any register for the example"):
    val instructions = Seq(
      "b inc 5 if a > 1",
      "a inc 1 if b < 5",
      "c dec -10 if a >= 1",
      "c inc -20 if c == 10"
    )
    assertEquals(followInstructions(instructions, Map.empty, 0)._1, 1)

  test("determines largest value in any register for the input file"):
    val instructions = readInputFile()
    assertEquals(followInstructions(instructions, Map.empty, 0)._1, 5215)

  test("determines highest value held in any register for the example"):
    val instructions = Seq(
      "b inc 5 if a > 1",
      "a inc 1 if b < 5",
      "c dec -10 if a >= 1",
      "c inc -20 if c == 10"
    )
    assertEquals(followInstructions(instructions, Map.empty, 0)._2, 10)

  test("determines highest value held in any register for the input file"):
    val instructions = readInputFile()
    assertEquals(followInstructions(instructions, Map.empty, 0)._2, 6419)
end Day08Suite
