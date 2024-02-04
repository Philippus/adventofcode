package adventofcode2021

import adventofcode2021.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("counts 1, 4, 7 and 8 for the example"):
    val value = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
    assertEquals(count1478(Seq(handleLine(value))), 2)

  test("counts 1, 4, 7 and 8 for the input"):
    val values = importLines().map(handleLine)
    assertEquals(count1478(values), 390)

  test("determines the output value"):
    val value              = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    val (patterns, output) = handleLinePartTwo(value)
    assertEquals(outputValue(patterns, output), 5353)

  test("determines output values"):
    val values = importLines().map(handleLinePartTwo)
    assertEquals(outputValues(values), 1011785)
end Day08Suite
