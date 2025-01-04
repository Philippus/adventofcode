package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("calculates power level of fuel cell for the examples"):
    assertEquals(powerLevelOfFuelCell(3, 5, 8), 4)
    assertEquals(powerLevelOfFuelCell(122, 79, 57), -5)
    assertEquals(powerLevelOfFuelCell(217, 196, 39), 0)
    assertEquals(powerLevelOfFuelCell(101, 153, 71), 4)

  test("finds the 3x3 grid with the largest total power for the examples"):
    assertEquals(gridWithLargestPower(18)._1, (33, 45))
    assertEquals(gridWithLargestPower(18)._2, 29)
    assertEquals(gridWithLargestPower(42)._1, (21, 61))
    assertEquals(gridWithLargestPower(42)._2, 30)

  test("finds the 3x3 grid with the largest total power for the input"):
    assertEquals(gridWithLargestPower(importLines())._1, (21, 53))
    assertEquals(gridWithLargestPower(importLines())._2, 29)

  test("finds the square with the largest total power for the sample"):
    val largestPowerForSerialNumber18 = gridWithLargestPowerWithSize(18)
    assertEquals(largestPowerForSerialNumber18._1, (90, 269, 16))
    assertEquals(largestPowerForSerialNumber18._2, 113)

    val largestPowerForSerialNumber42 = gridWithLargestPowerWithSize(42)
    assertEquals(largestPowerForSerialNumber42._1, (232, 251, 12))
    assertEquals(largestPowerForSerialNumber42._2, 119)

  test("finds the square with the largest total power for the input"):
    val largestPowerForSerialNumberOfInput = gridWithLargestPowerWithSize(importLines())
    assertEquals(largestPowerForSerialNumberOfInput._1, (233, 250, 12))
    assertEquals(largestPowerForSerialNumberOfInput._2, 121)

  def importSampleLines(): String =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().next()
end Day11Suite
