package adventofcode2023

import munit.FunSuite

import day1.Day1._

class Day1Suite extends FunSuite:
  test("finds correct calibration value for a line"):
    assertEquals(calibrationValue("1abc2"), 12)
    assertEquals(calibrationValue("pqr3stu8vwx"), 38)
    assertEquals(calibrationValue("a1b2c3d4e5f"), 15)
    assertEquals(calibrationValue("reb7uchet"), 77)

  test("finds correct calibration value for a a line in part two"):
    assertEquals(calibrationValuePartTwo("two1nine"), 29)
    assertEquals(calibrationValuePartTwo("eightwothree"), 83)
    assertEquals(calibrationValuePartTwo("abcone2threexyz"), 13)
    assertEquals(calibrationValuePartTwo("xtwone3four"), 24)
    assertEquals(calibrationValuePartTwo("4nineeightseven2"), 42)
    assertEquals(calibrationValuePartTwo("zoneight234"), 14)
    assertEquals(calibrationValuePartTwo("7pqrstsixteen"), 76)

  test("day one part one input file"):
    assertEquals(readCalibrationDocument(calibrationValue), 54605)

  test("day one part two input file"):
    assertEquals(readCalibrationDocument(calibrationValuePartTwo), 55429)
end Day1Suite
