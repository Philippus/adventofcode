package adventofcode2023

import munit.ScalaCheckSuite
import munit.Clue.generate

import day1.Day1._

class Day1Suite extends ScalaCheckSuite {
  property("finds correct calibration value for a line") {
    assertEquals(calibrationValue("1abc2"), 12)
    assertEquals(calibrationValue("pqr3stu8vwx"), 38)
    assertEquals(calibrationValue("a1b2c3d4e5f"), 15)
    assertEquals(calibrationValue("reb7uchet"), 77)
  }

  property("finds correct calibration value for a a line in part two") {
    assertEquals(calibrationValuePartTwo("two1nine"), 29)
    assertEquals(calibrationValuePartTwo("eightwothree"), 83)
    assertEquals(calibrationValuePartTwo("abcone2threexyz"), 13)
    assertEquals(calibrationValuePartTwo("xtwone3four"), 24)
    assertEquals(calibrationValuePartTwo("4nineeightseven2"), 42)
    assertEquals(calibrationValuePartTwo("zoneight234"), 14)
    assertEquals(calibrationValuePartTwo("7pqrstsixteen"), 76)
  }

  property("day one part one input file") {
    assertEquals(readCalibrationDocument(calibrationValue), 54605)
  }

  property("day one part two input file") {
    assertEquals(readCalibrationDocument(calibrationValuePartTwo), 55429)
  }
}
