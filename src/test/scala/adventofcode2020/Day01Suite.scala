package adventofcode2020

import adventofcode2020.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("handles expense report"):
    assertEquals(handleExpenseReport(Seq(1721, 979, 366, 299, 675, 1456)), 514579)

  test("handles expense report for input file"):
    val expenses = readInputFile()
    assertEquals(handleExpenseReport(expenses), 440979)

  test("handles expense report part two"):
    assertEquals(handleExpenseReportPartTwo(Seq(1721, 979, 366, 299, 675, 1456)), 241861950)

  test("handles expense report for input file part two"):
    val expenses = readInputFile()
    assertEquals(handleExpenseReportPartTwo(expenses), 82498112)
end Day01Suite
