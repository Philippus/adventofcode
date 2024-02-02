package adventofcode2021

import adventofcode2021.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("determines measurements larger than the previous measurement for the example"):
    val lines = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    assertEquals(measurementsLargerThanPrevious(lines), 7)
  test("determines measurements larger than the previous measurement for the input"):
    val lines = importLines()
    assertEquals(measurementsLargerThanPrevious(lines), 1121)
  test("determines sums larger than the previous sum for the example"):
    val lines = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    assertEquals(measurementsLargerThanPreviousSums(lines), 5)
  test("determines sums larger than the previous sum for the input"):
    val lines = importLines()
    assertEquals(measurementsLargerThanPreviousSums(lines), 1065)
end Day01Suite
