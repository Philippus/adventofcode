package adventofcode2021

import adventofcode2021.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("determines flashes for the examples"):
    val input  = "11111\n19991\n19191\n19991\n11111".split('\n')
    assertEquals(flashes(handleLines(input), 1), 9)
    val input2 =
      "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
        .split(
          '\n'
        )
    assertEquals(flashes(handleLines(input2), 10), 204)
    assertEquals(flashes(handleLines(input2), 100), 1656)

  test("determines flashes for the input"):
    val input = importLines()
    assertEquals(flashes(handleLines(input), 100), 1647)

  test("determines step when all flash for the example"):
    val input2 =
      "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
        .split(
          '\n'
        )
    assertEquals(flashes(handleLines(input2), 100, true), 195)

  test("determines step when all flash for the input"):
    val input = importLines()
    assertEquals(flashes(handleLines(input), 200, true), 348)
end Day11Suite
