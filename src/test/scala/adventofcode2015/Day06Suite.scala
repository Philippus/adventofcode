package adventofcode2015

import adventofcode2015.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("can turn on lights"):
    manipulateGrid("turn on 0,0 through 999,999")
    assertEquals(grid.flatten.count(_.==(true)), 1000000)

  test("can turn off lights"):
    manipulateGrid("turn off 499,499 through 500,500")
    assertEquals(grid(499)(499), false)

  test("can toggle lights"):
    manipulateGrid("toggle 0,0 through 999,0")
    assertEquals(grid(66)(0), false)

  test("calculates how many lights are lit after running the commands"):
    manipulateGrid("turn off 0,0 through 999,999") // reset grid
    assertEquals(calculateLitLights, 377891)

  test("can increase brightness"):
    val brightnessBefore = dimmableGrid.flatten.sum
    manipulateBrightness("turn on 0,0 through 0,0")
    assertEquals(dimmableGrid.flatten.sum, brightnessBefore + 1)

  test("can decrease brightness"):
    resetDimmableGrid()
    manipulateBrightness("turn on 0,0 through 1,1")
    manipulateBrightness("turn off 0,0 through 0,0")
    assert(dimmableGrid.flatten.sum == 3)

  test("can decrease brightness, but not below 0"):
    resetDimmableGrid()
    manipulateBrightness("turn off 0,0 through 0,0")
    assert(dimmableGrid.flatten.sum == 0)

  test("can increase brightness by 2"):
    val brightnessBefore = dimmableGrid.flatten.sum
    manipulateBrightness("toggle 0,0 through 999,999")
    assertEquals(dimmableGrid.flatten.sum, brightnessBefore + 2000000)

  test("calculates brightness after running the commands"):
    resetDimmableGrid()
    assertEquals(calculateBrightness, 14110788)
end Day06Suite
