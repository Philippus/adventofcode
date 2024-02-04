package adventofcode2021

import adventofcode2021.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("determines least fuel spent for the example"):
    val positions = "16,1,2,0,4,2,7,1,2,14".split(',').map(_.toLong)
    assertEquals(fuelCost(positions), 37L)

  test("determines least fuel spent for the input"):
    val positions = importLines()
    assertEquals(fuelCost(positions), 331067L)

//  test("calcs costs for part two"):
//    assertEquals(calcCost(3L), 1L)

  test("determines least fuel spent for the example part two"):
    val positions = "16,1,2,0,4,2,7,1,2,14".split(',').map(_.toLong)
    assertEquals(fuelCostPartTwo(positions), 168L)

  test("determines least fuel spent for the input part two"):
    val positions = importLines()
    assertEquals(fuelCostPartTwo(positions), 92881128L)
end Day07Suite
