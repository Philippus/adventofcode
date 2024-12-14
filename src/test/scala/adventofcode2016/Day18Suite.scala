package adventofcode2016

import adventofcode2016.Day18.*
import munit.FunSuite

class Day18Suite extends FunSuite:
  test("determines next row"):
    assertEquals(nextRow("..^^."), ".^^^^")

  test("generates map"):
    generateMap(".^^.^.^^^^", 10).foreach(println)
    assert(true)

  test("counts safe tiles in map for the sample"):
    assertEquals(countSafeTiles(".^^.^.^^^^", 10), 38L)

  test("counts safe tiles in map for the input"):
    assertEquals(countSafeTiles(importLines(), 40), 1913L)

  test("counts safe tiles in map for the input part two"):
    assertEquals(countSafeTiles(importLines(), 400000), 1913L)
end Day18Suite
