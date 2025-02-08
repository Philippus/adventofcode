package adventofcode2018

import adventofcode2018.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("calculates risk level for the sample"):
    assertEquals(riskLevel(510, Pos(10, 10)), 114L)

  test("calculates risk level for the input"):
    val (depth, target) = importLines()
    assertEquals(riskLevel(depth, target), 10204L)
end Day22Suite
