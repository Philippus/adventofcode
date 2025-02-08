package adventofcode2018

import scala.concurrent.duration.Duration

import adventofcode2018.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  override val munitTimeout = Duration(1, "m")
  test("calculates risk level for the sample"):
    assertEquals(riskLevel(510, Pos(10, 10)), 114L)

  test("calculates risk level for the input"):
    val (depth, target) = importLines()
    assertEquals(riskLevel(depth, target), 10204L)

  test("calculates minutes to target for the sample"):
    assertEquals(fewestMinutesToTarget(510, Pos(10, 10)), 45L)

  test("calculates minutes to target for the input"):
    val (depth, target) = importLines()
    assertEquals(fewestMinutesToTarget(depth, target), 1004L)
end Day22Suite
