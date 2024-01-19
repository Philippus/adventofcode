package adventofcode2016

import munit.FunSuite

import Day03._
class Day03Suite extends FunSuite:
  test("finds possible triangles"):
    val candidates = readInputfile()
    val count      = candidates.map(handleLine).map(isValidTriangle).count(_.==(true))
    assertEquals(count, 993)

  test("finds possible triangles by column"):
    val candidates = readInputfile()
    val count      = candidates.map(handleLine).grouped(3).flatMap(validTrianglesInGroup).count(_.==(true))
    assertEquals(count, 1849)
end Day03Suite
