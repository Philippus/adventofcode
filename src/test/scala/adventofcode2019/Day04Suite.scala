package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("validates password"):
    assert(validPassword("111111"))
    assert(!validPassword("223450"))
    assert(!validPassword("123789"))

  test("counts valid passwords within range"):
    assertEquals(checkRange(importLines()), 979)

  test("validates password within the larger group"):
    assert(validPassword("112233", checkLargerGroup = true))
    assert(!validPassword("123444", checkLargerGroup = true))
    assert(validPassword("111122", checkLargerGroup = true))

  test("counts valid passwords within range within the larger group"):
    assertEquals(checkRange(importLines(), checkLargerGroup = true), 635)
end Day04Suite
