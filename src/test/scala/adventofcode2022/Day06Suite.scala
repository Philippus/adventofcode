package adventofcode2022

import adventofcode2022.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("works for the example"):
    val s = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

    assertEquals(firstMarkerAt(s), 7)
    assertEquals(firstMessageAt(s), 19)

  test("works for the input file"):
    val s = importLines()

    assertEquals(firstMarkerAt(s), 1757)
    assertEquals(firstMessageAt(s), 2950)
end Day06Suite
