package adventofcode2016

import adventofcode2016.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("finds error-corrected message"):
    assertEquals(handleMessages(readInputfile()), "gebzfnbt")

  test("finds error-corrected message part two"):
    assertEquals(handleMessagesPartTwo(readInputfile()), "fykjtwyn")
end Day06Suite
