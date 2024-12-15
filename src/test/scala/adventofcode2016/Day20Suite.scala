package adventofcode2016

import adventofcode2016.Day20.*
import munit.FunSuite

class Day20Suite extends FunSuite:
  test("finds lowest value ip that is not blocked for the example"):
    assertEquals(lowestValueIp(Seq("5-8", "0-2", "4-7")), 3L)

  test("finds lowest value ip that is not blocked for the input"):
    assertEquals(lowestValueIp(importLines()), 4793564L)
end Day20Suite
