package adventofcode2016

import adventofcode2016.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("determines viable pairs"):
    assertEquals(viablePairs(importLines().map(parseLine)), 985)
end Day22Suite
