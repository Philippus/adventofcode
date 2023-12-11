package adventofcode2015

import adventofcode2015.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("sums the numbers for json file"):
    assertEquals(sumNumbersInJsonFile, 156366)
end Day12Suite
