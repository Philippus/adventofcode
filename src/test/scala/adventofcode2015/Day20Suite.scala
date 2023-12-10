package adventofcode2015

import adventofcode2015.Day20.*
import munit.FunSuite

class Day20Suite extends FunSuite:
  test("calculates presents for a house"):
    assertEquals(presentsForHouse(1), 10)
    assertEquals(presentsForHouse(2), 30)
    assertEquals(presentsForHouse(9), 130)

// takes long
//  test("find first house for number of presents"):
//    assertEquals(findFirstHouseForNumberOfPresents(33100000), 776160)

// takes long as well
//  test("find first house for number of presents part two"):
//    assertEquals(findFirstHouseForNumberOfPresentsPartTwo(33100000), 776160)
end Day20Suite
