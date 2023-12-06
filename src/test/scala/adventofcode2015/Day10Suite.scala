package adventofcode2015

import adventofcode2015.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("determines the correct next sequence"):
    assertEquals(nextSequence("1"), "11")
    assertEquals(nextSequence("11"), "21")
    assertEquals(nextSequence("21"), "1211")
    assertEquals(nextSequence("1211"), "111221")
    assertEquals(nextSequence("111221"), "312211")

  test("determines the correct value for the sample input"):
    assertEquals(nthSequence("1", 5), "312211")

  test("determines the correct value for the puzzle input"):
    assertEquals(nthSequence("1113222113", 40).length, 252594)

  test("determines the correct value for the puzzle input part two"):
    assertEquals(nthSequence("1113222113", 50).length, 3579328)
end Day10Suite
