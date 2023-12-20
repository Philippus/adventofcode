package adventofcode2022

import adventofcode2022.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("finds fully contained pairs"):
    assert(Pair(2, 8).contains(Pair(3, 7)) || Pair(3, 7).contains(Pair(2, 8)))
    assert(Pair(4, 6).contains(Pair(6, 6)))
    val lines = importLines()
    assertEquals(lines.map(handleLine).count(x => x), 448)

  test("finds overlap between pairs"):
    assert(overlapsBetween(Pair(5, 7), Pair(7, 9)))
    val lines = importLines()
    assertEquals(lines.map(handleLinePartTwo).count(x => x), 794)
end Day04Suite
