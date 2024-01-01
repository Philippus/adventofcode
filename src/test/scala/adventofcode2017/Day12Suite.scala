package adventofcode2017

import adventofcode2017.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("finds group for the example"):
    val map = Seq(
      "0 <-> 2",
      "1 <-> 1",
      "2 <-> 0, 3, 4",
      "3 <-> 2, 4",
      "4 <-> 2, 3, 6",
      "5 <-> 6",
      "6 <-> 4, 5"
    ).map(handleLine).toMap
    assertEquals(findGroup(0, map).size, 6)

  test("finds group for the input file"):
    assertEquals(findGroup(0, readInputFile().map(handleLine).toMap).size, 169)

  test("counts groups for the example"):
    val map = Seq(
      "0 <-> 2",
      "1 <-> 1",
      "2 <-> 0, 3, 4",
      "3 <-> 2, 4",
      "4 <-> 2, 3, 6",
      "5 <-> 6",
      "6 <-> 4, 5"
    ).map(handleLine).toMap
    assertEquals(countGroups(0, map), 2)

  test("counts groups for the input file"):
    val map = readInputFile().map(handleLine).toMap
    assertEquals(countGroups(0, map), 179)
end Day12Suite
