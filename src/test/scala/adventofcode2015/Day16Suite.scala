package adventofcode2015

import adventofcode2015.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("find sue for input file"):
    assertEquals(findSueForInputFile, 213)

  test("find sue for input file part two"):
    assertEquals(findSueForInputFilePartTwo, 323)
end Day16Suite
