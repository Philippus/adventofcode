package adventofcode2022

import adventofcode2022.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("calculates total sum of priorities"):
    val lines = importLines()
    assertEquals(lines.map(handleLine).sum, 7795)

  test("calculates total sum of priorities part two"):
    val lines = importLines()
    assertEquals(handleLinesPartTwo(lines), 2703)
end Day03Suite
