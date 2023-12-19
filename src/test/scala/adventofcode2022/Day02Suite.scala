package adventofcode2022

import adventofcode2022.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("calculates total score for input file"):
    assertEquals(calculateSumForImportFile(), 10994)

  test("calculates total score for input file part two"):
    assertEquals(calculateSumForImportFilePartTwo(), 12526)
end Day02Suite
