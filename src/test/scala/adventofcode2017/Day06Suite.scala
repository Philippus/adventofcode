package adventofcode2017

import adventofcode2017.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("calculates redistribution cycles"):
    assertEquals(redistributeBlocks(Seq(0, 2, 7, 0), Seq.empty, 0), 5)

  test("calculates redistribution cycles for the input file"):
    assertEquals(redistributeBlocks(readInputFile, Seq.empty, 0), 11137)

  test("calculates cycles of loop in part two"):
    assertEquals(redistributeBlocks(Seq(0, 2, 7, 0), Seq.empty, 0, true), 4)

  test("calculates cycles of loop for the input file"):
    assertEquals(redistributeBlocks(readInputFile, Seq.empty, 0, true), 1037)
end Day06Suite
