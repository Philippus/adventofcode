package adventofcode2017

import adventofcode2017.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("calculates steps needed to escape the maze for the example"):
    assertEquals(followInstructionsUsingArray(Seq(0, 3, 0, 1, -3)), 5)

  test("calculates steps needed to escape the maze for the input file"):
    assertEquals(followInstructionsUsingArray(readInputFile), 396086)

  test("calculates steps needed to escape the maze for the example in part two"):
    assertEquals(followInstructionsUsingArray(Seq(0, 3, 0, 1, -3), true), 10)

  test("calculates steps needed to escape the maze for the input file in part two"):
    assertEquals(followInstructionsUsingArray(readInputFile, true), 28675390)
end Day05Suite
