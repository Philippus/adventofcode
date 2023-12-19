package adventofcode2022

import munit.FunSuite

import Day01.*

class Day01Suite extends FunSuite:
  test("calculates Elf with most calories"):
    assertEquals(calculateElfWithMostCalories(), 70720)

  test("calculates sum of top three Elves"):
    assertEquals(calculateSumOfTopThreeElves(), 207148)
end Day01Suite
