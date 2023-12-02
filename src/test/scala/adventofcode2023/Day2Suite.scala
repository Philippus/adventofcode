package adventofcode2023

import munit.FunSuite

import day2.Day2._

class Day2Suite extends FunSuite:
  test("finds possible game for a line"):
    assertEquals(
      possibleGame("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", red = 12, green = 13, blue = 14),
      1
    )
    assertEquals(possibleGame("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", 12, 13, 14), 2)
    assertEquals(
      possibleGame("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", 12, 13, 14),
      0
    )
    assertEquals(
      possibleGame("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", 12, 13, 14),
      0
    )
    assertEquals(possibleGame("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", 12, 13, 14), 5)

  test("day two part one input file"):
    assertEquals(readInputDocument(possibleGame, 12, 13, 14), 2207)

  test("calculates correct power of a game"):
    assertEquals(
      powerGame("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", red = 12, green = 13, blue = 14),
      48
    )
    assertEquals(powerGame("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", 12, 13, 14), 12)
    assertEquals(
      powerGame("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", 12, 13, 14),
      1560
    )
    assertEquals(powerGame("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", 12, 13, 14), 630)
    assertEquals(powerGame("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", 12, 13, 14), 36)

  test("day two part two input file"):
    assertEquals(readInputDocument(powerGame, 0, 0, 0), 62241)
end Day2Suite
