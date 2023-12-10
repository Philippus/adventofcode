package adventofcode2015

import adventofcode2015.Day21.*
import munit.FunSuite

class Day21Suite extends FunSuite:
  test("determines the battle"):
    assertEquals(battle(Player(8, 5, 5), Boss(12, 7, 2), Player(8, 5, 5)), Player(2, 5, 5))

  test("determines the most expensive losing battle"):
    assertEquals(battle(Player(100, 7, 4), Boss(103, 9, 2), Player(100, 108, 4)), Boss(3, 9, 2))

  test("calculates minimum spend to win the battle"):
    assertEquals(calculateMinimumSpend(Boss(103, 9, 2)), 121)

  test("calculates maximum spend and still lose the battle"):
    assertEquals(calculateMaximumSpend(Boss(103, 9, 2)), 201)
end Day21Suite
