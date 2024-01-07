package adventofcode2017

import adventofcode2017.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  test("generate"):
    assertEquals(generate(65L, 16807L), 1092455L)
    assertEquals(generate(8921L, 48271L), 430625591L)

  test("compare"):
    assertEquals(compare(245556042L, 1431495498L), true)

  test("judge"):
    assertEquals(judge(65L, 16807L, 8921L, 48271L, 5L), 1L)
    assertEquals(judge(65L, 16807L, 8921L, 48271L, 40_000_000L), 588L)
    assertEquals(judge(883L, 16807L, 879L, 48271L, 40_000_000L), 609L)

  test("generate with multiple"):
    assertEquals(generateWithMultiple(65L, 16807L, 4L), 1352636452L)
    assertEquals(generateWithMultiple(8921L, 48271L, 8L), 1233683848L)

  test("judge with multiple"):
    assertEquals(judgeWithMultiple(65L, 16807L, 4L, 8921L, 48271L, 8L, 5_000_000L), 309L)
    assertEquals(judgeWithMultiple(883L, 16807L, 4L, 879L, 48271L, 8L, 5_000_000L), 609L)
end Day15Suite
